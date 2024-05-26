{-# LANGUAGE FlexibleInstances, OverloadedStrings, InstanceSigs, NoImplicitPrelude #-}

module Renderer where

import Document

import qualified GHC.SyntaxHighlighter as SH
import qualified Data.Text as T
import Data.Text ( Text )
import Prelude hiding (span, head, div)

import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A


class HTMLRenderer a where
    render :: a -> Html


instance HTMLRenderer PElement where
    render :: PElement -> Html
    render (PText str) = text str
    render (PCode str) = code $ highlight str
    render (PCCode str sign) = span ! A.class_ "centerCode" $ code (highlight str) >> text sign
    render (PPre str) = code $ text str
    render (PMath str) = text $ "\\(" <> str <> "\\)"
    render (DisplayMath eq tag) = text $ "\\[" <> eq <> "\\]"
    render (PDefinition str) = strong $ text str
    render (PEmphasis str) = em $ text str
    render (PNote _ i) = sup ! A.class_ "counter" $ string $ show i
    render (PReference _) = a $ string "(1)"
    render (PUrl name url) =  a ! A.href (textValue url) $ text name


instance HTMLRenderer P where
    render =  mapM_ render


instance HTMLRenderer Block where
    render (Section name ref) = h2 ! A.id (stringValue id) $ a ! A.href (stringValue $ '#' : id) $ render name
        where id = "S" <> show ref
    render (Subsection name) = h3 $ render name
    render (Paragraph ps) = 
        p (render ps) >> sequence_ notes
        where notes = [ f text index | (PNote text index) <- ps]
              f t index = let i = show index in aside ! A.id (stringValue $ "note" ++ i) $ (mark i >> render t)
              mark i = span ! A.class_ "ref" $ string (i ++ ". ")
    render (CodeBlock code Nothing) = pre $ highlight code
    render (CodeBlock code (Just desc)) =
        figure $ do
            pre $ highlight code
            figcaption $ render desc
    render (Terminal code Nothing) = pre ! A.class_ "terminal" $ text code
    render (Terminal code (Just desc)) =
        figure $ do
            pre ! A.class_ "terminal" $ text code
            figcaption $ render desc
    render (Figure path desc ref) =
        figure $ do
            img ! A.src (textValue $ "img/" <> path <> ".svg")
            figcaption cap
            where cap = case desc of
                    Nothing -> pure ()
                    Just s -> render s
    render (Problem ps s ref) =
        (if null s
            then p $ anchor >> render ps
            else details $ summary (anchor >> render ps) >> mapM_ render s
        ) ! A.class_ "problem" ! A.id (stringValue $ "p" <> show ref)
        where anchor = (a ! A.class_ "ref" ! A.href (stringValue $ "#p" <> show ref) $ string $ "Zadatak " <> show ref <> ".") >> " "
    render (Example ps ref) =
        div ! A.class_ "example" ! A.id (stringValue $ "example" <> show ref) $ anchor >> mapM_ render ps
        where anchor = (a ! A.class_ "ref" ! A.href (stringValue $ "#example" <> show ref) $ string $ "Primer " <> show ref <> ".") >> " "
    render (List items) = ol $ mapM_ (li . render) items
    render (Todo _) = pure ()


instance HTMLRenderer Chapter where
    render (Chapter name ss) = do
        h1 $ text name
        mapM_ render ss
        script ! A.src "index.js" $ ""


instance HTMLRenderer (SH.Token, T.Text) where
    render (SH.CharTok, t) = span ! A.class_ "sh-string" $ text t
    render (SH.StringTok, t) = span ! A.class_ "sh-string" $ text t
    render (SH.IntegerTok, t) = span ! A.class_ "sh-number" $ text t
    render (SH.RationalTok, t) = span ! A.class_ "sh-number" $ text t
    render (SH.KeywordTok, t) = span ! A.class_ "sh-keyword" $ text t
    render (SH.CommentTok, t) = span ! A.class_ "sh-comment" $ text t
    render (SH.ConstructorTok, t) = span ! A.class_ "sh-constructor" $ text t
    render (_, t) = text t


highlight :: Text -> Html
highlight s = maybe (text s) (mapM_ render) (SH.tokenizeHaskell s)


renderChapter :: Maybe (String, String) -> Maybe (String, String) -> String -> Chapter -> Html
renderChapter prevChapter nextChapter date chapter@(Chapter cTitle _) = pageHTML body cTitle date
    where
        body = do
            nav $ do
                navLink prevChapter "Prethodna lekcija"
                navLink (Just ("/", "Index")) "Povratak na početnu"
                navLink nextChapter "Naredna lekcija"
            main $ render chapter
        navLink Nothing _ = div ! A.class_ "empty" $ ""
        navLink (Just (path, name)) s = a ! A.href (stringValue path) $ string name


renderIndexPage :: [(String, String, [Html])] -> String -> Html
renderIndexPage chapters date = pageHTML body "Programiranje u Haskelu" date
    where
        body = do
            hgroup ! A.id "titleHeading" $ do
                h1 $ string "Programiranje u Haskelu"
                p $ string "Knjiga o funkcionalnom programiranju"
            main $ do
                ol ! A.id "index" $ mapM_ f chapters
        f (path, name, sections) = do
            li $ do
                a ! A.href (stringValue path) $ string name
                ol $ mapM_ (\(i, s) -> li $ a ! A.href (stringValue $ path ++ "#S" ++ show i) $ s) . zip [1..] $ sections


pageHTML :: Html -> Text -> String -> Html
pageHTML pageBody pageTitle date = docTypeHtml ! A.lang "sr" $ do
    head $ do
        title $ text pageTitle
        meta ! A.charset "UTF-8"
        meta ! A.name "author" ! A.content "Nikola Ubavić"
        meta ! A.name "description" ! A.content "Knjiga o programskom jeziku Haskel"
        meta ! A.name "viewport" ! A.content "width=device-width, user-scalable=yes"
        meta ! A.name "theme-color" ! A.content "#ffffff"
        meta ! customAttribute "property" "og:image" ! A.content "https://haskel.ubavic.rs/img/og.png"
        link ! A.rel "stylesheet" ! A.href "style.css"
        link ! A.rel "icon" ! A.type_ "image/png" ! A.sizes "192x192" ! A.href "/img/favicon-192x192.png"
        link ! A.rel "icon" ! A.type_ "image/png" ! A.sizes "32x32" ! A.href "/img/favicon-32x32.png"
        link ! A.rel "icon" ! A.type_ "image/png" ! A.sizes "16x16" ! A.href "/img/favicon-16x16.png"
        link ! A.rel "stylesheet" ! A.href "https://cdn.jsdelivr.net/npm/katex@0.16.3/dist/katex.min.css"
        preEscapedToHtml katexImports
    body $ do
        pageBody
        footer $ do
            div $ do
                a ! A.href "#" $ "Vrh stranice"
                " • "
                a ! A.href "/" $ "Početna"
                " • "
                a ! A.href "https://github.com/ubavic/programming-in-haskell" $ "GitHub"
            div $ do
                "Knjiga je objavljena pod licencom "
                a ! A.href "https://creativecommons.org/licenses/by-nc-sa/4.0/deed.en" $ "CC BY-NC-SA 4.0"
            div $ do
                "Autor "
                a ! A.class_ "version" ! A.href "https://ubavic.rs" $ "Nikola Ubavić"
            div $ string $ "Sadržaj knjige se redovno unapređuje i dopunjuje. Poslednja izmena " <> date <> " godine."


katexImports :: String
katexImports = "\
\<link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/katex@0.16.3/dist/katex.min.css' integrity='sha384-Juol1FqnotbkyZUT5Z7gUPjQ9gzlwCENvUZTpQBAPxtusdwFLRy382PSDx5UUJ4/' crossorigin='anonymous'>\
\<script defer src='https://cdn.jsdelivr.net/npm/katex@0.16.3/dist/katex.min.js' integrity='sha384-97gW6UIJxnlKemYavrqDHSX3SiygeOwIZhwyOKRfSaf0JWKRVj9hLASHgFTzT+0O' crossorigin='anonymous'></script>\
\<script defer src='https://cdn.jsdelivr.net/npm/katex@0.16.3/dist/contrib/auto-render.min.js' integrity='sha384-+VBxd3r6XgURycqtZ117nYw44OOcIax56Z4dCRWbxyPt0Koah1uHoK0o4+/RRE05' crossorigin='anonymous' onload='renderMathInElement(document.body);'></script>\
\<script src='https://beamanalytics.b-cdn.net/beam.min.js' data-token='c408e92a-b8b2-4f8c-b0ac-a62f282ee954' async></script>"
