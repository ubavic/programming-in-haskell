{-# LANGUAGE FlexibleInstances, OverloadedStrings, NoImplicitPrelude #-}

module EpubRenderer (renderEpubBook, mathErrors) where

import Document
import Renderer ( highlight )

import Control.Monad ( forM_, unless )
import Data.List ( nub )
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text ( Text )
import Prelude hiding ( head, div, span )

import Text.Blaze.Html.Renderer.Text ( renderHtml )
import Text.Blaze.XHtml5 hiding ( map )
import qualified Text.Blaze.XHtml5.Attributes as A
import Text.TeXMath ( DisplayType(..), readTeX, writeMathML )
import Text.XML.Light ( Attr(..), add_attr, showElement, unqual )


class EpubRenderer a where
    renderEpub :: a -> Html


instance EpubRenderer PElement where
    renderEpub (PText str) = text str
    renderEpub (PCode str) = code $ highlight str
    renderEpub (PCCode str sign) = span ! A.class_ "centerCode" $ code (highlight str) >> text sign
    renderEpub (PPre str) = code $ text str
    renderEpub (PMath str) = math DisplayInline str
    renderEpub (DisplayMath eq _) = math DisplayBlock eq
    renderEpub (PDefinition str) = strong $ text str
    renderEpub (PEmphasis str) = em $ text str
    renderEpub (PNote _ i) =
        a ! epubType "noteref" ! A.id (noteRefId i) ! A.href ("#" <> noteId i) $ sup $ string $ show i
    renderEpub (PReference _) = "(1)"
    renderEpub (PUrl name url) = a ! A.href (textValue url) $ text name


instance EpubRenderer P where
    renderEpub = mapM_ renderEpub


instance EpubRenderer Block where
    renderEpub (Section name ref) = h2 ! A.id (stringValue $ "S" <> show ref) $ renderEpub name
    renderEpub (Subsection name) = h3 $ renderEpub name
    renderEpub (Paragraph ps) = paragraph mempty ps
    renderEpub (CodeBlock code desc) = withCaption desc $ pre $ highlight code
    renderEpub (Terminal code desc) = withCaption desc $ pre ! A.class_ "terminal" $ text code
    renderEpub (Figure path desc _) =
        figure $ do
            img ! A.src (textValue $ "../images/" <> path <> ".svg") ! A.alt (textValue $ maybe "" plainText desc)
            mapM_ (figcaption . renderEpub) desc
    renderEpub (Problem ps s ref) =
        div ! A.class_ "problem" ! A.id (stringValue $ "p" <> show ref) $ do
            paragraph (strong (string $ "Zadatak " <> show ref <> ".") >> " ") ps
            unless (null s) $ labelled (em "Rešenje" >> ".") s
    renderEpub (Example ps ref) =
        div ! A.class_ "example" ! A.id (stringValue $ "example" <> show ref) $
            labelled (strong $ string $ "Primer " <> show ref <> ".") ps
    renderEpub (List items) = ol $ mapM_ (li . renderEpub) items
    renderEpub (Todo _) = pure ()


instance EpubRenderer Chapter where
    renderEpub (Chapter name blocks) =
        section ! epubType "chapter" $ do
            h1 $ text name
            mapM_ renderEpub blocks


-- Readers that support popup footnotes hide these asides and show them when the noteref is followed.
paragraph :: Html -> P -> Html
paragraph prefix ps = do
    p $ prefix >> renderEpub ps
    forM_ [ (t, i) | PNote t i <- ps ] $ \(t, i) ->
        aside ! epubType "footnote" ! A.id (noteId i) $
            p $ (a ! A.href ("#" <> noteRefId i) $ string $ show i <> ".") >> " " >> renderEpub t

labelled :: Html -> [Block] -> Html
labelled prefix (Paragraph ps : rest) = paragraph (prefix >> " ") ps >> mapM_ renderEpub rest
labelled prefix blocks = p prefix >> mapM_ renderEpub blocks

withCaption :: Maybe P -> Html -> Html
withCaption Nothing content = content
withCaption (Just desc) content = figure $ content >> figcaption (renderEpub desc)

noteId :: Reference -> AttributeValue
noteId i = stringValue $ "note" <> show i

noteRefId :: Reference -> AttributeValue
noteRefId i = stringValue $ "noteref" <> show i

epubType :: AttributeValue -> Attribute
epubType = customAttribute "epub:type"

math :: DisplayType -> Text -> Html
math display tex =
    case readTeX tex of
        Right exps -> preEscapedString . showElement . add_attr alttext $ writeMathML display exps
        Left _ -> span ! A.class_ "math-error" $ text tex
    where alttext = Attr (unqual "alttext") (T.unpack tex)

mathSources :: Chapter -> [Text]
mathSources chapter = [ tex | e <- allElements chapter, tex <- source e ]
    where
        source (PMath tex) = [tex]
        source (DisplayMath tex _) = [tex]
        source _ = []

mathErrors :: Chapter -> [Text]
mathErrors chapter = [ tex | tex <- mathSources chapter, Left _ <- [readTeX tex] ]

plainText :: P -> Text
plainText = T.concat . fmap elementText
    where
        elementText (PText t) = t
        elementText (PCode t) = t
        elementText (PCCode t sign) = t <> sign
        elementText (PPre t) = t
        elementText (PMath t) = t
        elementText (DisplayMath t _) = t
        elementText (PDefinition t) = t
        elementText (PEmphasis t) = t
        elementText (PNote _ _) = ""
        elementText (PReference _) = "(1)"
        elementText (PUrl name _) = name


bookTitle :: Text
bookTitle = "Programiranje u Haskelu"

bookSubtitle :: Text
bookSubtitle = "Knjiga o funkcionalnom programiranju"

bookAuthor :: Text
bookAuthor = "Nikola Ubavić"

bookLanguage :: Text
bookLanguage = "sr-Latn"

xhtmlDocument :: AttributeValue -> Text -> Html -> Text
xhtmlDocument stylesheet pageTitle pageBody = LT.toStrict . renderHtml $ do
    preEscapedText "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    html ! customAttribute "xmlns" "http://www.w3.org/1999/xhtml"
        ! customAttribute "xmlns:epub" "http://www.idpf.org/2007/ops"
        ! customAttribute "xml:lang" (textValue bookLanguage)
        ! A.lang (textValue bookLanguage) $ do
            head $ do
                title $ text pageTitle
                link ! A.href stylesheet ! A.rel "stylesheet" ! A.type_ "text/css"
            pageBody

renderEpubChapter :: Chapter -> Text
renderEpubChapter chapter@(Chapter name _) =
    xhtmlDocument "../css/style.css" name $ body ! epubType "bodymatter" $ renderEpub chapter

renderTitlePage :: Text
renderTitlePage =
    xhtmlDocument "../css/style.css" bookTitle $
        body ! epubType "frontmatter" $ section ! epubType "titlepage" ! A.id "titlepage" $ do
            h1 $ text bookTitle
            p ! A.class_ "subtitle" $ text bookSubtitle
            p ! A.class_ "author" $ text bookAuthor

renderColophon :: String -> Text
renderColophon date =
    xhtmlDocument "../css/style.css" "Impresum" $
        body ! epubType "backmatter" $ section ! epubType "colophon" ! A.id "colophon" $ do
            p $ do
                "Knjiga je objavljena pod licencom "
                a ! A.href "https://creativecommons.org/licenses/by-nc-sa/4.0/deed.en" $ "CC BY-NC-SA 4.0"
                ". Slobodni ste da menjate, kopirate i distribuirate deo ili celokupnu knjigu u bilo kom formatu ili mediju, \
                \sve dok: 1) jasno naznačite autora knjige i sve izmene koje ste izvršili, 2) ne koristite sadržaj knjige \
                \u komercijalne svrhe, 3) materijal koji ste načinili izmenom ove knjige objavite pod istom licencom."
            p $ do
                "Sadržaj knjige se redovno unapređuje i dopunjuje. Poslednja verzija je dostupna na "
                a ! A.href "https://haskel.ubavic.rs" $ "haskel.ubavic.rs"
                "."
            p $ string $ "Generisano " <> date <> " godine."

renderToc :: [(String, Chapter)] -> Text
renderToc chapters =
    xhtmlDocument "css/style.css" "Sadržaj" $
        body ! epubType "frontmatter" $ do
            nav ! epubType "toc" ! A.id "toc" $ do
                h1 "Sadržaj"
                ol $ mapM_ entry chapters
            nav ! epubType "landmarks" ! A.id "landmarks" ! A.hidden "" $
                ol $ do
                    li $ a ! epubType "toc" ! A.href "toc.xhtml" $ "Sadržaj"
                    forM_ (take 1 chapters) $ \(stem, Chapter name _) ->
                        li $ a ! epubType "bodymatter" ! A.href (chapterHref stem "") $ text name
    where
        entry (stem, Chapter name blocks) = li $ do
            a ! A.href (chapterHref stem "") $ text name
            let sections = [ (sectionName, ref) | Section sectionName ref <- blocks ]
            unless (null sections) $ ol $ forM_ sections $ \(sectionName, ref) ->
                li $ a ! A.href (chapterHref stem $ "#S" <> show ref) $ text (plainText sectionName)
        chapterHref stem fragment = stringValue $ "text/" <> stem <> ".xhtml" <> fragment


data Page = Page
    { pageId :: Text
    , pageHref :: Text
    , pageProperties :: [Text]
    , pageContents :: Text
    }

renderPackage :: String -> [Page] -> [Text] -> Text
renderPackage modified pages figures = T.unlines $
    [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    , "<package xmlns=\"http://www.idpf.org/2007/opf\" version=\"3.0\" unique-identifier=\"uid\" xml:lang=\"" <> bookLanguage <> "\">"
    , "  <metadata xmlns:dc=\"http://purl.org/dc/elements/1.1/\">"
    , "    <dc:identifier id=\"uid\">https://haskel.ubavic.rs</dc:identifier>"
    , "    <dc:title>" <> bookTitle <> "</dc:title>"
    , "    <dc:creator>" <> bookAuthor <> "</dc:creator>"
    , "    <dc:language>" <> bookLanguage <> "</dc:language>"
    , "    <dc:description>" <> bookSubtitle <> "</dc:description>"
    , "    <dc:rights>CC BY-NC-SA 4.0</dc:rights>"
    , "    <meta property=\"dcterms:modified\">" <> T.pack modified <> "</meta>"
    , "  </metadata>"
    , "  <manifest>"
    ]
    ++ [ item (pageId page) (pageHref page) "application/xhtml+xml" (pageProperties page) | page <- pages ]
    ++ [ item "style" "css/style.css" "text/css" [] ]
    ++ [ item ("img-" <> f) ("images/" <> f <> ".svg") "image/svg+xml" [] | f <- figures ]
    ++
    [ "  </manifest>"
    , "  <spine>"
    ]
    ++ [ "    <itemref idref=\"" <> pageId page <> "\"/>" | page <- pages ]
    ++
    [ "  </spine>"
    , "</package>"
    ]
    where
        item itemId href mediaType properties =
            "    <item id=\"" <> itemId <> "\" href=\"" <> href <> "\" media-type=\"" <> mediaType <> "\""
            <> (if null properties then "" else " properties=\"" <> T.unwords properties <> "\"")
            <> "/>"

-- Output files are placed in a flat directory. build-epub.sh moves content.opf and toc.xhtml
-- to the epub/ directory of the container and all other XHTML files to epub/text/.
renderEpubBook :: String -> String -> [(String, Chapter)] -> [(FilePath, Text)]
renderEpubBook date modified chapters =
    ("content.opf", renderPackage modified pages figures)
    : [ (T.unpack (pageId page) <> ".xhtml", pageContents page) | page <- pages ]
    where
        pages =
            [ textPage "titlepage" [] renderTitlePage
            , Page "toc" "toc.xhtml" ["nav"] (renderToc chapters)
            ]
            ++ [ textPage (T.pack stem) ["mathml" | not . null $ mathSources chapter] (renderEpubChapter chapter)
               | (stem, chapter) <- chapters ]
            ++ [ textPage "colophon" [] (renderColophon date) ]
        textPage name = Page name ("text/" <> name <> ".xhtml")
        figures = nub [ path | (_, chapter) <- chapters, Figure path _ _ <- allBlocks chapter ]
