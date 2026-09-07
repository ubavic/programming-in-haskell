{-# LANGUAGE LambdaCase #-}
module Main where

import Parser ( parseDoc )
import Renderer ( HTMLRenderer(render), renderChapter, renderIndexPage )
import TypstRenderer ( renderTypstChapter, renderTypstBook )
import Document ( Chapter(..), Block(..), filterTodos, addReferences )

import Control.Exception ( try, SomeException )
import Data.Time.Clock ( getCurrentTime, UTCTime(utctDay) )
import Data.Time.Calendar ( toGregorian )
import System.Environment ( getArgs )
import System.Exit ( exitFailure )

import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Date = String
type Filename = String

errorS :: String -> String
errorS t = "\ESC[31m" ++ t ++ "\ESC[0m"

okS :: String -> String
okS t = "\ESC[32m" ++ t ++ "\ESC[0m"

warningS :: String -> String
warningS t = "\ESC[33m" ++ t ++ "\ESC[0m"

chapterName :: Chapter -> String
chapterName (Chapter name _) = T.unpack name

sections :: Chapter -> [Html]
sections (Chapter _ blocks) = 
    (\(Section s _) -> render s) <$> filter (\case (Section _ _) -> True; _ -> False) blocks

getDate :: IO Date
getDate = getCurrentTime >>=
    return . (\(y, m, d) -> concat[show d, ".", show m, ".", show y]) . toGregorian . utctDay

buildChapterHTML :: Maybe (Filename, String) -> Maybe (Filename, String) -> Date -> Chapter -> String
buildChapterHTML prev next date chapter =
    renderHtml $ renderChapter prev next date $ addReferences chapter

parseChapter :: Filename -> Filename -> IO (Maybe (Filename, Chapter))
parseChapter path filename = do
    cs <- try (TIO.readFile (path ++ "/src/" ++ filename ++ ".atex")) :: IO (Either SomeException Text)
    case cs of
        Left err -> do
            (putStrLn . errorS . concat) ["Can't load file ", filename, ".atex"]
            return Nothing
        Right contents ->
            case parseDoc contents of
                Left err -> do
                    (putStrLn . errorS . concat) ["Error parsing document ", filename, ".atex"]
                    putStrLn err
                    return Nothing
                Right chapter ->
                    return (Just (filename, chapter))

saveChapter :: Filename -> Date -> (Filename, Chapter, Maybe (Filename, String), Maybe (Filename, String)) -> IO ()
saveChapter path date (filename, chapter, prev, next) =
        writeFile (path ++ "/" ++ filename) (buildChapterHTML prev next date chapter) >>
        putStr (warningS . T.unpack . T.unlines . filterTodos $ chapter) >>
        (putStrLn . okS . concat) ["Saved document ", filename]

saveTypstChapter :: Filename -> (Filename, Chapter) -> IO ()
saveTypstChapter path (filename, chapter) =
        TIO.writeFile (path ++ "/" ++ dest) (renderTypstChapter $ addReferences chapter) >>
        putStr (warningS . T.unpack . T.unlines . filterTodos $ chapter) >>
        (putStrLn . okS . concat) ["Saved document ", dest]
    where dest = filename ++ ".typ"

loadTypstFile :: Filename -> Filename -> IO (Maybe Text)
loadTypstFile bookPath name = do
    result <- try (TIO.readFile (bookPath ++ "/typst/" ++ name)) :: IO (Either SomeException Text)
    case result of
        Left _ -> do
            (putStrLn . errorS . concat) ["Can't load file typst/", name]
            return Nothing
        Right contents -> return (Just contents)

buildIndexPage :: [(Filename, String, [Html])] -> Date -> String
buildIndexPage chapters date = renderHtml $ renderIndexPage chapters date

generateSpine :: [(Filename, String)] -> [(Maybe (Filename, String), Maybe (Filename, String))]
generateSpine list = let m = Just <$> list in zip (Nothing : m) (tail m ++ [Nothing])

siteUrl :: String
siteUrl = "https://haskel.ubavic.rs"

sitemapUrl :: String -> String -> String
sitemapUrl loc priority =
    concat
        [ "  <url>\n"
        , "    <loc>", loc, "</loc>\n"
        , "    <priority>", priority, "</priority>\n"
        , "    <changefreq>monthly</changefreq>\n"
        , "  </url>\n"
        ]

buildSitemap :: [Filename] -> String
buildSitemap pages =
    concat
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        , "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n"
        , sitemapUrl (siteUrl ++ "/") "1.00"
        ]
        ++ concatMap (\page -> sitemapUrl (siteUrl ++ "/" ++ page) "0.80") pages
        ++ "</urlset>\n"

main :: IO ()
main = do
    args <- getArgs
    let typstMode = "--typst" `elem` args
        positional = filter (/= "--typst") args
    case positional of
        path : outPath : _ -> do
            index <- filter ((/=) '#' . head) . lines <$> readFile (path ++ "/index")
            results <- mapM (parseChapter path) index
            parsedChapters <- case sequence results of
                Nothing -> do
                    (putStrLn . errorS) "Compilation failed"
                    exitFailure
                Just chapters -> return chapters
            date <- getDate
            if typstMode
                then exportTypst path outPath parsedChapters
                else exportHtml outPath date parsedChapters
        _ -> putStrLn "Usage: haskellBook [--typst] BOOK_DIR OUT_DIR" >> exitFailure

exportHtml :: Filename -> Date -> [(Filename, Chapter)] -> IO ()
exportHtml outPath date parsedChapters = do
    let htmlChapters = (\(stem, chapter) -> (stem <> ".html", chapter)) <$> parsedChapters
        chapterNames = (\(a, b) -> (a, chapterName b)) <$> htmlChapters
        spine = generateSpine chapterNames
        renderedChapters = zipWith (\secs (file, name) -> (file, name, secs)) (sections . snd <$> htmlChapters) chapterNames
    mapM_ (saveChapter outPath date) $ zipWith (\(f, c) (p, n) -> (f, c, p, n)) htmlChapters spine
    writeFile (outPath ++ "/index.html") (buildIndexPage renderedChapters date)
    writeFile (outPath ++ "/sitemap.xml") (buildSitemap $ fst <$> htmlChapters) >>
        (putStrLn . okS) "Saved document sitemap.xml"

exportTypst :: Filename -> Filename -> [(Filename, Chapter)] -> IO ()
exportTypst bookPath outPath parsedChapters = do
    prologue <- loadTypstFile bookPath "prologue.typ"
    epilogue <- loadTypstFile bookPath "epilogue.typ"
    mapM_ (saveTypstChapter outPath) parsedChapters
    case (prologue, epilogue) of
        (Just p, Just e) ->
            TIO.writeFile (outPath ++ "/book.typ") (renderTypstBook p e $ map (addReferences . snd) parsedChapters) >>
            (putStrLn . okS) "Saved document book.typ"
        _ -> (putStrLn . errorS) "Can't assemble book.typ" >> exitFailure
