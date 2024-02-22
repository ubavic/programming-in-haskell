{-# LANGUAGE LambdaCase #-}
module Main where

import Parser ( parseDoc )
import Renderer ( HTMLRenderer(render), renderChapter, renderIndexPage )
import Document ( Chapter(..), Block(..), filterTodos, addReferences )

import Data.Maybe ( catMaybes )
import Control.Exception ( try, SomeException )
import Data.Time.Clock ( getCurrentTime, UTCTime(utctDay) )
import Data.Time.Calendar ( toGregorian )
import System.Environment ( getArgs )

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
                    return (Just (filename <> ".html", chapter))

saveChapter :: Filename -> Date -> (Filename, Chapter, Maybe (Filename, String), Maybe (Filename, String)) -> IO ()
saveChapter path date (filename, chapter, prev, next) =
        writeFile (path ++ "/" ++ filename) (buildChapterHTML prev next date chapter) >>
        putStr (warningS . T.unpack . T.unlines . filterTodos $ chapter) >>
        (putStrLn . okS . concat) ["Saved document ", filename]

buildIndexPage :: [(Filename, String, [Html])] -> Date -> String
buildIndexPage chapters date = renderHtml $ renderIndexPage chapters date

generateSpine :: [(Filename, String)] -> [(Maybe (Filename, String), Maybe (Filename, String))]
generateSpine list = let m = Just <$> list in zip (Nothing : m) (tail m ++ [Nothing])

main :: IO ()
main = do
    path : outPath : _ <- getArgs
    index <- filter ((/=) '#' . head) . lines <$> readFile (path ++ "/index")
    parsedChapters <- catMaybes <$> mapM (parseChapter path) index
    let chapterNames = (\(a, b) -> (a, chapterName b)) <$> parsedChapters
    let spine = generateSpine chapterNames
    let renderedChapters = zipWith (\sections (file, name) -> (file, name, sections)) (sections . snd <$> parsedChapters) chapterNames
    date <- getDate
    mapM_ (saveChapter outPath date) $ zipWith (\(f, c) (p, n) -> (f, c, p, n)) parsedChapters spine
    writeFile (outPath ++ "/index.html") (buildIndexPage renderedChapters date)
