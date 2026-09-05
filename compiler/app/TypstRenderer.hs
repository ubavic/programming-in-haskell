{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module TypstRenderer (renderTypstChapter, renderTypstBook) where

import Document

import Data.Text (Text)
import qualified Data.Text as T

class TypstRenderer a where
    renderTypst :: a -> Text

instance TypstRenderer PElement where
    renderTypst (PText str) = escapeTypst (T.map whitespaceToSpace str)
        where whitespaceToSpace c
                | c == '\n' || c == '\r' || c == '\t' = ' '
                | otherwise = c
    renderTypst (PCode str) = inlineRaw (Just "haskell") str
    renderTypst (PCCode str sign) =
        "#align(center)[" <> inlineRaw (Just "haskell") str <> escapeTypst sign <> "]"
    renderTypst (PPre str) = inlineRaw Nothing str
    renderTypst (PMath str) = math False str
    renderTypst (DisplayMath eq _) = math True eq
    renderTypst (PDefinition str) = "#strong[" <> escapeTypst str <> "]"
    renderTypst (PEmphasis str) = "#emph[" <> escapeTypst str <> "]"
    renderTypst (PNote content _) = "#footnote[" <> renderTypst content <> "]"
    renderTypst (PReference _) = "(1)"
    renderTypst (PUrl name url) =
        "#link(" <> typstString url <> ")[" <> escapeTypst name <> "]"

instance TypstRenderer P where
    renderTypst = T.concat . map renderTypst

instance TypstRenderer Block where
    renderTypst (Section name ref) =
        "== " <> renderTypst name <> " <S" <> T.pack (show ref) <> ">"
    renderTypst (Subsection name) = "=== " <> renderTypst name
    renderTypst (Paragraph ps) = renderTypst ps
    renderTypst (CodeBlock code desc) = renderedCodeBlock "haskell" code desc
    renderTypst (Terminal code desc) = renderedCodeBlock "" code desc
    renderTypst (Figure path desc _) =
        case desc of
            Nothing -> "#figure(" <> img <> ")"
            Just caption ->
                "#figure(\n  " <> img <> ",\n  caption: [" <> renderTypst caption <> "],\n)"
        where img = "image(" <> typstString ("img/" <> path <> ".svg") <> ")"
    renderTypst (Problem ps s ref) =
        "#problem(" <> T.pack (show ref) <> solutionArg <> ")[" <> renderTypst ps <> "]"
        where
            solutionArg
                | null s = ""
                | otherwise = ", solution: [\n" <> renderBlocks s <> "\n]"
    renderTypst (Example ps ref) =
        "#example(" <> T.pack (show ref) <> ")[\n" <> renderBlocks ps <> "\n]"
    renderTypst (List items) =
        T.intercalate "\n" $ map (("+ " <>) . renderTypst) items
    renderTypst (Todo _) = ""

renderTypstChapter :: Chapter -> Text
renderTypstChapter (Chapter name blocks) =
    "= " <> escapeTypst name <> "\n\n" <> renderBlocks blocks

renderTypstBook :: Text -> Text -> [Chapter] -> Text
renderTypstBook prologue epilogue chapters =
    prologue
    <> "\n\n"
    <> T.intercalate "\n\n" (map renderTypstChapter chapters)
    <> "\n\n"
    <> epilogue
    <> "\n"

renderBlocks :: [Block] -> Text
renderBlocks = T.intercalate "\n\n" . filter (not . T.null) . map renderTypst

renderedCodeBlock :: Text -> Text -> Maybe P -> Text
renderedCodeBlock lang code desc =
    case desc of
        Nothing -> fence
        Just caption ->
            "#figure(\n  [\n" <> fence <> "\n  ],\n  caption: [" <> renderTypst caption <> "],\n)"
    where
        fence = codeFence lang (dropOuterNewlines code)

codeFence :: Text -> Text -> Text
codeFence lang code =
    ticks <> lang <> "\n" <> code <> "\n" <> ticks
    where ticks = T.replicate (max 3 (longestRun '`' code + 1)) "`"

inlineRaw :: Maybe Text -> Text -> Text
inlineRaw mLang str =
    "#raw(" <> T.intercalate ", " args <> ")"
    where
        args = ["lang: " <> typstString lang | Just lang <- [mLang]] ++ [typstString str]

math :: Bool -> Text -> Text
math display expr =
    cmd <> "(" <> ticks <> normalizeMath expr <> ticks <> ")"
    where
        cmd = if display then "#mitex" else "#mi"
        ticks = T.replicate (max 1 (longestRun '`' expr + 1)) "`"

normalizeMath :: Text -> Text
normalizeMath =
    T.replace "\\lt" "<" .
    T.replace "\\gt" ">"

escapeTypst :: Text -> Text
escapeTypst = T.replace "//" "\\/\\/" . T.concatMap escapeChar
    where
        specials = ['\\', '*', '_', '`', '$', '#', '@', '<', '>', '~', '[', ']']
        escapeChar c
            | c `elem` specials = T.pack ['\\', c]
            | otherwise = T.singleton c

typstString :: Text -> Text
typstString t = "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" t) <> "\""

longestRun :: Char -> Text -> Int
longestRun c t =
    case [T.length g | g <- T.group t, T.head g == c] of
        [] -> 0
        xs -> maximum xs

dropOuterNewlines :: Text -> Text
dropOuterNewlines = T.dropAround (\c -> c == '\n' || c == '\r')
