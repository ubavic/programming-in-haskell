{-# LANGUAGE OverloadedStrings #-}

module Parser (parseDoc) where

import Document ( P, PElement(..), Block(..), Chapter(..) )

import qualified Data.Text as T
import Data.Text ( Text )
import Data.Void ( Void )
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

cmd :: Text -> Parser Text
cmd name = char '@' >> string name

chapter :: Parser Chapter
chapter = label "chapter" $ do
        cmd "chapter" <* space
        name <- argument text <* space
        blocks <- many block
        return $ Chapter name blocks

block :: Parser Block
block = choice 
        (try <$> [section, subsection, codeBlock, figure, terminal, problem, list, todo, example, Paragraph <$> paragraphBlock])

problemBlock :: Parser Block
problemBlock = choice 
        (try <$> [codeBlock, figure, terminal, list, Paragraph <$> paragraphBlock])

section :: Parser Block
section = label "section" $ cmd "section" *> (flip Section 0 <$> argument captionText) <* space

subsection :: Parser Block
subsection = label "subsection" $ cmd "subsection" *> (Subsection <$> argument captionText) <* space

figure :: Parser Block
figure = label "figure" $ do
        cmd "figure" <* space
        path <- argument text <* space
        description <- option Nothing (Just <$> argument captionText)<* space
        return $ Figure path description 0

codeBlock :: Parser Block
codeBlock = label "codeBlock" $ do
        cmd "codeBlock" <* space
        code <- argument spaceText <* space
        description <- option Nothing (Just <$> argument captionText) <* space
        return $ CodeBlock code description

terminal :: Parser Block
terminal = label "terminal" $ do
        cmd "terminal" <* space
        code <- argument spaceText <* space
        description <- option Nothing (Just <$> argument captionText) <* space
        return $ Terminal code description

problem :: Parser Block
problem = label "problem" $ do
        cmd "problem" <* space
        problem <- argument paragraph <* space
        solution <- option [] (argument $ many problemBlock) <* space
        return $ Problem problem solution 0

example :: Parser Block
example = label "example" $ do
        cmd "example" <* space
        text <- argument (space *> many problemBlock) <* space
        return $ Example text 0

list :: Parser Block
list = label "list" $ cmd "list" *> (List <$> argument (many listItem)) <* space

listItem :: Parser P
listItem = space *> cmd "li" *> paragraph

paragraphBlock :: Parser P
paragraphBlock = cmd "p" *> paragraph

paragraph :: Parser P
paragraph = label "paragraph" $ space >>
        (some . choice $ try <$> [PText <$> text, emphasis, definition, inlineMath, code, cCode, equation, note, url, pre])

text :: Parser Text
text = label "text" $ T.concat <$> some (choice $ try <$> [nonSpecialChars, spaceChars, specialChar <* space])
        where nonSpecialChars = (\x->T.pack [x]) <$> satisfy (\x -> x /= '{' && x /= '}' && x /= '@' && x/= ' ')
              spaceChars      = space1 *> return " "

spaceText :: Parser Text
spaceText = label "space text" $ T.concat <$> some (choice $ try <$> [nonSpecialChars, specialChar])
        where nonSpecialChars = (\x->T.pack [x]) <$> satisfy (\x -> x /= '{' && x /= '}' && x /= '@')

captionText :: Parser P
captionText = space >>
        (some . choice $ try <$> [PText <$> text, emphasis, inlineMath, code, pre])

emphasis :: Parser PElement
emphasis = label "emphasis" $ cmd "em" >> PEmphasis <$> argument text

definition :: Parser PElement
definition = label "def" $ cmd "def" >> PDefinition <$> argument text

code :: Parser PElement
code = label "code" $ cmd "code" >> PCode <$> argument text

cCode :: Parser PElement
cCode = label "cCode" $ do 
        cmd "ccode"
        code <- argument text
        sign <- option "" ((\x->T.pack [x]) <$> choice (try <$> [char '.', char ',']))
        return $ PCCode code sign

pre :: Parser PElement
pre = label "pre" $ cmd "pre" >> PPre <$> argument text

note :: Parser PElement
note = label "code" $ cmd "note" >> flip PNote 0 <$> argument paragraph

reference :: Parser PElement
reference = label "reference" $ do
        cmd "ref"
        id <- argument text
        return $ PReference id

inlineMath :: Parser PElement
inlineMath = label "inline math" $ cmd "m" >> PMath <$> argument mathCode

equation :: Parser PElement
equation = label "block math" $ do
        cmd "eq"
        math <- argument mathCode
        return $ DisplayMath math ""

url :: Parser PElement
url = label "url" $ do
        cmd "url"
        name <- argument text
        url <- argument text
        return $ PUrl name url

mathCode :: Parser Text
mathCode = label "math code" $ T.concat <$> many (choice $ try <$> [T.pack <$> some onf, addParentheses <$> argument mathCode])
        where
                addParentheses s = T.concat ["{", s, "}"]
                onf = satisfy (\x -> x /= '{' && x /= '}')

todo :: Parser Block
todo = label "todo" $ cmd "todo" *> (Todo <$> argument text) <* space

specialChar :: Parser Text
specialChar = label "specialChar" $ cmd "@" *> ((\x->T.pack [x]) <$> choice (try <$> [char '@', char '{', char '}']))

argument :: Parser a -> Parser a
argument x = label "argument" $ between (char '{' <* space) (char '}') x

parseDoc :: Text -> Either String Chapter
parseDoc input = let output = parse (chapter <* space <* eof) "" input
        in case output of
                Left err -> Left $ errorBundlePretty err
                Right output -> Right output

