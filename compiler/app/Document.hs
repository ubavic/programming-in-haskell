{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Document where

import Data.Text ( Text )
import Control.Lens ( (+~), makeLenses )

data Chapter = Chapter Text [Block]
    deriving (Show, Eq)

data Block = Section P Reference
    | Subsection P
    | Paragraph P 
    | CodeBlock Text (Maybe P)
    | Terminal Text (Maybe P)
    | Figure Text (Maybe P) Reference
    | Problem P [Block] Reference
    | List [P]
    | Example [Block] Reference
    | Todo Text
    deriving (Show, Eq)

data PElement = PText Text
    | PCode Text
    | PCCode Text Text
    | PPre Text
    | PMath Text
    | DisplayMath Text Text
    | PDefinition Text
    | PEmphasis Text
    | PNote P Reference
    | PReference Text
    | PUrl Text Text
    deriving (Show, Eq)

type P = [PElement]

type Reference = Int

data ReferenceState = ReferenceState {
        _sections :: Int,
        _figures :: Int,
        _problems :: Int,
        _notes :: Int,
        _examples :: Int
    }
makeLenses ''ReferenceState


filterTodos :: Chapter -> [Text]
filterTodos (Chapter _ blocks) = foldl reducer [] blocks
    where
        reducer txs (Todo todo) = todo:txs
        reducer txs _           = txs


filterSections :: Chapter -> [Block]
filterSections (Chapter _ blocks) = [ s | s@(Section _ _) <- blocks]


addReferences :: Chapter -> Chapter
addReferences (Chapter title blocks) = Chapter title $ fst $ addRef blocks (ReferenceState 0 0 0 0 0)


addRef :: [Block] -> ReferenceState -> ([Block], ReferenceState)
addRef [] state = ([], state)
addRef (x:xs) state = let (ys, s) = addRef xs newState in (y:ys, s)
    where
        (y, newState) = case x of
            (Section a _) -> (Section a . _sections $ ns, ns) where ns = sections +~ 1 $ state
            (Figure a b _) -> (Figure a b . _figures $ ns, ns) where ns = figures +~ 1 $ state
            (Problem a b _) -> (Problem a b . _problems $ ns, ns) where ns = problems +~ 1 $ state
            (Paragraph ps) -> (Paragraph qs, ns) where (qs, ns) = addNoteRef ps state
            (Example a _) -> (Example a' . _examples $ ns, ns)
                where
                    (a', ns') = addRef a state
                    ns = examples +~ 1 $ ns'
            _ -> (x, state)


addNoteRef :: [PElement] -> ReferenceState -> ([PElement], ReferenceState)
addNoteRef [] state = ([], state)
addNoteRef (x:xs) state = let (ys, s) = addNoteRef xs newState in (y:ys, s)
    where (y, newState) = case x of
            (PNote a _) -> (PNote  a . _notes $ ns, ns) where ns = notes +~ 1 $ state
            _ -> (x, state)
