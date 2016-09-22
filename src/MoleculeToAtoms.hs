module MoleculeToAtoms where

import Data.Char
import Data.Map (fromListWith, toList)

parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = case readBrackets formula of
  err | (snd .last) err < 0 -> Left $ (fst .last) err
  l@(x:xs) -> Right $ formatAtoms l

-- one Element. one Digit
takePieces :: String -> [(String,Int)]
takePieces [] = []
takePieces input = let elem = takeWhile isAlpha input
                       rest = dropWhile isAlpha input
  in
    case (elem, rest) of
      (x:xs, _) | (not.isUpper) x -> [("Not a valid molecule", -1)]
      (_, x:xs) | isDigit x ->
        let count = read (takeWhile isDigit rest) :: Int
            elems = [ (name, 1) | name <- splitCamelCase elem ]
          in
            init elems ++ [((fst . last) elems, count)] ++ takePieces (dropWhile isDigit rest)
      ([], _) -> []
      _ -> (elem, 1) : takePieces rest

brackets = ['(', ')', '[', ']', '{', '}']

readBrackets :: String -> [(String,Int)]
readBrackets [] = []
readBrackets input = case head input of
  open | open `elem` brackets ->
    let (inBracket,end) = span (\w -> Just w /= closeBrackets open) $ tail input in
      case end of
        end@(x:xs) | closeBrackets open == Just x -> [ (a, c * read (takeWhile isDigit xs) :: Int ) | (a, c) <- readBrackets inBracket]
        _ -> [("Mismatched parenthesis", -1)]
  _ -> let (normal,inBracket) = span (`notElem` brackets) input in takePieces normal ++ readBrackets inBracket

closeBrackets :: Char -> Maybe Char
closeBrackets '(' = Just ')'
closeBrackets '{' = Just '}'
closeBrackets '[' = Just ']'
closeBrackets _ = Nothing

formatAtoms :: [(String, Int)] -> [(String, Int)]
formatAtoms [] = []
formatAtoms atoms = toList $ fromListWith (+) $ foldl (\acc i -> acc ++ splitElem i) [] atoms

splitElem :: (String, Int) -> [(String, Int)]
splitElem (name, count) = [ (atom, count) | atom <- splitCamelCase name]

splitCamelCase :: String -> [String]
splitCamelCase [] = []
splitCamelCase arr = foldl (\acc i -> if isUpper i then acc ++ [[i]] else init acc ++ [last acc ++ [i]]) [] arr
