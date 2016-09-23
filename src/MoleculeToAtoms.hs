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
takePieces input =  case (elem, rest) of
      (x:xs, _) | (not.isUpper) x -> [("Not a valid molecule", -1)]
      (_, x:xs) | isDigit x ->
        let (countExp, restExp) = span isDigit rest
            count = readCount countExp
            elems = [ (name, 1) | name <- splitCamelCase elem ]
          in
            init elems ++ [((fst . last) elems, count)] ++ takePieces restExp
      ([], _) -> []
      _ -> (elem, 1) : takePieces rest
      where (elem, rest) = span isAlpha input

brackets = "()[]{}"

readBrackets :: String -> [(String,Int)]
readBrackets [] = []
readBrackets input = case head input of
  open | open `elem` brackets ->
    case end of
      end@(x:xs) | closeBrackets open == Just x ->
        [(a, c * readCount current ) | (a, c) <- readBrackets inBracket  ] ++ readBrackets rest
        where (current, rest) = span isDigit xs
      _ -> [("Mismatched parenthesis", -1)]
    where (inBracket,end) = span (\w -> Just w /= closeBrackets open) $ tail input
  _ -> takePieces normal ++ readBrackets inBracket where (normal,inBracket) = span (`notElem` brackets) input

readCount :: String -> Int
readCount [] = 1
readCount str = read str :: Int

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
splitCamelCase [x] = [[x]]
splitCamelCase arr = foldl (\acc i -> if isUpper i then acc ++ [[i]] else init acc ++ [last acc ++ [i]]) [] arr
