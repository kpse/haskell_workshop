module FunctionEvaluator where

import Debug.Trace

-- evaluateFunction :: (Ord a, Show a, Show b)=> (a -> Either b ([a], [b] -> b)) -> a -> b
-- evaluateFunction f n = case trace ("new entry ... " ++ show n) result of
--       Left l -> trace ("Left - l " ++ show l) l
--       Right (as, fb) -> trace ("Right as - " ++ show as) fb $ map chain as
--     where
--       chain = evaluateFunction f
--       result = f n

evaluateFunction :: (Ord a, Show a, Show b)=> (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f n = case trace "new entry .." result of
      Left l -> trace ("Left - l " ++ show l) l
      Right (as, fb) -> let all = map chain as in trace ("all - show " ++ show all) fb all
    where
      chain = evaluateFunction f
      result = f n

evaluateFunction1 :: (Int -> Either Int ([Int], [Int] -> Int)) -> Int -> Int
evaluateFunction1 f n = evaluateFunction' f n []

evaluateFunction' :: (Int -> Either Int ([Int], [Int] -> Int)) -> Int -> [([Int], [Int] -> Int)] -> Int
evaluateFunction' f n rights = case f n of
  Left l ->  trace ("Left - l " ++ show l) (snd.head) rights $ concatMap fst rights
  Right (as, fb) ->  trace ("Right - as " ++ show as) evaluateFunction' f n (rights ++ [(as, fb)])


fib1 i | i < 2     = Left i
            | otherwise = Right ([i-1, i-2], sum)
