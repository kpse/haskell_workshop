module FunctionEvaluator where

import Debug.Trace

evaluateFunction :: (Ord a, Show a, Show b)=> (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f n = case result of
      Left l -> trace ("Left - l " ++ show l) l
      Right (as, fb) -> trace ("Right as - " ++ show as) fb $ map chain as
    where
      chain = evaluateFunction f
      result = f n
