module Pagination where

import Data.List.Split (chunksOf)
import Data.List (elemIndex)

type Collection a = [a]
type ItemsPerPage = Int

itemCount :: Collection a -> Int
itemCount = length

pageCount :: Collection a -> ItemsPerPage -> Int
pageCount [] _ = 0
pageCount xs n = (itemCount . chunksOf n) xs

pageItemCount :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageItemCount [] _ _ = Nothing
pageItemCount _ _ page | page < 0 = Nothing
pageItemCount _ n _ | n <= 0 = Nothing
pageItemCount xs n page | page > (pageCount xs n) - 1 = Nothing
pageItemCount xs n page = Just $ (length . head . drop page . chunksOf n) xs

pageIndex :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageIndex [] _ _ = Nothing
pageIndex _ n _ | n <= 0 = Nothing
pageIndex _ _ item | item < 0 = Just 0
pageIndex xs _ item | item >= itemCount xs = Nothing
pageIndex xs n item = Just $ item `div` n
