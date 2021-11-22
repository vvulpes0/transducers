> module Main where

> import Control.Applicative (liftA2,liftA3)
> import Data.List (intercalate)
> import Data.Maybe (isJust)
> import System.Environment (getArgs)
> import qualified Data.Set as Set

> import LTK
> import Transducer

> main = interact (to Dot . mkfsa . readATTT)

> formatT :: (Maybe String,[String]) -> String
> formatT (a,b)
>     | [a] == map Just b = maybe "λ" id a
>     | otherwise = (maybe "λ" id a) ++ ":" ++ f b
>     where f [] = "λ"
>           f xs = intercalate " " xs

> mkfsa :: Ord n => Transducer n String [String] -> FSA Int String
> mkfsa = renameStates . renameSymbolsBy formatT . f . Transducer.toFSA
>     where f x = flip contractAlphabetTo x
>                 (Set.filter (isJust . fst) $ alphabet x)
