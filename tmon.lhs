> module Main where

> import LTK
> import Transducer

> main = interact (to Dot . formatMonoid . mkmon . readATTT)
