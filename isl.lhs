> module Main where

> import Control.Applicative (liftA2,liftA3)
> import System.Environment (getArgs)

> import Transducer

> main = f =<< getArgs
>     where f (x:y:[]) = print
>                        =<< liftA3 (,,)
>                          (isISL <$> r x)
>                          (isISL <$> r y)
>                          (isISL <$> liftA2 compose (r x) (r y))
>           f _ = putStrLn "usage: isl t1 t2"
>           r = fmap readATTT . readFile
