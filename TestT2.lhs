> module Main where

> import System.IO
> import qualified Data.Map as Map
> import qualified Data.Set as Set

> import T2
> import LTK
> import LTK.Porters.ATT

> main = do
>   let aut = mkstraut $ cayley htp
>   putStr . (\(a,_,_) -> a) . extractSymbolsATT $ exportATT aut
>   hPrint stderr (isFO2 $ syntacticMonoid aut)

> htp = Transducer
>       (Map.fromList [ ((ELeft, 2), (ELeft, 2, DRight))
>                     , ((ERight, 2), (ERight, 2, DRight))
>                     , ((l, 2), (l, 2, DRight))
>                     , ((h, 2), (h, 3, DRight))
>                     , ((l, 3), (n, 3, DRight))
>                     , ((h, 3), (n, 4, DLeft))
>                     , ((ELeft, 3), (ELeft, 3, DRight))
>                     , ((ERight, 3), (n, 6, DLeft))
>                     , ((l, 4), (n, 4, DLeft))
>                     , ((h, 4), (n, 5, DRight))
>                     , ((ELeft, 4), (ELeft, 4, DLeft))
>                     , ((ERight, 4), (n, 4, DLeft))
>                     , ((l, 5), (h, 5, DRight))
>                     , ((h, 5), (h, 3, DRight))
>                     , ((ELeft, 5), (ELeft, 5, DRight))
>                     , ((ERight, 5), (n, 5, DRight))
>                     , ((l, 6), (n, 6, DLeft))
>                     , ((h, 6), (n, 7, DRight))
>                     , ((ELeft, 6), (ELeft, 6, DLeft))
>                     , ((ERight, 6), (n, 6, DLeft))
>                     , ((l, 7), (l, 7, DRight))
>                     , ((h, 7), (h, 7, DRight))
>                     , ((ELeft, 7), (ELeft, 7, DRight))
>                     , ((ERight, 7), (n, 7, DRight))
>                     ])
>       2 (Set.fromList [2,2,3,4,5,6])
>     where l = Tsym "L"
>           h = Tsym "H"
>           n = Null
>           fishl = ELeft
>           fishr = ERight
