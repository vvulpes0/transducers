> module Main where

> import System.IO
> import qualified Data.Map as Map
> import qualified Data.Set as Set

> import T2
> import LTK
> import LTK.Porters.ATT

> main = do
>   let aut = mkstraut $ cayley cd
>   putStr . (\(a,_,_) -> a) . extractSymbolsATT $ exportATT aut
>   --hPrint stderr (isLT $ syntacticMonoid aut)

> htp = Transducer
>       (Map.fromList [ ((l, 1), (l, 1, DRight))
>                     , ((l, 2), (n, 2, DRight))
>                     , ((l, 3), (n, 3, DLeft))
>                     , ((l, 4), (h, 4, DRight))
>                     , ((l, 5), (n, 5, DLeft))
>                     , ((l, 6), (l, 6, DRight))
>                     , ((h, 1), (h, 2, DRight))
>                     , ((h, 2), (n, 3, DLeft))
>                     , ((h, 3), (n, 4, DRight))
>                     , ((h, 4), (h, 2, DRight))
>                     , ((h, 5), (n, 6, DRight))
>                     , ((fishr, 2), (n, 5, DLeft))
>                     ])
>       1 (Set.fromList [1,6])
>     where l = Tsym "L"
>           h = Tsym "H"
>           n = Null
>           fishr = ERight

> cd = Transducer
>      (Map.fromList [ ((a, 1), (a, 1, DRight))
>                    , ((a, 2), (b, 2, DLeft))
>                    , ((a, 3), (n, 3, DRight))
>                    , ((b, 1), (n, 2, DLeft))
>                    , ((b, 2), (n, 3, DRight))
>                    , ((b, 3), (n, 1, DRight))
>                    , ((fishl, 1), (n, 1, DRight))
>                    , ((fishl, 2), (n, 3, DRight))
>                    , ((fishr, 1), (n, 2, DLeft))
>                    ])
>      1 (Set.fromList [3])
>     where a = Tsym "a"
>           b = Tsym "b"
>           n = Null
>           fishl = ELeft
>           fishr = ERight
