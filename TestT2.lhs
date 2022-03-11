> module Main where

> import System.IO
> import qualified Data.Map as Map
> import qualified Data.Set as Set

> import T2
> import LTK
> import LTK.Porters.ATT

> main = do
>   let aut = mkstraut $ cayley bh_lr tutrugbuNoC
>   putStr . (\(a,_,_) -> a) . extractSymbolsATT $ exportATT aut
>   --hPrint stderr (isLT $ syntacticMonoid aut)

note that C is a nonsalient symbol for tutrugbu,
which means it is the identity.
The inserted identity should be removed before furthur processing.
I'll code in the check to avoid spurious insertion at some point.

> tutrugbu = Transducer
>            (Map.fromList [ ((cc, 1), s (cc, 1, DRight))
>                          , ((ht, 1), s (ht, 2, DRight))
>                          , ((mt, 1), s (mt, 2, DRight))
>                          , ((hl, 1), s (hl, 3, DRight))
>                          , ((ml, 1), s (ml, 3, DRight))
>                          , ((cc, 2), s (cc, 2, DRight))
>                          , ((ht, 2), s (ht, 2, DRight))
>                          , ((mt, 2), s (mt, 2, DRight))
>                          , ((hl, 2), s (ht, 2, DRight))
>                          , ((ml, 2), s (mt, 2, DRight))
>                          , ((rt, 2), s (nl, 4, DRight))
>                          , ((cc, 3), s (cc, 3, DRight))
>                          , ((ht, 3), s (hl, 3, DRight))
>                          , ((mt, 3), s (ml, 3, DRight))
>                          , ((hl, 3), s (hl, 3, DRight))
>                          , ((ml, 3), s (ml, 3, DRight))
>                          , ((rt, 3), s (nl, 5, DRight))
>                          , ((cc, 4), s (cc, 4, DRight))
>                          , ((ht, 4), s (ht, 4, DRight))
>                          , ((hl, 4), s (ht, 4, DRight))
>                          , ( (mt, 4)
>                            , Set.fromList
>                              [ (ml, 6, DRight), (mt, 7, DRight) ])
>                          , ( (ml, 4)
>                            , Set.fromList
>                              [ (ml, 6, DRight), (mt, 7, DRight) ])
>                          , ((cc, 5), s (cc, 5, DRight))
>                          , ((ht, 5), s (hl, 5, DRight))
>                          , ((hl, 5), s (hl, 5, DRight))
>                          , ((mt, 5), s (ml, 5, DRight))
>                          , ((ml, 5), s (ml, 5, DRight))
>                          , ((cc, 6), s (cc, 6, DRight))
>                          , ((mt, 6), s (ml, 6, DRight))
>                          , ((ml, 6), s (ml, 6, DRight))
>                          , ( (ht, 6)
>                            , Set.fromList
>                              [ (hl, 6, DRight), (hl, 8, DRight) ])
>                          , ( (hl, 6)
>                            , Set.fromList
>                              [ (hl, 6, DRight), (hl, 8, DRight) ])
>                          , ((cc, 7), s (cc, 7, DRight))
>                          , ((mt, 7), s (mt, 7, DRight))
>                          , ((ml, 7), s (mt, 7, DRight))
>                          , ( (ht, 7)
>                            , Set.fromList
>                              [ (ht, 7, DRight), (ht, 8, DRight) ])
>                          , ( (hl, 7)
>                            , Set.fromList
>                              [ (ht, 7, DRight), (ht, 8, DRight) ])
>                          , ((cc, 8), s (cc, 8, DRight))
>                          ])
>            1 (Set.fromList [4, 5, 8])
>     where cc = Tsym "c"
>           ht = Tsym "i"
>           hl = Tsym "ɪ"
>           mt = Tsym "o"
>           ml = Tsym "ɔ"
>           nl = Null
>           rt = ELeft -- use a start-fish for a root marker
>           s = Set.singleton

An alternative definition, and indeed the transducer obtained
by removing extraneous states in the McCollum et al preprint.
The tutrugbuNoC transducer is simply tutrugbu without the
"c" transtions, obviating the need to account for adjoined identities.

> tutrugbuNoC
>     = Transducer
>       (Map.fromList [ ((ht, 1), s (ht, 2, DRight))
>                     , ((mt, 1), s (mt, 2, DRight))
>                     , ((hl, 1), s (hl, 3, DRight))
>                     , ((ml, 1), s (ml, 3, DRight))
>                     , ((ht, 2), s (ht, 2, DRight))
>                     , ((mt, 2), s (mt, 2, DRight))
>                     , ((hl, 2), s (ht, 2, DRight))
>                     , ((ml, 2), s (mt, 2, DRight))
>                     , ((rt, 2), s (nl, 4, DRight))
>                     , ((ht, 3), s (hl, 3, DRight))
>                     , ((mt, 3), s (ml, 3, DRight))
>                     , ((hl, 3), s (hl, 3, DRight))
>                     , ((ml, 3), s (ml, 3, DRight))
>                     , ((rt, 3), s (nl, 5, DRight))
>                     , ((ht, 4), s (ht, 4, DRight))
>                     , ((hl, 4), s (ht, 4, DRight))
>                     , ( (mt, 4)
>                       , Set.fromList
>                         [ (ml, 6, DRight), (mt, 7, DRight) ])
>                     , ( (ml, 4)
>                       , Set.fromList
>                         [ (ml, 6, DRight), (mt, 7, DRight) ])
>                     , ((ht, 5), s (hl, 5, DRight))
>                     , ((hl, 5), s (hl, 5, DRight))
>                     , ((mt, 5), s (ml, 5, DRight))
>                     , ((ml, 5), s (ml, 5, DRight))
>                     , ((mt, 6), s (ml, 6, DRight))
>                     , ((ml, 6), s (ml, 6, DRight))
>                     , ( (ht, 6)
>                       , Set.fromList
>                         [ (hl, 6, DRight), (hl, 8, DRight) ])
>                     , ( (hl, 6)
>                       , Set.fromList
>                         [ (hl, 6, DRight), (hl, 8, DRight) ])
>                     , ((mt, 7), s (mt, 7, DRight))
>                     , ((ml, 7), s (mt, 7, DRight))
>                     , ( (ht, 7)
>                       , Set.fromList
>                         [ (ht, 7, DRight), (ht, 8, DRight) ])
>                     , ( (hl, 7)
>                       , Set.fromList
>                         [ (ht, 7, DRight), (ht, 8, DRight) ])
>                     ])
>       1 (Set.fromList [4, 5, 8])
>     where ht = Tsym "i"
>           hl = Tsym "ɪ"
>           mt = Tsym "o"
>           ml = Tsym "ɔ"
>           nl = Null
>           rt = ELeft -- use a start-fish for a root marker
>           s = Set.singleton

High-tone plateauing, a different circumambient unbounded process

> htp = Transducer
>       (Map.fromList [ ((l, 1), s (l, 1, DRight))
>                     , ((l, 2), s (n, 2, DRight))
>                     , ((l, 3), s (n, 3, DLeft))
>                     , ((l, 4), s (h, 4, DRight))
>                     , ((l, 5), s (n, 5, DLeft))
>                     , ((l, 6), s (l, 6, DRight))
>                     , ((h, 1), s (h, 2, DRight))
>                     , ((h, 2), s (n, 3, DLeft))
>                     , ((h, 3), s (n, 4, DRight))
>                     , ((h, 4), s (h, 2, DRight))
>                     , ((h, 5), s (n, 6, DRight))
>                     , ((fishr, 2), s (n, 5, DLeft))
>                     ])
>       1 (Set.fromList [1,6])
>     where l = Tsym "L"
>           h = Tsym "H"
>           n = Null
>           fishr = ERight
>           s = Set.singleton

> cd = Transducer
>      (Map.fromList [ ((a, 1), s (a, 1, DRight))
>                    , ((a, 2), s (b, 2, DLeft))
>                    , ((a, 3), s (n, 3, DRight))
>                    , ((b, 1), s (n, 2, DLeft))
>                    , ((b, 2), s (n, 3, DRight))
>                    , ((b, 3), s (n, 1, DRight))
>                    , ((fishl, 1), s (n, 1, DRight))
>                    , ((fishl, 2), s (n, 3, DRight))
>                    , ((fishr, 1), s (n, 2, DLeft))
>                    ])
>      1 (Set.fromList [3])
>     where a = Tsym "a"
>           b = Tsym "b"
>           n = Null
>           fishl = ELeft
>           fishr = ERight
>           s = Set.singleton
