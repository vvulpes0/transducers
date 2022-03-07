> module T2 where

> import Data.Map.Strict (Map)
> import Data.Set (Set)
> import LTK hiding (reverse)
> import qualified Data.Map as Map
> import qualified Data.Set as Set


Data types for deterministic two-way transducers.
Don't use Null as an input, that isn't handled (yet?)

> data Tsym e = Null | ELeft | Tsym e | ERight
>                 deriving (Eq, Ord, Read, Show)
> data Direction = DLeft | DStay | DRight deriving (Eq, Ord, Read, Show)
> data Transducer n a b
>     = Transducer
>       { transitionsT :: Map (Tsym a, n) (Tsym b, n, Direction)
>       , initialT     :: n
>       , finalsT      :: Set n
>       }
>     deriving (Eq, Ord, Read, Show)

> sinks :: Ord n => Transducer n a b -> Set n
> sinks = Set.fromList . map (\(_,b,_)->b) . Map.elems . transitionsT

> sources :: Ord n => Transducer n a b -> Set n
> sources = Set.fromList . map snd . Map.keys . transitionsT

> states :: Ord n => Transducer n a b -> Set n
> states t = Set.insert (initialT t)
>            $ Set.unions [sources t, sinks t, finalsT t]

> domain :: Ord a => Transducer n a b -> Set (Tsym a)
> domain = Set.fromList . map fst . Map.keys . transitionsT
> codomain :: Ord b => Transducer n a b -> Set (Tsym b)
> codomain = Set.fromList . map (\(b,_,_)->b) . Map.elems . transitionsT

> t = Transducer
>     (Map.fromList [ ((a,1),(a,2,DRight))
>                   , ((b,2),(b,3,DLeft))
>                   , ((a,3),(a,4,DRight))
>                   , ((b,4),(b,5,DLeft))
>                   , ((a,5),(a,6,DLeft))
>                   ])
>     1 (Set.fromList [1,2,3,4,5,6])
>     where a = Tsym "a"
>           b = Tsym "b"
> t' = Transducer
>     (Map.fromList [ ((a,1),(a,2,DRight))
>                   , ((b,2),(b,3,DLeft))
>                   , ((a,3),(a,4,DRight))
>                   , ((b,4),(b,5,DRight))
>                   , ((a,5),(a,6,DLeft))
>                   ])
>     1 (Set.fromList [1,2,3,4,5,6])
>     where a = Tsym "a"
>           b = Tsym "b"



> isLeft, isRight :: (a, b, Direction) -> Bool
> isLeft  (_, _, x) = x == DLeft
> isRight (_, _, x) = x == DRight

> stp :: (x, a) -> (y, b, z) -> (a, b)
> stp p q = (snd p, (\(_, x, _) -> x) q)

> behaviour :: (Ord a, Ord b, Ord n) =>
>              (Set n -> Transducer n a b -> [Tsym a] -> Set n)
>           -> ([Tsym a] -> [Tsym a])
>           -> Transducer n a b -> [Tsym a] -> Set (n, n)
> behaviour f g t xs = Set.unions
>                      (Set.mapMonotonic
>                       (\s -> Set.map ((,) s)
>                        $ f (Set.singleton s) t (g xs))
>                       (sources t))
> bh_ll, bh_lr, bh_rl, bh_rr ::
>     (Ord a, Ord b, Ord n) =>
>     Transducer n a b -> [Tsym a] -> Set (n,n)
> bh_ll = behaviour fs id
> bh_lr = behaviour fo id
> bh_rl = behaviour ro reverse
> bh_rr = behaviour rs reverse



leave same, string is forward

> fs :: (Ord a, Ord b, Ord n) =>
>       Set n -> Transducer n a b -> [Tsym a] -> Set n
> fs _ _ [] = Set.empty
> fs open t (x:[]) = Set.fromList . map (\(_,b,_) -> b)
>                    . Map.elems
>                    . Map.filter isLeft
>                    . Map.filterWithKey onX
>                    $ transitionsT t
>     where onX p _ = x == fst p && Set.member (snd p) open
> fs open t (x:xs)
>     | Set.null open = Set.empty
>     | otherwise = Set.union noright withright
>     where noright = fs open t [x]
>           newopen = fs (fo open t [x]) t xs
>           withright = fs (Set.difference newopen noright) t (x:xs)

leave opposite, string is forward

> fo :: (Ord a, Ord b, Ord n) =>
>       Set n -> Transducer n a b -> [Tsym a] -> Set n
> fo _ _ [] = Set.empty
> fo open t (x:[]) = Set.fromList . map (\(_,b,_) -> b)
>                    . Map.elems
>                    . Map.filter isRight
>                    . Map.filterWithKey onX
>                    $ transitionsT t
>     where onX p _ = x == fst p && Set.member (snd p) open
> fo open t (x:xs)
>     | Set.null open = Set.empty
>     | otherwise = Set.union noleft withleft
>     where noleft = fo (fo open t [x]) t xs
>           newopen = fs (fo open t [x]) t xs
>           withleft = fo (Set.difference newopen noleft) t (x:xs)

leave same, string is reversed

> rs :: (Ord a, Ord b, Ord n) =>
>       Set n -> Transducer n a b -> [Tsym a] -> Set n
> rs _ _ [] = Set.empty
> rs open t (x:[]) = fo open t [x]
> rs open t (x:xs)
>     | Set.null open = Set.empty
>     | otherwise = Set.union noleft withleft
>     where noleft = rs open t [x]
>           newopen = rs (ro open t [x]) t xs
>           withleft = rs (Set.difference newopen noleft) t (x:xs)

leave opposite, string is reversed

> ro :: (Ord a, Ord b, Ord n) =>
>       Set n -> Transducer n a b -> [Tsym a] -> Set n
> ro _ _ [] = Set.empty
> ro open t (x:[]) = fs open t [x]
> ro open t (x:xs)
>     | Set.null open = Set.empty
>     | otherwise = Set.union noright withright
>     where noright = ro (ro open t [x]) t xs
>           newopen = rs (ro open t [x]) t xs
>           withright = ro (Set.difference newopen noright) t (x:xs)



The slowest way to make a monoid, but hey, it works!

> type Behaviours n = (Set (n,n), Set (n,n), Set (n,n), Set (n,n))

> monset :: (Ord a, Ord b, Ord n) =>
>           Transducer n a b -> [([Tsym a], Behaviours n)]
> monset = truncMS Set.empty 0 . monset'

> monset' :: (Ord a, Ord b, Ord n) =>
>            Transducer n a b -> [([Tsym a], Behaviours n)]
> monset' t = map (\a -> (a, bh a)) . drop 1
>             $ stringsOver (Set.toList $ domain t)
>     where bh x = (bh_ll t x, bh_lr t x, bh_rl t x, bh_rr t x)

> truncMS :: (Ord a, Ord n) =>
>            Set (Behaviours n) -> Int -> [([Tsym a], Behaviours n)]
>         -> [([Tsym a], Behaviours n)]
> truncMS _ _ [] = []
> truncMS seen sz (x:xs)
>     | len > sz + 1 = []
>     | snd x `elem` seen = x : truncMS seen sz xs
>     | otherwise = x : truncMS (Set.insert (snd x) seen) len xs
>     where len = length (fst x)

The "cayley'" function generates the syntactic semigroup
and adjoins 1 unconditionally.
In order to be truly correct, the identity should be adjoined only
when there is no identity already present.
This requires testing to find an identity.
This is a problem for future me.

> cayley :: (Ord a, Ord b, Ord n) =>
>           Transducer n a b -> Transducer (Maybe (Behaviours n)) a ()
> cayley t = Transducer
>            { transitionsT = Map.fromList
>                             . concatMap tr
>                             $ (([],Nothing) : map (fmap Just) m)
>            , initialT = Nothing
>            , finalsT  = Set.insert Nothing . Set.fromList
>                         $ map (Just . snd) m
>            }
>     where m = filter nf $ monset t
>           mm = Map.fromList m
>           ml = length . fst $ last m
>           d = domain t `Set.difference` Set.fromList [ELeft, ERight]
>           nf x = (ELeft `notElem` fst x) && (ERight `notElem` fst x)
>           tr x
>               | length (fst x) == ml = []
>               | otherwise = map
>                             (\s -> ((s, snd x),
>                                     (Tsym (),
>                                      mm Map.!? (fst x ++ [s]),
>                                      DRight)))
>                             (Set.toList d)

> renameStates :: (Ord a, Ord b, Ord n) =>
>                 Transducer n a b -> Transducer Int a b
> renameStates t = Transducer
>                  { transitionsT = Map.fromList . map rt
>                                   . Map.assocs $ transitionsT t
>                  , initialT = r $ initialT t
>                  , finalsT  = Set.map r $ finalsT t
>                  }
>     where rt ((x, p), (y, q, d)) = ((x, r p), (y, r q, d))
>           s = Set.toList $ T2.states t
>           m = Map.fromList $ zip s [1..]
>           r x = maybe 0 id (m Map.!? x)

> mkaut :: (Ord a, Ord n) => Transducer n a () -> FSA n (Tsym a)
> mkaut t = FSA
>           { sigma = domain t
>           , transitions = Set.fromList . mktrs . Map.assocs
>                           $ transitionsT t
>           , initials = Set.singleton . State $ initialT t
>           , finals = Set.map State $ finalsT t
>           , isDeterministic = False
>           }
>     where mktrs [] = []
>           mktrs (((a,p),(_,q,_)):xs)
>               = Transition { edgeLabel = Symbol a
>                            , source = State p
>                            , destination = State q
>                            }
>                 : mktrs xs

> untsym :: Show a => Tsym a -> String
> untsym (Tsym x) = show x
> untsym ELeft = "⋊"
> untsym _ = "⋉"

> mkstraut :: Ord n => Transducer n String () -> FSA Int String
> mkstraut = renameSymbolsBy untsym' . mkaut . T2.renameStates
>     where untsym' (Tsym x) = x
>           untsym' x = untsym x



> stringsOver :: [a] -> [[a]]
> stringsOver a = [] :
>                 if null a
>                 then []
>                 else concatMap (\w -> map (: w) a) (stringsOver a)
