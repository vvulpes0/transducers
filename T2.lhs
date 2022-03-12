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
>       { transitionsT :: Map (Tsym a, n) (Set (Tsym b, n, Direction))
>       , initialT     :: n
>       , finalsT      :: Set n
>       }
>     deriving (Eq, Ord, Read, Show)

> sinks :: Ord n => Transducer n a b -> Set n
> sinks = Set.fromList . map (\(_,b,_)->b) . listify . transitionsT

> sources :: Ord n => Transducer n a b -> Set n
> sources = Set.fromList . map snd . Map.keys . transitionsT

> states :: Ord n => Transducer n a b -> Set n
> states t = Set.insert (initialT t)
>            $ Set.unions [sources t, sinks t, finalsT t]

> domain :: Ord a => Transducer n a b -> Set (Tsym a)
> domain = Set.fromList . map fst . Map.keys . transitionsT
> codomain :: Ord b => Transducer n a b -> Set (Tsym b)
> codomain = Set.fromList . map (\(b,_,_)->b) . listify . transitionsT

> t = Transducer
>     (Map.fromList [ ((a,1),s (a,2,DRight))
>                   , ((b,2),s (b,3,DLeft))
>                   , ((a,3),s (a,4,DRight))
>                   , ((b,4),s (b,5,DLeft))
>                   , ((a,5),s (a,6,DLeft))
>                   ])
>     1 (Set.fromList [1,2,3,4,5,6])
>     where a = Tsym "a"
>           b = Tsym "b"
>           s = Set.singleton
> t' = Transducer
>     (Map.fromList [ ((a,1),s (a,2,DRight))
>                   , ((b,2),s (b,3,DLeft))
>                   , ((a,3),s (a,4,DRight))
>                   , ((b,4),s (b,5,DRight))
>                   , ((a,5),s (a,6,DLeft))
>                   ])
>     1 (Set.fromList [1,2,3,4,5,6])
>     where a = Tsym "a"
>           b = Tsym "b"
>           s = Set.singleton



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
>                    . listify
>                    . Map.map (Set.filter isLeft)
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
>                    . listify
>                    . Map.map (Set.filter isRight)
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

> twoway :: (Ord a, Ord b, Ord n) =>
>           Transducer n a b -> [Tsym a] -> Behaviours n
> twoway t x = (bh_ll t x, bh_lr t x, bh_rl t x, bh_rr t x)

> monset :: (Ord a, Ord b, Ord n, Ord e) =>
>           (Transducer n a b -> [Tsym a] -> e)
>        -> Transducer n a b -> [([Tsym a], e)]
> monset bh = truncMS Set.empty 0 . monset' bh

> monset' :: (Ord a, Ord b, Ord n) =>
>            (Transducer n a b -> [Tsym a] -> e)
>         -> Transducer n a b -> [([Tsym a], e)]
> monset' bh t = map (\a -> (a, bh t a)) . drop 1
>             $ stringsOver (Set.toList $ domain t)

> truncMS :: (Ord a, Ord e) =>
>            Set e -> Int -> [([Tsym a], e)] -> [([Tsym a], e)]
> truncMS _ _ [] = []
> truncMS seen sz (x:xs)
>     | len > sz + 1 = []
>     | snd x `elem` seen = x : truncMS seen sz xs
>     | otherwise = x : truncMS (Set.insert (snd x) seen) len xs
>     where len = length (fst x)

The "cayley'" function generates the syntactic semigroup
and adjoins 1 iff no existing element is the identity.
The identity test is not exactly cheap, but it will do for now.

> cayley :: (Ord a, Ord b, Ord n, Ord e) =>
>           (Transducer n a b -> [Tsym a] -> e)
>        -> Transducer n a b -> Transducer (Maybe e) a ()
> cayley bh t = Transducer
>            { transitionsT = Map.fromList
>                             . concatMap tr
>                             . (maybe (([],Nothing):) (const id) i)
>                             $ map (fmap Just) m
>            , initialT = i
>            , finalsT  = Set.insert i . Set.fromList
>                         $ map (Just . snd) m
>            }
>     where m = filter nf $ monset bh t
>           mm = Map.fromList m
>           ml = length . fst $ last m
>           isLI x = all (\(w,g) -> bh t (x++w) == g) m
>           isRI x = all (\(w,g) -> bh t (w++x) == g) m
>           ids = filter (uncurry (const . isRI))
>                 $ filter (uncurry (const . isLI)) m
>           i = case ids of
>                 (x:_) -> Just (snd x)
>                 _     -> Nothing
>           d = domain t `Set.difference` Set.fromList [ELeft, ERight]
>           nf x = (ELeft `notElem` fst x) && (ERight `notElem` fst x)
>           tr x
>               | length (fst x) == ml = []
>               | otherwise = map
>                             (\s -> ((s, snd x),
>                                     Set.singleton
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
>     where rt ((x, p), ds) = ((x, r p), Set.map f ds)
>           f (y, q, d) = (y, r q, d)
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
>           mktrs ((a,ds):xs)
>               = map (mktr a) (Set.toList ds) ++ mktrs xs
>           mktr (a,p) (_,q,_) = Transition { edgeLabel = Symbol a
>                                           , source = State p
>                                           , destination = State q
>                                           }

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

> listify = concatMap Set.toList . Map.elems
