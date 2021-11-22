> module Transducer where

> import Control.Applicative (liftA2)
> import Control.Monad (join)
> import Data.Bifunctor
> import Data.Char (isDigit)
> import Data.Map.Strict (Map)
> import Data.Maybe (catMaybes)
> import Data.Set (Set)
> import LTK
> import LTK.Algebra (SynMon)
> import qualified Data.Map.Strict as Map
> import qualified Data.Set as Set

> data Transducer n a b
>     = Transducer
>       { transitionsT :: Map (a,n) (b,n)
>       , initialT     :: n
>       , finalsT      :: Set n
>       , piT          :: b
>       , sigmaT       :: Map n b
>       }
>     deriving (Eq, Ord, Read, Show)

> empty :: Monoid b => Transducer n a b
> empty = Transducer Map.empty undefined Set.empty mempty Map.empty

> -- |@compose f g@ is a sort of product construction
> -- which yields a transducer whose output is equal to
> -- what would be obtained if the given input were run
> -- through g and then that output subsequently run through f.
> -- The transducer version of the @(.)@ function.
> compose :: (Ord m, Ord n, Ord a, Ord b, Ord c, Monoid c) =>
>            Transducer m b c -> Transducer n a [b]
>         -> Transducer (m,n) a c
> compose f g
>     | maybe True (const False) mpi = Transducer.empty
>     | otherwise = Transducer
>                   { transitionsT = Map.fromList $ Set.toList trs
>                   , initialT = (i'f, initialT g)
>                   , finalsT = fins
>                   , piT = piT f <> p
>                   , sigmaT = Map.fromList
>                              . map (\x -> (x, maybe mempty fst
>                                               $ uncurry suff x))
>                              $ Set.toList fins
>                   }
>     where mpi = transduceQ f (initialT f) (piT g)
>           (sts, trs) = cprod f g Set.empty Set.empty
>                        $ Set.singleton (i'f, initialT g)
>           fins = Set.filter (uncurry acc) sts
>           (p, i'f) = maybe undefined id mpi
>           acc m n = maybe False (flip elem (finalsT f) . snd)
>                     (suff m n)
>                     && elem n (finalsT g)
>           suff m n = transduceQ f m
>                      $ Map.findWithDefault [] n (sigmaT g)

> cprod :: (Ord a, Ord b, Ord c, Ord m, Ord n, Monoid c) =>
>          Transducer m b c -> Transducer n a [b]
>       -> Set ((a,(m,n)),(c,(m,n)))
>       -> Set (m,n) -> Set (m,n)
>       -> (Set (m,n), Set ((a,(m,n)),(c,(m,n))))
> cprod f g trs closed open
>     | Set.isSubsetOf nopen clopen = (clopen, Set.union trs ntrs)
>     | otherwise = cprod f g (Set.union trs ntrs) clopen
>                   $ Set.difference nopen clopen
>     where nopen = Set.map (snd . snd) ntrs
>           ntrs  = Set.fromList . catMaybes $ Set.toList ntrs'
>           ntrs' = Set.unions
>                   $ map (\a -> Set.map (uncurry (go a)) open) alph
>           clopen = Set.union closed open
>           alph = Set.toList . Set.fromAscList . map fst
>                  . Map.keys $ transitionsT g
>           go a p q = (maybe Nothing (Just . (,) (a,(p,q))))
>                      <$> join $ uncurry (#)
>                      <$> bimap (transduceQ f p) id
>                      <$> transduceQ g q [a]
>           a # n = maybe Nothing (\(c,m) -> Just (c,(m,n))) a

> compose' :: (Monoid c, Ord a, Ord b, Ord m, Ord n) =>
>             Transducer m b c -> Transducer n a [b]
>          -> a -> m -> n
>          -> Maybe (c, (m, n))
> compose' f g a p q
>     = join $ uncurry (#)
>       <$> bimap (transduceQ f p) id <$> transduceQ g q [a]
>     where m # x = maybe Nothing (\(b,n) -> Just (b,(n,x))) m

> -- |The output associated with a given input
> transduce :: (Ord n, Ord a, Monoid b) =>
>              Transducer n a b -> [a] -> Maybe b
> transduce f xs
>     = (<>) p <$> uncurry (<>)
>       <$> bimap id s <$> transduceQ f (initialT f) xs
>     where p = piT f
>           s = flip (Map.findWithDefault mempty) (sigmaT f)

> -- |The outputs associated with a given input from a given state
> transduceQ :: (Ord n, Ord a, Monoid b) =>
>               Transducer n a b -> n -> [a] -> Maybe (b, n)
> transduceQ f n [] = Just (mempty, n)
> transduceQ f n (x:xs)
>     = join $ uncurry (#)
>       <$> fmap (flip (transduceQ f) xs)
>       <$> Map.lookup (x,n) (transitionsT f)
>     where str # m = bimap (str<>) id <$> m


Conversions

> -- |Convert a transducer to an LTK-style FSA, ignoring the prefix.
> -- Suffixes are retained to prevent unwarranted state-merging.
> toFSA :: (Ord n, Ord a, Ord b) =>
>          Transducer n a b -> FSA (Maybe n) (Maybe a,b)
> toFSA t = FSA { sigma = alph
>               , initials = Set.singleton (State (Just $ initialT t))
>               , finals = Set.singleton (State Nothing)
>               , transitions = Set.union trs ftrs
>               , isDeterministic = False
>               }
>     where as = Map.assocs $ transitionsT t
>           alph = Set.union fal
>                  . Set.fromList
>                  $ map (\x -> (Just . fst$fst x,fst$snd x)) as
>           fal  = Set.fromList
>                  . map ((,) Nothing . snd) . Map.assocs $ sigmaT t
>           trs = Set.fromList
>                 $ (map (\((i,p),(o,q)) ->
>                         Transition { edgeLabel = Symbol (Just i,o)
>                                    , source = State (Just p)
>                                    , destination = State (Just q)
>                                    })
>                    as)
>           ftrs = Set.fromList
>                  . map (\(q,o) ->
>                         Transition { edgeLabel = Symbol (Nothing, o)
>                                    , source = State (Just q)
>                                    , destination = State Nothing
>                                    })
>                  $ Map.assocs (sigmaT t)

> mkaut :: (Ord a, Ord b, Ord n) => Transducer n a b -> FSA Integer a
> mkaut f = renameSymbolsBy g $ contractAlphabetTo a x
>     where x = renameStates $ Transducer.toFSA f
>           a = Set.filter (isJust . fst) $ alphabet x
>           isJust = maybe False (const True)
>           g = maybe undefined id . fst

> mkmon :: (Ord a, Ord b, Ord n) =>
>          Transducer n a b -> SynMon Integer a
> mkmon = syntacticMonoid . mkaut

> isISL :: (Ord a, Ord b, Ord n) => Transducer n a b -> Bool
> isISL = isDef . mkaut

> formatMonoid :: Ord n => SynMon n String -> FSA String String
> formatMonoid = renameStatesBy (concat.([]++).unsymbols.snd)


Basic AT&T-ish import, not as thorough as in LTK proper,
and with extensions

> readATTT :: String -> Transducer Int String [String]
> readATTT s = Transducer
>              { transitionsT = Map.fromList trs
>              , initialT = read . hed $ hed lws
>              , finalsT = Set.fromList $ map fst fs
>              , piT = p
>              , sigmaT = Map.fromList fs
>              }
>     where hed = concat . take 1
>           lws = map words $ lines s
>           isTr ws = (all (all isDigit) $ take 2 ws)
>                     && length ws > 2
>           fs = map
>                (bimap (read . concat) (filter (/= "0")) . splitAt 1)
>                $ filter (not . isTr) lws
>           (trs,i,p) = (\s -> makeTransitions s Nothing Nothing [])
>                       $ filter isTr lws

> makeTransitions :: [[String]]
>                 -> Maybe Int
>                 -> Maybe [String]
>                 -> [((String,Int),([String],Int))]
>                 -> ( [((String,Int),([String],Int))]
>                    , Int
>                    , [String]
>                    )
> makeTransitions [] m s a = (a, maybe 0 id m, maybe [] id s)
> makeTransitions (x:xs) m s a
>     | prefixed = makeTransitions xs (p # m) (os # s) a
>     | otherwise = makeTransitions xs (p # m) s (((i,p),(os,q)) : a)
>     where (p' : q' : i : os') = x
>           p = read p'
>           q = read q'
>           os = if null os'
>                then [i]
>                else filter (/= "0") os'
>           a # b = maybe (Just a) Just b
>           prefixed = maybe True (const False) m
>                      && i == "0" && take 1 os' == ["0"]
