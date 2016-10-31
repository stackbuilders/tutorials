{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}

module Goaf
  ( inlining0
  , inlining1
  , special0'
  -- , special0
  , nofusion0
  , manuallyFused
  , manuallyFused'
  , fuseda
  , fused1
  , fused2
  )
where

import Data.List (unfoldr, uncons)
import GHC.Exts (build)
import Prelude hiding (foldr1)

inlining0 :: Int -> Int
inlining0 x =
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000]

inlining1 :: Int -> Int
inlining1 x =
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000]
{-# INLINEABLE inlining1 #-}

special0' :: (Num a, Enum a) => a -> a
special0' x =
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000]
{-# INLINEABLE special0' #-}

-- {-# SPECIALIZE special0' :: Int -> Int #-}

-- special0 :: Int -> Int
-- special0 x = special0' x `rem` 10

----------------------------------------------------------------------------
-- Demo of function composition without fusion

map0 :: (a -> b) -> [a] -> [b]
map0 _ []     = []
map0 f (x:xs) = f x : map0 f xs

foldr0 :: (a -> b -> b) -> b -> [a] -> b
foldr0 _ b []     = b
foldr0 f b (a:as) = foldr0 f (f a b) as

nofusion0 :: [Int] -> Int
nofusion0 = foldr0 (+) 0 . map0 sqr

sqr :: Int -> Int
sqr x = x * x

----------------------------------------------------------------------------
-- “Manually” fused version of nofusion0

manuallyFused :: [Int] -> Int
manuallyFused []     = 0
manuallyFused (x:xs) = x * x + manuallyFused xs

manuallyFused' :: [Int] -> Int
manuallyFused' = go 0
  where
    go !n []     = n
    go !n (x:xs) = go (n + x * x) xs

----------------------------------------------------------------------------
-- Explicit fusion (arrays)

data Array a = Array Int (Int -> a)

rangea :: Int -> Array Int
rangea n = Array n id

mapa :: (a -> b) -> Array a -> Array b
mapa f (Array size g) = Array size (f . g)

foldra :: (a -> b -> b) -> b -> Array a -> b
foldra f b (Array size g) = go 0 b
  where
    go n b' | n < size  = go (n + 1) (f (g n) b')
            | otherwise = b'

fuseda :: Int -> Int
fuseda = foldra (+) 0 . mapa sqr . rangea

----------------------------------------------------------------------------
-- Explicit fusion (lists)

data List a = forall s. List (s -> Maybe (a, s)) s

fromLinkedList :: [a] -> List a
fromLinkedList = List uncons

toLinkedList :: List a -> [a]
toLinkedList (List f s) = unfoldr f s

map1 :: (a -> b) -> List a -> List b
map1 g (List f s) = List h s
  where
    h s' = case f s' of
      Nothing       -> Nothing
      Just (x, s'') -> Just (g x, s'')

foldr1 :: (a -> b -> b) -> b -> List a -> b
foldr1 g b (List f s) = go b s
  where
    go b' s' = case f s' of
      Nothing       -> b'
      Just (x, s'') -> go (g x b') s''

fused1 :: [Int] -> Int
fused1 = foldr1 (+) 0 . map1 sqr . fromLinkedList

----------------------------------------------------------------------------
-- ‘build’/‘foldr’ fusion system in action

map2 :: (a -> b) -> [a] -> [b]
map2 _ []     = []
map2 f (x:xs) = f x : map2 f xs
{-# NOINLINE map2 #-}

{-# RULES
"map2"     [~1] forall f xs. map2 f xs               = build (\c n -> foldr2 (mapFB c f) n xs)
"map2List" [1]  forall f.    foldr2 (mapFB (:) f) [] = map2 f
"mapFB"    forall c f g.     mapFB (mapFB c f) g     = mapFB c (f . g)
  #-}

mapFB :: (b -> l -> l) -> (a -> b) -> a -> l -> l
mapFB c f = \x ys -> c (f x) ys
{-# INLINE [0] mapFB #-}

foldr2 :: (a -> b -> b) -> b -> [a] -> b
-- foldr2 _ b []     = b
-- foldr2 f b (a:as) = foldr2 f (f a b) as
foldr2 f z = go
  where
    go []     = z
    go (y:ys) = y `f` go ys
{-# INLINE [0] foldr2 #-}

{-# RULES
"build/foldr2" forall f z (g :: forall b. (a -> b -> b) -> b -> b). foldr2 f z (build g) = g f z
  #-}

fused2 :: [Int] -> Int
fused2 = foldr2 (+) 0 . map2 sqr
