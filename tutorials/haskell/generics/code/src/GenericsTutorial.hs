{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module GenericsTutorial
  ( CountFields (..) )
where

import GHC.Generics
import Numeric.Natural
import Prelude

class CountFields a where
  -- | Return number of constructor fields for a value.
  countFields :: a -> Natural
  default countFields :: (Generic a, CountFields1 (Rep a)) => a -> Natural
  countFields = defaultCountFields

defaultCountFields :: (Generic a, CountFields1 (Rep a)) => a -> Natural
defaultCountFields = countFields1 . from

class CountFields1 f where
  countFields1 :: f p -> Natural

instance CountFields1 V1 where
  countFields1 _ = 0

instance CountFields1 U1 where
  countFields1 _ = 0

instance CountFields1 (K1 i c) where
  countFields1 _ = 1

instance CountFields1 f => CountFields1 (M1 i c f) where
  countFields1 (M1 x) = countFields1 x

instance (CountFields1 a, CountFields1 b) => CountFields1 (a :+: b) where
  countFields1 (L1 x) = countFields1 x
  countFields1 (R1 x) = countFields1 x

instance (CountFields1 a, CountFields1 b) => CountFields1 (a :*: b) where
  countFields1 (a :*: b) = countFields1 a + countFields1 b

----------------------------------------------------------------------------
-- Tests

instance CountFields (Maybe a)

data Foo = Foo Int Int Int | Bar Bool
  deriving (Show, Generic)

instance CountFields Foo
