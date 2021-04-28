{-# LANGUAGE TypeApplications #-}

-- | Util(s)
module Util
  ( show',
    ShowModifiers (..),
    ShowOptions (..),
    CustomShow (..),
    module GHC.Generics,
  )
where

--------------------------------------------------

import Data.Char (toLower, toUpper)
import Data.Foldable (foldl')
import Data.Proxy
import Data.String (IsString (..))
import GHC.Generics
import GHC.TypeLits

--------------------------------------------------

show' :: (IsString str, Show a) => a -> str
show' = fromString . show
{-# INLINE show' #-}

--------------------------------------------------

-- Derivig Via Show Instances.

-- TODO mutually exclusive e.g. LowerCase should never be in the same list as UpperCase
data ShowModifiers
  = LowerCase
  | UpperCase

class ShowOptions t where
  showOptions :: [ShowModifiers]

instance ShowOptions 'LowerCase where showOptions = [LowerCase]

instance ShowOptions 'UpperCase where showOptions = [UpperCase]

instance ShowOptions '[] where showOptions = []

instance
  (ShowOptions t, ShowOptions ts) =>
  ShowOptions (t ': ts)
  where
  showOptions = showOptions @t ++ showOptions @ts

newtype CustomShow t a = CustomShow {unCustomShow :: a}

instance (ShowOptions t, Generic a, GShow (Rep a)) => Show (CustomShow t a) where
  show (CustomShow a) = foldl' (flip alg) (gshow (from a)) (showOptions @t)
    where
      alg LowerCase = fmap toLower
      alg UpperCase = fmap toUpper
  {-# INLINE show #-}

class GShow a where
  {-# MINIMAL gshow #-}
  gshow :: a x -> String

instance GShow U1 where
  gshow _ = ""
  {-# INLINE gshow #-}

instance GShow V1 where
  gshow _ = ""
  {-# INLINE gshow #-}

instance {-# OVERLAPPING #-} GShow (K1 _1 String) where
  gshow (K1 a) = a
  {-# INLINE gshow #-}

instance (Show a) => GShow (K1 _1 a) where
  gshow (K1 a) = show a
  {-# INLINE gshow #-}

instance (GShow a, GShow b) => GShow (a :+: b) where
  gshow (L1 a) = gshow a
  gshow (R1 a) = gshow a
  {-# INLINE gshow #-}

instance (GShow a, GShow b) => GShow (a :*: b) where
  gshow (a1 :*: a2) = " " <> gshow a1 <> " " <> gshow a2
  {-# INLINE gshow #-}

instance {-# OVERLAPPING #-} (KnownSymbol nm, GShow a) => GShow (M1 C ('MetaCons nm _1 _2) a) where
  gshow (M1 a) = symbolVal (Proxy @nm) <> " " <> gshow a
  {-# INLINE gshow #-}

instance GShow a => GShow (M1 _1 _2 a) where
  gshow (M1 a) = gshow a
  {-# INLINE gshow #-}
