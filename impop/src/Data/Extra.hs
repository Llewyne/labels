{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Extra 
  ( module Data.Extra
  , module Data.Ext
  ) where

import Control.Lens
import Data.Proxy
import Data.Default
import Data.Ext hiding (extra)
import qualified Data.Ext (extra)


class HasExtra a b where
  extra :: Lens' a b

instance HasExtra (a :+ b) b where  
  extra = Data.Ext.extra

instance {-# OVERLAPS #-} HasExtra a c => HasExtra (a :+ b) c where
  extra = core . extra

instance (Default a, Default b) => Default (a :+ b) where
  def = def :+ def



{- ALTERNATIVE IMPLEMENTATION WITHOUT OVERLAPPING INSTANCES

type family (Equal a b) :: Bool where
  Equal a a = 'True
  Equal a b = 'False

class HasExtra a b where
  extra :: Lens' a b

instance (Equal b c ~ flag, HasExtra' flag (a :+ b) c) => HasExtra (a :+ b) c where
  extra = extra' (Proxy :: Proxy flag)

class HasExtra' (flag :: Bool) a b where
  extra' :: Proxy flag -> Lens' a b

instance HasExtra' 'True (a :+ b) b where  
  extra' _ = Data.Ext.extra

instance HasExtra a c => HasExtra' 'False (a :+ b) c where
  extra' _ = core . extra

-}