{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
    Stolen from https://wiki.haskell.org/New_monads/MonadSupply
-}

module MonadSupply where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

import Text.Parsec

-- newtype SupplyT s m a = SupplyT (StateT [s] m a)
--     deriving (Applicative, Functor, Monad, MonadTrans, MonadIO)

-- newtype Supply s a = Supply (SupplyT s Identity a)
--     deriving (Applicative, Functor, Monad, MonadSupply s)



class Monad m => MonadSupply s m where
    supply :: m s

instance MonadSupply s Parser where
    supply = do (x:xs) <- get
                put xs
                return x



-- instance (Monad m, MonadFail m) => MonadSupply s (SupplyT s m) where
--     supply = SupplyT $ do
--                 (x:xs) <- get
--                 put xs
--                 return x


-- evalSupplyT (SupplyT s) supp = evalStateT s supp
-- evalSupply (Supply s) supp = evalSupplyT s supp

-- runSupplyT (SupplyT s) supp = runStateT s supp
-- runSupply (Supply s) supp = runSupplyT s supp