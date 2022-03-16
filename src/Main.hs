{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
module Main where
    
import Data.Kind (Type)
import Control.Algebra 
import Control.Carrier.State.Strict (State, get)
import Control.Carrier.Fail.Either

<<<<<<< HEAD
main :: IO ()
main = putStrLn "Hello, Haskell!"

data LogTarget a where
    NOP 
        :: LogTarget a
    VAR 
        :: Monoid a 
        => a 
        -> LogTarget a

data Logger a (m :: Type -> Type) k where
    Warning :: a -> Logger a m ()
    Error :: a -> Logger a m ()
    Info :: a -> Logger a m ()


warning 
    :: (Has (Logger a) sig m)
    => a -> m ()
warning = send . Warning

error
    :: (Has (Logger a) sig m)
    => a -> m ()
error = send . Error

info
    :: (Has (Logger a) sig m)
    => a -> m ()
info = send . Info

newtype LoggerC a m b = LoggerC { runLogger :: m b }
    deriving (Functor, Applicative, Monad)

instance (
    Has (State (LogTarget a)) sig m,
    Algebra sig m ) => Algebra (Logger a :+: sig) (LoggerC a m) where
        alg hdl sig ctx = case sig of 
            L action -> do
                target <- get
                case target of
                    NOP -> return ()
                    VAR x -> return ()
            R other -> LoggerC (alg (runLogger . hdl) other ctx)
=======
import Main.Utf8 (withUtf8)

main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  withUtf8 $ do
    putStrLn "Hello 🌎"
>>>>>>> parent of cef30e2 (added)
