{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Prelude ()
import Prelude.Compat

import Data.Char          (isSpace)
import Data.IORef         (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe         (fromMaybe)
import Data.Word          (Word32)
import System.Environment (getArgs)
import System.IO.Unsafe   (unsafePerformIO)

import qualified System.Random.SplitMix   as SM64
import qualified System.Random.SplitMix32 as SM32

-------------------------------------------------------------------------------
-- SplitMix32
-------------------------------------------------------------------------------

sm32ref :: IORef SM32.SMGen
sm32ref = unsafePerformIO $ newIORef $ SM32.mkSMGen 42
{-# NOINLINE sm32ref #-}

foreign export ccall haskell_splitmix32 :: IO Word32
foreign export ccall haskell_splitmix32_double :: IO Double

haskell_splitmix32 :: IO Word32
haskell_splitmix32 = do
    g <- readIORef sm32ref
    let !(w32, g') = SM32.nextWord32 g
    writeIORef sm32ref g'
    return w32

haskell_splitmix32_double :: IO Double
haskell_splitmix32_double = do
    g <- readIORef sm32ref
    let !(d, g') = SM32.nextDouble g
    writeIORef sm32ref g'
    return d

-------------------------------------------------------------------------------
-- SplitMix64
-------------------------------------------------------------------------------

sm64ref :: IORef SM64.SMGen
sm64ref = unsafePerformIO $ newIORef $ SM64.mkSMGen 42
{-# NOINLINE sm64ref #-}

foreign export ccall haskell_splitmix64 :: IO Word32
foreign export ccall haskell_splitmix64_double :: IO Double

haskell_splitmix64 :: IO Word32
haskell_splitmix64 = do
    g <- readIORef sm64ref
    let !(w32, g') = SM64.nextWord32 g
    writeIORef sm64ref g'
    return w32

haskell_splitmix64_double :: IO Double
haskell_splitmix64_double = do
    g <- readIORef sm64ref
    let !(d, g') = SM64.nextDouble g
    writeIORef sm64ref g'
    return d

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

foreign import ccall "run_testu01" run_testu01_c :: Int -> Int -> IO ()

main :: IO ()
main = do
    args <- getArgs
    (gen, bat) <- parseArgsIO args $ (,)
        <$> optDef "-g" SplitMix
        <*> optDef "-b" SmallCrush
    run_testu01_c (fromEnum gen) (fromEnum bat)

data Gen
    = SplitMixDouble
    | SplitMix
    | SplitMix32Double
    | SplitMix32
    | SplitMix32Native
  deriving (Read, Enum)

data Bat
    = SmallCrush
    | Crush
    | BigCrush
    | Sample
  deriving (Read, Enum)

-------------------------------------------------------------------------------
-- readMaybe
-------------------------------------------------------------------------------

readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,rest) <- reads s, all isSpace rest ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"

readMaybe :: Read a => String -> Maybe a
readMaybe s = case readEither s of
                Left _  -> Nothing
                Right a -> Just a

-------------------------------------------------------------------------------
-- Do it yourself command line parsing
-------------------------------------------------------------------------------

-- | 'Parser' is not an 'Alternative', only a *commutative* 'Applicative'.
--
-- Useful for quick cli parsers, like parametrising tests.
data Parser a where
    Pure :: a -> Parser a
    Ap :: Arg b -> Parser (b -> a) -> Parser a

instance Functor Parser where
    fmap f (Pure a) = Pure (f a)
    fmap f (Ap x y) = Ap x (fmap (f .) y)

instance  Applicative Parser where
    pure = Pure

    Pure f <*> z = fmap f z
    Ap x y <*> z = Ap x (flip <$> y <*> z)

data Arg a where
    Flag :: String -> Arg Bool
    Opt  :: String -> (String -> Maybe a) -> Arg (Maybe a)
    Arg  :: Arg String

-- arg :: Parser String
-- arg = Ap Arg (Pure id)
--
-- flag :: String -> Parser Bool
-- flag n = Ap (Flag n) (Pure id)
--
-- opt :: Read a => String -> Parser (Maybe a)
-- opt n = Ap (Opt n readMaybe) (Pure id)

optDef :: Read a => String -> a -> Parser a
optDef n d = Ap (Opt n readMaybe) (Pure (fromMaybe d))

parseArgsIO :: [String] -> Parser a -> IO a
parseArgsIO args p = either fail pure (parseArgs args p)

parseArgs :: [String] -> Parser a -> Either String a
parseArgs []       p = parserToEither p
parseArgs (x : xs) p = do
    (xs', p') <- singleArg p x xs
    parseArgs xs' p'

singleArg :: Parser a -> String -> [String] -> Either String ([String], Parser a)
singleArg (Pure _)           x _  = Left $ "Extra argument " ++ x
singleArg (Ap Arg p)         x xs
    | null x || head x /= '-'     = Right (xs, fmap ($ x) p)
    | otherwise                   = fmap2 (Ap Arg) (singleArg p x xs)
singleArg (Ap f@(Flag n) p)  x xs
    | x == n                      = Right (xs, fmap ($ True) p)
    | otherwise                   = fmap2 (Ap f) (singleArg p x xs)
singleArg (Ap o@(Opt n r) p) x xs
    | x == n                      = case xs of
        [] -> Left $ "Expected an argument for " ++ n
        (x' : xs') -> case r x' of
            Nothing -> Left $ "Cannot read an argument of " ++ n ++ ": " ++ x'
            Just y  -> Right (xs', fmap ($ Just y) p)
    | otherwise                   = fmap2 (Ap o) (singleArg p x xs)

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

-- | Convert parser to 'Right' if there are only defaultable pieces left.
parserToEither :: Parser a -> Either String a
parserToEither (Pure x)         = pure x
parserToEither (Ap (Flag _) p)  = parserToEither $ fmap ($ False) p
parserToEither (Ap (Opt _ _) p) = parserToEither $ fmap ($ Nothing) p
parserToEither (Ap Arg _)       = Left "argument required"
