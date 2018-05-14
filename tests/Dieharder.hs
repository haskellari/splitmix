{-# LANGUAGE GADTs #-}
module Main (main) where

import Prelude ()
import Prelude.Compat

import Control.Concurrent.QSem
import Control.DeepSeq         (force)
import Data.List               (isInfixOf, unfoldr)
import Data.Maybe              (fromMaybe)
import Data.Traversable        (for)
import Data.Word               (Word64)
import Foreign.C               (Errno (..), ePIPE)
import GHC.IO.Exception        (IOErrorType (..), IOException (..))
import System.Environment      (getArgs)
import System.IO               (Handle, hGetContents)
import Text.Read               (readMaybe)

import qualified Control.Concurrent.Async as A

import qualified Control.Exception       as E
import qualified Data.ByteString.Builder as B
import qualified System.Process          as Proc
import qualified System.Random.SplitMix  as SM
import qualified System.Random.TF        as TF
import qualified System.Random.TF.Gen    as TF
import qualified System.Random.TF.Init   as TF

main :: IO ()
main = do
    args <- getArgs
    if null args
    then return ()
    else do
        (cmd, runs, conc, seed, test, _help) <- parseArgsIO args $ (,,,,,)
            <$> arg
            <*> optDef "-n" 1
            <*> optDef "-j" 1
            <*> opt "-s"
            <*> opt "-d"
            <*> flag "-h"

        let mgen = case cmd of
              "splitmix"      -> Just $ splitmix seed
              "tfrandom"      -> Just tfrandom
              "splitmix-tree" -> Just splitmixTree
              _               -> Nothing

        qsem <- newQSem conc

        res <- for mgen $ \gen -> do
            rs <- A.forConcurrently (replicate runs ()) $ \() ->
                E.bracket_ (waitQSem qsem) (signalQSem qsem) $
                    dieharder test gen
            return $ mconcat rs

        print res

dieharder :: Maybe Int -> (Handle -> IO ()) -> IO Result
dieharder test gen = do
    let proc = Proc.proc "dieharder" $ ["-g", "200"] ++ maybe ["-a"] (\t -> ["-d", show t]) test
    (Just hin, Just hout, _, ph) <- Proc.createProcess proc
        { Proc.std_in  = Proc.CreatePipe
        , Proc.std_out = Proc.CreatePipe
        }

    out <- hGetContents hout
    waitOut <- A.async $ E.evaluate $ force out

    E.catch (gen hin) $ \e -> case e of
        IOError { ioe_type = ResourceVanished , ioe_errno = Just ioe }
            | Errno ioe == ePIPE -> return ()
        _ -> E.throwIO e

    res <- A.wait waitOut
    _ <- Proc.waitForProcess ph

    return $ parseOutput res

parseOutput :: String -> Result
parseOutput = foldMap parseLine . lines where
    parseLine l
        | any (`isInfixOf` l) doNotUse = mempty
        | "PASSED" `isInfixOf` l = Result 1 0 0
        | "WEAK"   `isInfixOf` l = Result 0 1 0
        | "FAILED" `isInfixOf` l = Result 0 1 0
        | otherwise = mempty

    doNotUse = ["diehard_opso", "diehard_oqso", "diehard_dna", "diehard_weak"]

data Result = Result
    { _passed :: Int
    , _weak   :: Int
    , _failed :: Int
    }
  deriving Show

instance Semigroup Result where
    Result p w f <> Result p' w' f' = Result (p + p') (w +  w') (f + f')

instance Monoid Result where
    mempty = Result 0 0 0
    mappend = (<>)

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

splitmix :: Maybe Word64 -> Handle -> IO ()
splitmix seed h = smgen >>= B.hPutBuilder h . go
  where
    smgen = maybe SM.initSMGen (return . SM.mkSMGen) seed

    go :: SM.SMGen -> B.Builder
    go g = case SM.nextWord64 g of
        ~(w64, g') -> B.word64LE w64 `mappend` go g'

splitmixTree :: Handle -> IO ()
splitmixTree h = SM.initSMGen >>= B.hPutBuilder h . go
  where
    go :: SM.SMGen -> B.Builder
    go g = case SM.splitSMGen g of
        ~(ga, gb) -> builder 8 ga `mappend` go gb

    builder :: Int -> SM.SMGen -> B.Builder
    builder n = mconcat . take n . map B.word64LE . unfoldr (Just . SM.nextWord64)

tfrandom :: Handle -> IO ()
tfrandom h = TF.initTFGen >>= B.hPutBuilder h . go
  where
    go :: TF.TFGen -> B.Builder
    go g = case TF.next g of
        ~(w32, g') -> B.word32LE w32 `mappend` go g'

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

arg :: Parser String
arg = Ap Arg (Pure id)

flag :: String -> Parser Bool
flag n = Ap (Flag n) (Pure id)

opt :: Read a => String -> Parser (Maybe a)
opt n = Ap (Opt n readMaybe) (Pure id)

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
