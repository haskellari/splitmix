-- |
-- /SplitMix/ is a splittable pseudorandom number generator (PRNG) that is quite fast.
--
-- Guy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014.  Fast splittable pseudorandom number generators. /In Proceedings of the 2014 ACM International Conference on Object Oriented Programming Systems Languages & Applications/ (OOPSLA '13). ACM, New York, NY, USA, 453-472. DOI: <https://doi.org/10.1145/2660193.2660195>
--
-- Guy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014.
--  Fast splittable pseudorandom number generators. In Proceedings
--  of the 2014 ACM International Conference on Object Oriented
--  Programming Systems Languages & Applications (OOPSLA '14). ACM,
--  New York, NY, USA, 453-472. DOI:
--  <https://doi.org/10.1145/2660193.2660195>
--
--  The paper describes a new algorithm /SplitMix/ for /splittable/
--  pseudorandom number generator that is quite fast: 9 64 bit arithmetic/logical
--  operations per 64 bits generated.
--
--  /SplitMix/ is tested with two standard statistical test suites (DieHarder and
--  TestU01, this implementation only using the former) and it appears to be
--  adequate for "everyday" use, such as Monte Carlo algorithms and randomized
--  data structures where speed is important.
--
--  In particular, it __should not be used for cryptographic or security applications__,
--  because generated sequences of pseudorandom values are too predictable
--  (the mixing functions are easily inverted, and two successive outputs
--  suffice to reconstruct the internal state).
--
--  Note: This module supports all GHCs since GHC-7.0.4,
--  but GHC-7.0 and GHC-7.2 have slow implementation, as there
--  are no native 'popCount'.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module System.Random.SplitMix (
    SMGen,
    nextWord64,
    nextInt,
    nextDouble,
    splitSMGen,
    -- * Initialisation
    mkSMGen,
    initSMGen,
    newSMGen,
    seedSMGen,
    seedSMGen',
    unseedSMGen,
    ) where

import Control.DeepSeq       (NFData (..))
import Data.Bits             (shiftL, shiftR, xor, (.|.))
import Data.IORef            (IORef, atomicModifyIORef, newIORef)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word             (Word32, Word64)
import System.CPUTime        (cpuTimePrecision, getCPUTime)
import System.IO.Unsafe      (unsafePerformIO)

import qualified System.Random as R

#if MIN_VERSION_base(4,5,0)
import Data.Bits             (popCount)
#else
import Data.Bits             ((.&.))
popCount :: Word64 -> Int
popCount = go 0
 where
   go !c 0 = c
   go c w = go (c+1) (w .&. (w - 1)) -- clear the least significant
#endif

-- $setup
-- >>> import Text.Read (readMaybe)
-- >>> import Data.List (unfoldr)
-- >>> import Text.Printf (printf)

-------------------------------------------------------------------------------
-- Generator
-------------------------------------------------------------------------------

-- | SplitMix generator state.
data SMGen = SMGen !Word64 !Word64 -- seed and gamma; gamma is odd
  deriving Show

instance NFData SMGen where
    rnf (SMGen _ _) = ()

-- |
--
-- >>> readMaybe "SMGen 1 1" :: Maybe SMGen
-- Just (SMGen 1 1)
--
-- >>> readMaybe "SMGen 1 2" :: Maybe SMGen
-- Nothing
--
-- >>> readMaybe (show (mkSMGen 42)) :: Maybe SMGen
-- Just (SMGen 9297814886316923340 13679457532755275413)
--
instance Read SMGen where
    readsPrec d r =  readParen (d > 10) (\r0 ->
      [ (SMGen seed gamma, r3)
      | ("SMGen", r1) <- lex r0
      , (seed, r2) <- readsPrec 11 r1
      , (gamma, r3) <- readsPrec 11 r2
      , odd gamma
      ]) r

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Generate a 'Word64'.
--
-- >>> take 3 $ map (printf "%x") $ unfoldr (Just . nextWord64) (mkSMGen 1337) :: [String]
-- ["b5c19e300e8b07b3","d600e0e216c0ac76","c54efc3b3cc5af29"]
--
nextWord64 :: SMGen -> (Word64, SMGen)
nextWord64 (SMGen seed gamma) = (mix64 seed', SMGen seed' gamma)
  where
    seed' = seed + gamma

-- | Generate an 'Int'.
nextInt :: SMGen -> (Int, SMGen)
nextInt g = case nextWord64 g of
    (w64, g') -> (fromIntegral w64, g')

-- | Generate a 'Double' in @[0, 1)@ range.
--
-- >>> take 8 $ map (printf "%0.3f") $ unfoldr (Just . nextDouble) (mkSMGen 1337) :: [String]
-- ["0.710","0.836","0.771","0.409","0.297","0.527","0.589","0.067"]
--
nextDouble :: SMGen -> (Double, SMGen)
nextDouble g = case nextWord64 g of
    (w64, g') -> (fromIntegral (w64 `shiftR` 11) * doubleUlp, g')

-- | Split a generator into a two uncorrelated generators.
splitSMGen :: SMGen -> (SMGen, SMGen)
splitSMGen (SMGen seed gamma) =
    (SMGen seed'' gamma, SMGen (mix64 seed') (mixGamma seed''))
  where
    seed'  = seed + gamma
    seed'' = seed' + gamma

-------------------------------------------------------------------------------
-- Algorithm
-------------------------------------------------------------------------------

goldenGamma :: Word64
goldenGamma = 0x9e3779b97f4a7c15

doubleUlp :: Double
doubleUlp =  1.0 / fromIntegral (1 `shiftL` 53 :: Word64)

-- Note: in JDK implementations the mix64 and mix64variant13
-- (which is inlined into mixGamma) are swapped.
--
-- I have no idea if swapping them affects statistical properties.
mix64 :: Word64 -> Word64
mix64 z0 =
   -- MurmurHash3Mixer
    let z1 = shiftXorMultiply 33 0xff51afd7ed558ccd z0
        z2 = shiftXorMultiply 33 0xc4ceb9fe1a85ec53 z1
        z3 = shiftXor 33 z2
    in z3

-- used only in mixGamma
mix64variant13 :: Word64 -> Word64
mix64variant13 z0 =
   -- Better Bit Mixing - Improving on MurmurHash3's 64-bit Finalizer
   -- http://zimbry.blogspot.fi/2011/09/better-bit-mixing-improving-on.html
    let z1 = shiftXorMultiply 30 0xbf58476d1ce4e5b9 z0 -- MurmurHash3 mix constants
        z2 = shiftXorMultiply 27 0x94d049bb133111eb z1
        z3 = shiftXor 31 z2
    in z3

mixGamma :: Word64 -> Word64
mixGamma z0 =
    let z1 = mix64variant13 z0 .|. 1             -- force to be odd
        n  = popCount (z1 `xor` (z1 `shiftR` 1))
    -- see: http://www.pcg-random.org/posts/bugs-in-splitmix.html
    -- let's trust the text of the paper, not the code.
    in if n >= 24
        then z1
        else z1 `xor` 0xaaaaaaaaaaaaaaaa

shiftXor :: Int -> Word64 -> Word64
shiftXor n w = w `xor` (w `shiftR` n)

shiftXorMultiply :: Int -> Word64 -> Word64 -> Word64
shiftXorMultiply n k w = shiftXor n w * k

-------------------------------------------------------------------------------
-- Initialisation
-------------------------------------------------------------------------------

-- | Create 'SMGen' using seed and gamma.
--
-- >>> seedSMGen 2 2
-- SMGen 2 3
--
seedSMGen
    :: Word64 -- ^ seed
    -> Word64 -- ^ gamma
    -> SMGen
seedSMGen seed gamma = SMGen seed (gamma .|. 1)

-- | Like 'seedSMGen' but takes a pair.
seedSMGen' :: (Word64, Word64) -> SMGen
seedSMGen' = uncurry seedSMGen

-- | Extract current state of 'SMGen'.
unseedSMGen :: SMGen -> (Word64, Word64)
unseedSMGen (SMGen seed gamma) = (seed, gamma)

-- | Preferred way to deterministically construct 'SMGen'.
--
-- >>> mkSMGen 42
-- SMGen 9297814886316923340 13679457532755275413
--
mkSMGen :: Word64 -> SMGen
mkSMGen s = SMGen (mix64 s) (mixGamma (s + goldenGamma))

-- | Initialize 'SMGen' using system time.
initSMGen :: IO SMGen
initSMGen = fmap mkSMGen mkSeedTime

-- | Derive a new generator instance from the global 'SMGen' using 'splitSMGen'.
newSMGen :: IO SMGen
newSMGen = atomicModifyIORef theSMGen splitSMGen

theSMGen :: IORef SMGen
theSMGen = unsafePerformIO $ initSMGen >>= newIORef
{-# NOINLINE theSMGen #-}

mkSeedTime :: IO Word64
mkSeedTime = do
    now <- getPOSIXTime
    cpu <- getCPUTime
    let lo = truncate now :: Word32
        hi = fromIntegral (cpu `div` cpuTimePrecision) :: Word32
    return $ fromIntegral hi `shiftL` 32 .|. fromIntegral lo

-------------------------------------------------------------------------------
-- System.Random
-------------------------------------------------------------------------------

instance R.RandomGen SMGen where
    next = nextInt
    split = splitSMGen
