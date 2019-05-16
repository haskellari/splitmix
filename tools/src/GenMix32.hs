{-# LANGUAGE RankNTypes #-}
-- cabal v2-build generate-mix32 && $(cabal-plan list-bin generate-mix32)
module Main (main) where

import Prelude ()
import Prelude.Compat

import Control.Applicative       ((<|>))
import Control.Monad             (ap, liftM)
import Control.Monad.Trans.State (State, evalState, get, put)
import Data.Bits                 (shiftL, shiftR, xor, (.&.))
import Data.Foldable             (toList)
import Data.Maybe (isNothing)
import Data.Functor.Classes
import Data.Int                  (Int32)
import Data.Traversable          (foldMapDefault)
import Data.Word                 (Word32)
import Text.Printf               (printf)

-------------------------------------------------------------------------------
-- Expr
-------------------------------------------------------------------------------

data Expr a
    -- Combinators we use in the expression
    = V a
    | K Word32
    | ShiftR (Expr a) Int
    | Xor (Expr a) (Expr a)
    | Mult (Expr a) (Expr a)

    -- Other combinators
    | ShiftL (Expr a) Int
    | Plus (Expr a) (Expr a)

    | Trunc16 (Expr a)
    | Trunc32 (Expr a)

    | Let (Expr a) (Expr (Maybe a))

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Foldable Expr where
    foldMap = foldMapDefault

instance Traversable Expr where
    traverse f (V a) = V <$> f a
    traverse _ (K x) = pure (K x)

    traverse f (ShiftR x n) = ShiftR <$> traverse f x <*> pure n
    traverse f (Xor x y)    = Xor <$> traverse f x <*> traverse f y
    traverse f (Mult x y)   = Mult <$> traverse f x <*> traverse f y

    traverse f (ShiftL x n) = ShiftR <$> traverse f x <*> pure n
    traverse f (Plus x y)   = Plus <$> traverse f x <*> traverse f y

    traverse f (Trunc16 x)   = Trunc16 <$> traverse f x
    traverse f (Trunc32 x)   = Trunc32 <$> traverse f x

    traverse f (Let x y) = Let <$> traverse f x <*> traverse (traverse f) y

instance Eq1 Expr where
    liftEq eq (V x) (V x') = eq x x'
    liftEq _  (K x) (K x') = x == x'

    liftEq eq (ShiftR x y) (ShiftR x' y') = liftEq eq x x' && y == y'
    liftEq eq (Xor x y)    (Xor    x' y') = liftEq eq x x' && liftEq eq y y'
    liftEq eq (Mult x y)   (Mult   x' y') = liftEq eq x x' && liftEq eq y y'

    liftEq eq (ShiftL x y) (ShiftL x' y') = liftEq eq x x' && y == y'
    liftEq eq (Plus x y)   (Plus   x' y') = liftEq eq x x' && liftEq eq y y'

    liftEq eq (Trunc16 x) (Trunc16 x') = liftEq eq x x'
    liftEq eq (Trunc32 x) (Trunc32 x') = liftEq eq x x'

    liftEq eq (Let x y) (Let x' y') = liftEq eq x x' && liftEq (liftEq eq) y y'

    liftEq _ _ _ = False

instance Eq a => Eq (Expr a) where
    (==) = eq1

instance Show1 Expr where
    liftShowsPrec sp _ d (V x) = showsUnaryWith
        sp
        "V" d x
    liftShowsPrec _ _ d (K x) = showsUnaryWith
        showsPrec
        "K" d x

    liftShowsPrec sp sl d (ShiftR x y) = showsBinaryWith
        (liftShowsPrec sp sl)
        showsPrec
        "ShiftR" d x y
    liftShowsPrec sp sl d (Xor x y) = showsBinaryWith
        (liftShowsPrec sp sl)
        (liftShowsPrec sp sl)
        "Xor" d x y
    liftShowsPrec sp sl d (Mult x y) = showsBinaryWith
        (liftShowsPrec sp sl)
        (liftShowsPrec sp sl)
        "Mult" d x y

    liftShowsPrec sp sl d (ShiftL x y) = showsBinaryWith
        (liftShowsPrec sp sl)
        showsPrec
        "ShiftL" d x y
    liftShowsPrec sp sl d (Plus x y) = showsBinaryWith
        (liftShowsPrec sp sl)
        (liftShowsPrec sp sl)
        "Plus" d x y

    liftShowsPrec sp sl d (Trunc16 x) = showsUnaryWith
        (liftShowsPrec sp sl)
        "Trunc16" d x

    liftShowsPrec sp sl d (Trunc32 x) = showsUnaryWith
        (liftShowsPrec sp sl)
        "Trunc32" d x

    liftShowsPrec sp sl d (Let x y) = showsBinaryWith
        (liftShowsPrec sp sl)
        (liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl))
        "Let" d x y

instance Show a => Show (Expr a) where
    showsPrec = showsPrec1

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "mix32"
    main' (mix32 "$1")
    putStrLn ""
    putStrLn "mix32variant13"
    main' (mix32variant13 "$1")
  where
    main' e =  do
        putStrLn "=============================================="
        let v = eval (const 42) e
        print v
        print (fromIntegral v :: Int32)
        putStrLn $ pretty $ "42" <$ e

        let e1 = compile e
        let v1 = eval (const 42) e1
        print v1
        print (fromIntegral v1 :: Int32)
        putStrLn $ pretty $ "42" <$ e1

        let e2 = optimise e1
        let v2 = eval (const 42) e2
        print v2
        print (fromIntegral v2 :: Int32)
        print e2
        putStrLn "=============================================="
        putStrLn $ pretty $ "42" <$ e2
        putStrLn "----------------------------------------------"
        putStrLn $ pretty $ "$1" <$ e2
        putStrLn "=============================================="

-------------------------------------------------------------------------------
-- Mixer helpers
-------------------------------------------------------------------------------

shiftXor :: Int -> Expr ctx -> Expr ctx
shiftXor n w =
    Let w $ V Nothing `Xor` (V Nothing `ShiftR` n)

shiftXorMultiply :: Int -> Expr ctx -> Expr ctx -> Expr ctx
shiftXorMultiply n k w = shiftXor n w `Mult` k

-------------------------------------------------------------------------------
-- Mixers
-------------------------------------------------------------------------------

mix32 :: a -> Expr a
mix32 z0 =
    let z1 = shiftXorMultiply 16 (K 0x85ebca6b) (V z0)
        z2 = shiftXorMultiply 13 (K 0xc2b2ae35) z1
        z3 = shiftXor 16 z2
    in z3

mix32variant13 :: a -> Expr a
mix32variant13 z0 =
   -- See avalanche "executable"
    let z1 = shiftXorMultiply 16 (K 0x69ad6ccb) (V z0)
        z2 = shiftXorMultiply 13 (K 0xcd9ab5b3) z1
        z3 = shiftXor 16 z2
    in z3

-------------------------------------------------------------------------------
-- Expr stuff
-------------------------------------------------------------------------------

instance Functor Expr where
    fmap = liftM

instance Applicative Expr where
    pure  = return
    (<*>) = ap

instance Monad Expr where
    return = V

    V a        >>= k = k a
    K w        >>= _ = K w
    ShiftR x n >>= k = ShiftR (x >>= k) n
    Xor x y    >>= k = Xor (x >>= k) (y >>= k)
    Mult x y   >>= k = Mult (x >>= k) (y >>= k)

    ShiftL x n >>= k = ShiftL (x >>= k) n
    Plus x y   >>= k = Plus (x >>= k) (y >>= k)
    Trunc16 x  >>= k = Trunc16 (x >>= k)
    Trunc32 x  >>= k = Trunc32 (x >>= k)

    Let e v    >>= k = Let (e >>= k) (v >>== k)

(>>==) :: Expr (Maybe a) -> (a -> Expr b) -> Expr (Maybe b)
e >>== k = e >>= maybe (V Nothing) (lift . k)

instantiate :: Expr a -> Expr (Maybe a) -> Expr a
instantiate v e = e >>= maybe v V

lift :: Expr a -> Expr (Maybe a)
lift = fmap Just

-------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

eval :: (a -> Word32) -> Expr a -> Word32
eval ctx = go where
    go (V a)        = ctx a
    go (K w)        = w
    go (ShiftR x n) = go x `shiftR` n
    go (Xor x y)    = go x `xor` go y
    go (Mult x y)   = go x * go y

    go (ShiftL x n) = go x `shiftL` n
    go (Plus x y)   = go x + go y
    go (Trunc16 x)  = go x .&. 0xffff
    go (Trunc32 x)  = go x .&. 0xffffffff

    go (Let e f)    = go $ instantiate e f

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

data S = String :< S

names :: S
names = go (0 :: Int)
  where
    go n = ("x" ++ show n) :< go (succ n)

pretty :: Expr String -> String
pretty e = evalState (final e) names where
    -- JavaScript precedence
    -- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
    final :: Expr String -> State S String
    final x = do
        x' <- go 8 x
        return $ x' ++ " | 0"

    go :: Int -> Expr String -> State S String
    go _ (V n)        = return n
    go _ (K w)        = return $ printf "0x%08x" w
    go d (ShiftR x n) = parens (d > 12) $ do
        x' <- go 13 x
        return $ x' ++ " >>> " ++ show n
    go d (Xor x y) = parens (d > 8) $ do
        x' <- go 9 x
        y' <- go 8 y
        return $ x' ++ " ^ " ++ y'
    go d (Mult x y) = parens (d > 14) $ do
        x' <- go 15 x
        y' <- go 14 y
        return $ x' ++ " * " ++ y'

    go d (ShiftL x n) = parens (d > 12) $ do
        x' <- go 13 x
        return $ x' ++ " << " ++ show n
    go d (Plus x y) = parens (d > 13) $ do
        x' <- go 14 x
        y' <- go 13 y
        return $ x' ++ " + " ++ y'
    go d (Trunc16 x)  = parens (d > 9) $ do
        x' <- go 10 x
        return $ x' ++ " & 0xffff"
    go d (Trunc32 x)  = parens (d > 9) $ do
        x' <- go 10 x
        return $ x' ++ " & 0xffffffff"

    go d (Let x y) = parens (d > 1) $ do
        n :< ns <- get
        put ns
        x' <- go 2 x
        y' <- go 1 $ instantiate (V n) y
        return $ n ++ " = " ++ x' ++ " , " ++ y'

parens :: Functor f => Bool -> f String -> f String
parens False = id
parens True  = fmap $ \x -> "(" ++ x ++ ")"

-------------------------------------------------------------------------------
-- Compile
-------------------------------------------------------------------------------

compile :: Expr a -> Expr a
compile (V a)        = V a
compile (K w)        = K w
compile (ShiftR e n) = ShiftR (compile e) n
compile (Xor x y)    = Xor (compile x) (compile y)
compile (Mult xy uv)
    = Let (compile xy)
    $ Let (Trunc16 $ ShiftR (V Nothing) 16)
    $ Let (Trunc16 $ V $ Just Nothing)
    $ Let (lift $ lift $ lift $ compile uv)
    $ Let (Trunc16 $ ShiftR (V Nothing) 16)
    $ Let (Trunc16 $ V $ Just Nothing)
    $ ShiftL (Trunc16 $ Mult x v `Plus` Mult y u) 16 `Plus` Mult y v
  where
    x = V $ Just $ Just $ Just $ Just Nothing
    y = V $ Just $ Just $ Just Nothing
    u = V $ Just Nothing
    v = V Nothing

compile (Let x n) = Let (compile x) (compile n)

compile e = e

-------------------------------------------------------------------------------
-- Optimise
-------------------------------------------------------------------------------

optimise :: Eq a => Expr a -> Expr a
optimise = go (10 :: Int) where
    go n x | n < 0     = x
           | otherwise = opt (go (pred n) x)

    opt = rew -- . cse
    rew = rewrite $ \x -> inline x <|> constFold x <|> letFloat x -- <|> push x

-------------------------------------------------------------------------------
-- Rewrites
-------------------------------------------------------------------------------

rewrite :: (forall x. Expr x -> Maybe (Expr x)) -> Expr a -> Expr a
rewrite f = go
  where
    g :: forall y. Expr y -> Expr y
    g x = maybe x g (f x)

    go :: forall y. Expr y -> Expr y
    go e@(V _)      = g e
    go e@(K _)      = g e
    go (ShiftR x n) = g (ShiftR (go x) n)
    go (Xor x y)    = g (Xor (go x) (go y))
    go (Mult x y)   = g (Mult (go x) (go y))

    go (ShiftL x n) = g (ShiftL (go x) n)
    go (Plus x y)   = g (Plus (go x) (go y))

    go (Trunc16 x) = g (Trunc16 (go x))
    go (Trunc32 x) = g (Trunc32 (go x))

    go (Let x y) = g (Let (go x) (go y))

constFold :: Expr a -> Maybe (Expr a)
constFold (ShiftR (K w32) n) = Just (K (w32 `shiftR` n))
constFold (Trunc16 (K w32)) = Just (K (w32 .&. 0xffff))
constFold _ = Nothing

inline :: Expr a -> Maybe (Expr a)
inline (Let (K w32) e) = Just (instantiate (K w32) e)  -- let x = 42 in ...
inline (Let (V n)   e) = Just (instantiate (V n)   e)  -- let x = y  in ...
inline (Let x e) = case bindings of
    []  -> traverse (const Nothing) e -- unused binding
    [_] -> Just $ instantiate x e     -- single use
    _   -> Nothing
  where
    bindings = filter isNothing $ toList e
inline _ = Nothing

-- let n = (let m = x in y) in z
-- -----------------------------
-- let m = x in let n = y in z
--
letFloat :: Expr a -> Maybe (Expr a)
letFloat (Let (Let x y) z) = Just
     $ Let x
     $ Let y $ fmap (fmap Just) z
letFloat _ = Nothing


_push :: Expr a -> Maybe (Expr a)
_push = push

push :: Expr a -> Maybe (Expr a)
push (Trunc16 (Let x y))  = Just $ Let x (Trunc16 y)
push (ShiftR (Let x y) n) = Just $ Let x (ShiftR y n)
push (ShiftL (Let x y) n) = Just $ Let x (ShiftL y n)

push (Mult (Let x y) z) = Just $ Let x (Mult y (lift z))
push (Plus (Let x y) z) = Just $ Let x (Plus y (lift z))
push (Xor  (Let x y) z) = Just $ Let x (Xor  y (lift z))

push (Mult x (Let y z)) = Just $ Let y (Mult (lift x) z)
push (Plus x (Let y z)) = Just $ Let y (Plus (lift x) z)
push (Xor  x (Let y z)) = Just $ Let y (Xor  (lift x) z)

push _ = Nothing

-------------------------------------------------------------------------------
-- CSE
-------------------------------------------------------------------------------

{-
cse :: Eq a => Expr a -> Expr a
cse (Let x0 y0) = let y1 = subst (lift x0) (V Nothing) y0 in Let x0 (cse y1)
  where
    subst :: Eq b => Expr b -> Expr b -> Expr b -> Expr b
    subst x x' y | x == y = x'

    subst x x' (Let y z)
        | y == x = instantiate x' z
    subst x x' (Let y z) = Let (subst x x' y) (subst (lift x) (lift x') z)

    subst _ _ e@(V _) = e
    subst _ _ e@(K _) = e

    subst x x' (ShiftR y n) = ShiftR (subst x x' y) n
    subst x x' (Xor y z)    = Xor (subst x x' y) (subst x x' z)
    subst x x' (Plus y z)   = Plus (subst x x' y) (subst x x' z)
    subst x x' (ShiftL y n) = ShiftL (subst x x' y) n
    subst x x' (Mult y z)   = Mult (subst x x' y) (subst x x' z)
    subst x x' (Trunc16 y)  = Trunc16 (subst x x' y)
    subst x x' (Trunc32 y)  = Trunc32 (subst x x' y)

cse e@(V _)      = e
cse e@(K _)      = e
cse (ShiftR x n) = ShiftR (cse x) n
cse (Xor x y)    = Xor (cse x) (cse y)
cse (Mult x y)   = Mult (cse x) (cse y)

cse (ShiftL x n) = ShiftL (cse x) n
cse (Plus x y)   = Plus (cse x) (cse y)

cse (Trunc16 x) = Trunc16 (cse x)
cse (Trunc32 x) = Trunc32 (cse x)
-}
