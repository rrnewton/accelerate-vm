{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-- {-# LANGUAGE StandaloneDeriving #-}

import Prelude as P
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter
import qualified Data.Array.Accelerate.BackendKit.IRs.SimpleAcc as S
import           Data.Array.Accelerate.BackendKit.IRs.SimpleAcc (Type(..))
import qualified Data.Array.Accelerate.BackendKit.IRs.Internal.AccClone as C
import qualified Data.Array.Accelerate.Array.Sugar as Sug
import qualified Data.Array.Accelerate.Array.Data  as Dat
import qualified Data.Array.Accelerate.Type as T

import Data.Maybe
import Data.Typeable
import Data.Dynamic
import Data.Word
import Debug.Trace
import Control.Exception (bracket)
import Control.Monad (when)

import Unsafe.Coerce (unsafeCoerce)

-- Backend-kit representation:
------------------------------------------------------------
aty = TArray 0 TWord8
-- aexp = S.Unit (S.EConst (S.I8 8))

-- prog = S.Prog { 
--   S.progBinds   = [],
--   S.progResults = [S.var "x"],
--   S.progType    = ty
--   }

vr = S.var "x"

aexp = C.Map  aty (S.Lam1 (vr,TInt8) (C.EPrimApp TInt8 (S.NP S.Add) [C.EVr vr, C.EConst (S.I8 1)])) $
       C.Unit aty (C.EConst (S.I8 8))

------------------------------------------------------------
-- Or we could start from a string:

-- Let's say we start with "map (+1) (unit 8)", final type "Array word8" and we want
-- to deserialize.  To use the code presumably the user can provide the final type,
-- but not any intermediate types (e.g. for unit).

str :: String
str = "8"

eltty = case aty of TArray _ t -> t

ex1 :: Dynamic
ex1 = case eltty of
       TInt   -> toDyn ((A.constant (read str :: Int)) :: Exp Int)
       TWord8 -> toDyn ((A.constant (read str :: Word8)) :: Exp Word8)
       -- ...

-- unitDyn :: Exp S.Const -> Acc (Scalar S.Const)
-- unitDyn :: Type -> (forall e . Elt e => Exp e -> Acc (Scalar e))
unitDyn :: Type -> Dynamic -> Dynamic
unitDyn ty ex =
  case ty of
    TArray _ TWord8 -> toDyn (unit (fromDyn ex (unused::Exp Word8)))
    TArray _ TInt   -> toDyn (unit (fromDyn ex (unused::Exp Int)))

    TArray _ (TTuple [TInt,TInt]) ->
      toDyn (unit (fromDyn ex (unused::Exp (Int,Int))))
    TArray _ (TTuple ls) ->
      undefined -- toDyn (unit (fromDyn ex (unused::Exp (Int,Int))))    
    --  ...

arr1 :: Dynamic
arr1 = unitDyn aty ex1

theFn = (+1) -- TODO


unused = error "This dummy value should not be used"

--------------------------------------------------------------------------------
-- Scrap

arr0 :: Dynamic
arr0
  | Just x <- (fromDynamic ex1:: Maybe(Exp Int  )) = toDyn$ A.unit x
  | Just x <- (fromDynamic ex1:: Maybe(Exp Word8)) = toDyn$ A.unit x
  -- ...                                                     


unpack1 :: Maybe (Exp Word8) -- Test.
unpack1 = fromDynamic ex1

unpack2 :: (Acc (Scalar Word8))
unpack2 = fromJust $ fromDynamic arr1


-- try0 :: Acc (Scalar Word8)
-- try0 = A.map theFn arr


dest :: Acc (Scalar Word8)
dest = A.map (+1) (A.unit 8)

--------------------------------------------------------------------------------
-- Misc

singletonScalarType :: T.IsScalar a => a -> T.TupleType ((), a)
singletonScalarType _ = T.PairTuple T.UnitTuple (T.SingleTuple T.scalarType)

--------------------------------------------------------------------------------

-- Reified dictionary approach:

data EltDict a where
  EltDict :: (Elt a) => EltDict a
  -- Experimenting:
  EltTupDict :: (Elt a,Elt b) => EltDict a -> EltDict b -> EltDict (a,b)

data EltType where
  EltInt     :: EltDict Int   -> EltType
  EltInt8    :: EltDict Int8  -> EltType

  EltTup     :: (Elt a, Elt b) =>
                 -- Typeable a, Typeable b,
                 -- Dat.ArrayElt (Sug.EltRepr a),
                 -- Dat.ArrayElt (Sug.EltRepr' b)) =>
                EltDict a -> EltDict b -> EltType

  EltTup2     :: EltDict (a,b) -> EltType  
                

instance Show EltType where
  show (EltInt _)  = "Int"
  show (EltInt8 _) = "Int8"
  show (EltTup _ _) = "Tup"

instance Enum EltType where
  fromEnum (EltInt _)  = 0
  fromEnum (EltInt8 _) = 1
  toEnum 0 = EltInt  EltDict  
  toEnum 1 = EltInt8 EltDict

data AccBuilder

-- unitS :: EltType -> S.Exp -> AccBuilder
unitS :: EltType -> Dynamic -> Dynamic
unitS elt exp =
  case elt of
    EltInt (_ :: EltDict Int) ->
      toDyn$ unit$ fromDyn exp (unused::Exp Int)

    EltTup (_ :: EltDict a) (_ :: EltDict b) ->
      toDyn$ unit$ fromDyn exp (unused :: Exp (a,b))


ex2 :: EltType -> Dynamic
ex2 = undefined
-- ex2 = case eltty of
--        TInt   -> toDyn ((A.constant (read str :: Int)) :: Exp Int)
--        TWord8 -> toDyn ((A.constant (read str :: Word8)) :: Exp Word8)

arr2 :: Dynamic
--arr2 = unitS int_elt (ex2 int_elt)
arr2 = unitS tup_elt (ex2 tup_elt)

int_elt = EltInt EltDict

tup_elt = let EltInt d = int_elt in
          EltTup d d

dest2 :: Acc (Scalar (Word8,Word16))
dest2 = (A.unit tup)
 where
  tup :: Exp (Word8,Word16)
  tup = lift (constant 8,constant 15)



#if 0
foreign export ccall int_elt :: IO Int
int_elt = return$ fromEnum (EltInt EltDict)
#endif

main = putStrLn "hi"

-- int_elt :: EltType
-- int_elt  = toEnum 0

-- int8_elt :: EltType
-- int8_elt = toEnum 1


{-
data EltType a where
  EltInt     :: EltDict Int   -> EltType Int
  EltInt8    :: EltDict Int8  -> EltType Int8

instance Show (EltType a) where
  show (EltInt _)  = "Int"
  show (EltInt8 _) = "Int8"

-- instance Enum (EltType Int) where
--   fromEnum (EltInt _)  = 0
--   toEnum 0 = EltInt  EltDict

-- instance Enum (EltType Int8) where 
--   fromEnum (EltInt8 _) = 1    
--   toEnum 1 = EltInt8 EltDict
-}

-- deriving instance Enum EltType

-- data IntegralType a where
--   TypeInt     :: IntegralDict Int     -> IntegralType Int
--   TypeInt8    :: IntegralDict Int8    -> IntegralType Int8
--   TypeInt16   :: IntegralDict Int16   -> IntegralType Int16
--   TypeInt32   :: IntegralDict Int32   -> IntegralType Int32
--   TypeInt64   :: IntegralDict Int64   -> IntegralType Int64
--   TypeWord    :: IntegralDict Word    -> IntegralType Word
--   TypeWord8   :: IntegralDict Word8   -> IntegralType Word8
--   TypeWord16  :: IntegralDict Word16  -> IntegralType Word16
--   TypeWord32  :: IntegralDict Word32  -> IntegralType Word32
--   TypeWord64  :: IntegralDict Word64  -> IntegralType Word64
--   TypeCShort  :: IntegralDict CShort  -> IntegralType CShort
--   TypeCUShort :: IntegralDict CUShort -> IntegralType CUShort
--   TypeCInt    :: IntegralDict CInt    -> IntegralType CInt
--   TypeCUInt   :: IntegralDict CUInt   -> IntegralType CUInt
--   TypeCLong   :: IntegralDict CLong   -> IntegralType CLong
--   TypeCULong  :: IntegralDict CULong  -> IntegralType CULong
--   TypeCLLong  :: IntegralDict CLLong  -> IntegralType CLLong
--   TypeCULLong :: IntegralDict CULLong -> IntegralType CULLong
