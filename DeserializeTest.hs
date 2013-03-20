{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

import Prelude as P
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter
import qualified Data.Array.Accelerate.BackendKit.IRs.SimpleAcc as S
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

t8 = typeOf (undefined::Word8)

-- Let's say we start with "map (+1) (unit 8)", final type "Array word8" and we want
-- to deserialize.  To use the code presumably the user can provide the final type,
-- but not any intermediate types (e.g. for unit).

ty = S.TArray 0 S.TWord8

str :: String
str = "8"

eltty = case ty of S.TArray _ t -> t


ex1 :: Dynamic
ex1 = case eltty of
       S.TInt   -> toDyn ((A.constant (read str :: Int)) :: Exp Int)
       S.TWord8 -> toDyn ((A.constant (read str :: Word8)) :: Exp Word8)
       -- ...

arr1 :: Dynamic
arr1
  | Just x <- (fromDynamic ex1:: Maybe(Exp Int  )) = toDyn$ A.unit x
  | Just x <- (fromDynamic ex1:: Maybe(Exp Word8)) = toDyn$ A.unit x
  -- ...                                                     

arr2 :: Dynamic
arr2 = unitDyn ty ex1

unpack1 :: Maybe (Exp Word8) -- Test.
unpack1 = fromDynamic ex1

unpack2 :: (Acc (Scalar Word8))
unpack2 = fromJust $ fromDynamic arr1

theFn = (+1) -- TODO

-- try0 :: Acc (Scalar Word8)
-- try0 = A.map theFn arr

dest :: Acc (Scalar Word8)
dest = A.map (+1) (A.unit 8)

-- unitDyn :: Exp S.Const -> Acc (Scalar S.Const)
-- unitDyn :: S.Type -> (forall e . Elt e => Exp e -> Acc (Scalar e))
unitDyn :: S.Type -> Dynamic -> Dynamic
unitDyn ty ex =
  case ty of
    S.TArray _ S.TWord8 -> toDyn$ (unit (fromDyn ex (unused::Exp Word8)) :: Acc (Scalar Word8))
--  ...

unused = error "This dummy value should not be used"

--------------------------------------------------------------------------------

singletonScalarType :: T.IsScalar a => a -> T.TupleType ((), a)
singletonScalarType _ = T.PairTuple T.UnitTuple (T.SingleTuple T.scalarType)

