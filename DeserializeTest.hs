{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StandaloneDeriving #-}

import Prelude as P
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I
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
--------------------------------------------------------------------------------


-- foreign export ccall int_elt :: IO Int
-- int_elt = return$ fromEnum (EltInt EltDict)


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

unused = error "This dummy value should not be used"

--------------------------------------------------------------------------------
-- Scrap

arr0 :: Dynamic
arr0
  | Just x <- (fromDynamic ex1:: Maybe(Exp Int  )) = toDyn$ A.unit x
  | Just x <- (fromDynamic ex1:: Maybe(Exp Word8)) = toDyn$ A.unit x
  -- ...                                                     


unpack1 :: Exp Word8 -- Test.
unpack1 = downcastE ex1

unpack2 :: (Acc (Scalar Word8))
unpack2 = downcastA arr1

run1 = I.run unpack2

-- theFn = (+1) -- TODO
-- try0 :: Acc (Scalar Word8)
-- try0 = A.map theFn arr

-- This is where we want to get to:
dest :: Acc (Scalar Word8)
dest = A.map (+1) (A.unit 8)



--------------------------------------------------------------------------------
-- (2) Reified dictionary approach:
--------------------------------------------------------------------------------

data EltDict a where
  EltDict :: (Elt a) => EltDict a
  -- Experimenting:
  EltTupDict :: (Elt a,Elt b) => EltDict a -> EltDict b -> EltDict (a,b)

data EltType where
  EltInt     :: EltDict Int   -> EltType
  EltInt8    :: EltDict Int8  -> EltType
  EltWord8   :: EltDict Word8 -> EltType  

  EltTup     :: (Elt a, Elt b) =>
                 -- Typeable a, Typeable b,
                 -- Dat.ArrayElt (Sug.EltRepr a),
                 -- Dat.ArrayElt (Sug.EltRepr' b)) =>
                EltDict a -> EltDict b -> EltType

  EltTup2     :: EltDict (a,b) -> EltType

  -- This version witnesses a relation.  Probably doesn't help.
  EltTup3     :: (Elt a, Elt b) =>
                 EltDict a -> EltDict b -> EltDict (a,b) -> EltType
                

instance Show EltType where
  show (EltInt _)  = "Int"
  show (EltInt8 _) = "Int8"
  show (EltWord8 _) = "Word8"
  show (EltTup a b) = "("++show a++","++show b++")"

instance Show (EltDict a) where
  show (EltDict :: EltDict a) = show (toDyn (undefined :: a))
  show (EltTupDict a b ) = "("++show a++","++show b++")"

-- Enums: Won't work for tuples.  Need StablePtrs, alas.
-- instance Enum EltType where
--   fromEnum (EltInt _)  = 0
--   fromEnum (EltInt8 _) = 1
--   toEnum 0 = EltInt  EltDict  
--   toEnum 1 = EltInt8 EltDict

-- TODO: make these pairs that keep around some printed rep for debugging purposes:
type SealedExp = Dynamic
type SealedAcc = Dynamic

downcastE :: forall a . Typeable a => SealedExp -> Exp a
downcastE d = case fromDynamic d of
                Just e -> e
                Nothing ->
                  error$"Attempt to unpack dynamic typed Expr with type "++show d
                     ++ " expect type "++ show (toDyn (unused::a))

downcastA :: forall a . Typeable a => SealedAcc -> Acc a
downcastA d = case fromDynamic d of
                Just e -> e
                Nothing ->
                  error$"Attempt to unpack dynamic typed Acc with type "++show d
                     ++ " expect type "++ show (toDyn (unused::a))       

unit2 :: EltType -> SealedExp -> SealedAcc
unit2 elt exp =
  case elt of
    EltInt (_ :: EltDict Int) ->
      toDyn$ unit$ fromDyn exp (unused::Exp Int)

    EltTup (_ :: EltDict a) (_ :: EltDict b) ->
      toDyn$ unit$ fromDyn exp (unused :: Exp (a,b))

-- A tuple constant:
ex2 :: SealedExp
ex2 = mkTup (word8_elt,ex1) (word8_elt,ex1)

mkTup :: (EltType,SealedExp) -> (EltType,SealedExp) -> SealedExp
mkTup (e1,dyn1) (e2,dyn2) =
  case mkTupElt e1 e2 of
    EltTup (_ :: EltDict a) (_ :: EltDict b) ->
      let x  = fromDyn dyn1 (unused::Exp a)
          y  = fromDyn dyn2 (unused::Exp b) in
      toDyn (lift (x,y) :: Exp (a,b))
      
arr2 :: SealedAcc
arr2 = unit2 tup_elt ex2

unpack3 :: Exp (Word8,Word8)
unpack3 = downcastE ex2

unpack4 :: (Acc (Scalar (Word8,Word8)))
unpack4 = downcastA arr2

int_elt   = EltInt   EltDict
word8_elt = EltWord8 EltDict
tup_elt   = mkTupElt word8_elt word8_elt
tup_elt2  = mkTupElt tup_elt   word8_elt
tup_elt3  = mkTupElt word8_elt tup_elt  

-- How do we do this without the full cartesian prudct?
mkTupElt :: EltType -> EltType -> EltType
mkTupElt e1 e2 =
  case (e1,e2) of
    (EltWord8 d1, EltWord8 d2) -> EltTup d1 d2
    (EltTup d1 d2, EltWord8 d3) ->
      -- Hmm, is this going to work?
      EltTup (EltTupDict d1 d2) d3

    -- We should use an EltRepr style type family to canonicalize tuples...
    (EltWord8 d3, EltTup d1 d2) ->
      EltTup d3 (EltTupDict d1 d2) 

run2 = I.run unpack4

dest2 :: Acc (Scalar (Word8,Word16))
dest2 = (A.unit tup)
 where
  tup :: Exp (Word8,Word16)
  tup = lift (constant 8,constant 15)

--------------------------------------------------------------------------------
-- (3) Trying one other thing to reuse Type.hs types:
  --------------------------------------------------------------------------------

-- Erase the type parameter from an existing Accelerate type.
data SealedIntegralType where
  SealedIntegralType :: T.IntegralType a -> SealedIntegralType
  
data SealedTupleType where
  SealedTupleType :: T.TupleType a -> SealedTupleType

unit3 :: SealedTupleType -> SealedExp -> SealedAcc
unit3 elt exp =
  case elt of
    SealedTupleType (t :: T.TupleType a) ->
      case t of
        T.UnitTuple -> toDyn$ unit$ constant ()
        T.SingleTuple (st :: T.ScalarType s) ->
          undefined -- No ELT instance here.
--          toDyn$ unit (downcastE exp :: Exp s)

--------------------------------------------------------------------------------
-- (4) Ok, I think we've learned something above, let's try again
--------------------------------------------------------------------------------        

-- We enhance TupleType with Elt constraints:
data EltTuple a where
  UnitTuple   ::                                               EltTuple ()
  SingleTuple :: Elt a          => T.ScalarType a           -> EltTuple a
  PairTuple   :: (Elt a, Elt b) => EltTuple a -> EltTuple b -> EltTuple (a, b)

data SealedEltTuple where
  SealedEltTuple :: EltTuple a -> SealedEltTuple

-- We could avoid Dynamic:
data SealedExp2 where SealedExp2 :: Exp a -> SealedExp2

unit4 :: SealedEltTuple -> SealedExp -> SealedAcc
unit4 elt exp =
  case elt of
    SealedEltTuple (t :: EltTuple et) ->
      case t of
        UnitTuple -> toDyn$ unit$ constant ()
        SingleTuple (st :: T.ScalarType s) ->
          toDyn$ unit (downcastE exp :: Exp s)
        PairTuple (_ :: EltTuple l) (_ :: EltTuple r) ->
          toDyn$ unit (downcastE exp :: Exp (l,r))

arr4 :: SealedAcc
arr4 = unit4 tup_elt ex2
 where
   sword8_elt, tup_elt :: SealedEltTuple
   sword8_elt = SealedEltTuple word8_elt
   word8_elt = SingleTuple (T.scalarType :: T.ScalarType Word8)
   tup_elt   = SealedEltTuple$ PairTuple word8_elt word8_elt

run4 = I.run (downcastA arr4 :: (Acc (Scalar (Word8,Word8))))

--------------------------------------------------------------------------------
-- Misc

singletonScalarType :: T.IsScalar a => a -> T.TupleType ((), a)
singletonScalarType _ = T.PairTuple T.UnitTuple (T.SingleTuple T.scalarType)

main = do 
  print run1
  print run2
  print run4
