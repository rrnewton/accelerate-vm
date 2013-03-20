

-- ex :: Exp Word8
ex :: Exp Dynamic
-- ex = case eltty of
--        S.TInt   -> A.constant (read str :: Int)
--        S.TWord8 -> A.constant (read str :: Word8)
--        -- ...

ex = A.constant$
     case eltty of
       S.TInt   -> toDyn (read str :: Int)
       S.TWord8 -> toDyn (read str :: Word8)
       -- ...

--------------------------------------------------------------------------------
-- Messing around with Dynamic:
--------------------------------------------------------------------------------

type instance Sug.EltRepr Dynamic = ((), Dynamic)
type instance Sug.EltRepr' Dynamic = Dynamic

instance Dat.ArrayElt Dynamic where 

-- instance ArrayElt Int where
--   type ArrayPtrs Int = Ptr Int
--   unsafeIndexArrayData (AD_Int ba) i   = unsafeIndexArray ba i
--   ptrsOfArrayData (AD_Int ba)          = uArrayPtr ba
--   newArrayData size                    = liftM AD_Int $ unsafeNewArray_ size wORD_SCALE
--   unsafeReadArrayData (AD_Int ba) i    = unsafeReadArray ba i
--   unsafeWriteArrayData (AD_Int ba) i e = unsafeWriteArray ba i e
--   unsafeFreezeArrayData (AD_Int ba)    = liftM AD_Int $ Unsafe.unsafeFreeze ba
--   ptrsOfMutableArrayData (AD_Int ba)   = sTUArrayPtr ba
--   arrayElt                             = ArrayEltRint

instance IsScalar Dynamic where
--  scalarType = T.NumScalarType T.numType
  
-- instance IsNum Dynamic where
--   numType = error "No value to peak at here!"
  
  -- scalarType dy
  --   | Just _ <- (fromDynamic dy :: Maybe Int) = T.TypeInt T.IntegralDict

instance Elt Dynamic where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = T.SingleTuple T.scalarType
  fromElt'      = id
  toElt'        = id


--------------------------------------------------------------------------------
-- Or how about Const?
--------------------------------------------------------------------------------

type instance Sug.EltRepr S.Const = ((), S.Const)
type instance Sug.EltRepr' S.Const = S.Const

instance Dat.ArrayElt S.Const where 

instance IsScalar S.Const where
  
instance Elt S.Const where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = T.SingleTuple T.scalarType
  fromElt'      = id
  toElt'        = id


instance Typeable S.Const where
  typeOf _ = mkTyConApp (mkTyCon "Const") []


