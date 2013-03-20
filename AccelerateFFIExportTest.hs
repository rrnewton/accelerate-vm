

import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter

import Data.Typeable
import Data.Dynamic
import Data.Word

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Exception (bracket)
import Control.Monad (when)
-- import Duma.Font

foreign export ccall foo :: Int -> IO Int

foo :: Int -> IO Int
foo n = return (length (f n))

f :: Int -> [Int]
f 0 = []
f n = n:(f (n-1))

main = putStrLn "hello"


-- foreign import ccall duma_begin :: FunPtr (FunPtr a -> IO ()) -> IO Bool
-- foreign import ccall duma_end :: IO ()
-- foreign import ccall duma_run :: IO () -- implements a Windows message loop

-- foreign import ccall "wrapper" mkFreeFunPtr :: (FunPtr a -> IO ()) -> IO (FunPtr (FunPtr a -> IO ()))

-- run :: IO a -> IO ()
-- run f = bracket
--           (mkFreeFunPtr freeHaskellFunPtr
--           )
--           (\freeFunPtrFn -> do
--              duma_end
--              freeHaskellFunPtr freeFunPtrFn
--           )
--           (\freeFunPtrFn -> do
--              initialized <- duma_begin freeFunPtrFn
--              when (initialized)
--                (f >> duma_run)
--           )


{-

 typedef void (*FunPtrFn)(HsFunPtr fn);
 FunPtrFn freeFunPtrFn = NULL;
 
 __declspec(dllexport) HsBool duma_begin(HsFunPtr freeFunPtrFnRaw){
    freeFunPtrFn = reinterpret_cast<FunPtrFn>(freeFunPtrFnRaw);
    return HS_BOOL_TRUE;
 }

 (*freeFunPtrFn)(the_fun_ptr_to_free);

 foreign import ccall "&" hs_free_fun_ptr :: FunPtr (FunPtr a -> IO ())

-}

--------------------------------------------------------------------------------


-- IntegralDict