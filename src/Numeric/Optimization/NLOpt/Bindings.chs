{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      :  Numeric.Optimization.NLOpt.Bindings
Copyright   :  (c) Matthew Peddie 2015
License     :  LGPL3

Maintainer  :  mpeddie@gmail.com
Stability   :  experimental
Portability :  GHC

Low-level FFI bindings to the NLOpt nonlinear optimization library.
See <http://ab-initio.mit.edu/wiki/index.php/NLopt_Reference the NLOpt reference manual>
for detailed information.

For a few "get"-style functions, we require the caller to pass an
explicit problem size.  A higher-level interface could easily get this
information by using 'nloptGetDimension'.

Currently, no special type-correctness guarantees are provided for
user data in callbacks -- the 'StablePtr' gets cast from a C void
pointer ('Ptr ()'), and that's it.  A higher-level interface could use the

No exceptions are thrown on failure.  Please check the 'NloptResult'
you get back.

-}

module Numeric.Optimization.NLOpt.Bindings (
  -- * Callback types
  NloptFunc
  , NloptMFunc
  , NloptPrecond
    -- * Data types
  , NloptAlgorithm
  , nloptAlgorithmName
  , NloptResult
  , NloptOpt
  , nloptCreate
  , nloptCopy
  , nloptGetAlgorithm
  , nloptGetDimension
    -- * Minimization
  , nloptOptimize
    -- * Objective function
  , nloptSetMinObjective
  , nloptSetMaxObjective
  , nloptSetPrecondMinObjective
  , nloptSetPrecondMaxObjective
    -- * Constraints
    -- ** Bound constraints
  , nloptSetLowerBounds
  , nloptSetLowerBounds1
  , nloptGetLowerBounds
  , nloptSetUpperBounds
  , nloptSetUpperBounds1
  , nloptGetUpperBounds
    -- ** Inequality constraints
  , nloptRemoveInequalityConstraints
  , nloptAddInequalityConstraint
  , nloptAddPrecondInequalityConstraint
  , nloptAddInequalityMconstraint
    -- ** Equality constraints
  , nloptRemoveEqualityConstraints
  , nloptAddEqualityConstraint
  , nloptAddPrecondEqualityConstraint
  , nloptAddEqualityMconstraint
    -- * Stopping criteria
  , nloptSetStopval
  , nloptGetStopval
  , nloptSetFtolRel
  , nloptGetFtolRel
  , nloptSetFtolAbs
  , nloptGetFtolAbs
  , nloptSetXtolRel
  , nloptGetXtolRel
  , nloptSetXtolAbs1
  , nloptSetXtolAbs
  , nloptGetXtolAbs
  , nloptSetMaxeval
  , nloptGetMaxeval
  , nloptSetMaxtime
  , nloptGetMaxtime
  , nloptForceStop
  , nloptSetForceStop
  , nloptGetForceStop
    -- * Algorithm-specific parameters
  , nloptSetLocalOptimizer
  , nloptSetPopulation
  , nloptGetPopulation
  , nloptSetVectorStorage
  , nloptGetVectorStorage
  , nloptSetDefaultInitialStep
  , nloptSetInitialStep
  , nloptSetInitialStep1
  , nloptGetInitialStep
    -- * Miscellaneous
  , nloptSrand
  , nloptSrandTime
  , nloptVersion
    -- * Helpers
  , wrapNloptFunc
  ) where

import Foreign
import Foreign.C

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import Control.Monad ((>=>), when)

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

#include <nlopt.h>

{#enum nlopt_algorithm as NloptAlgorithm {underscoreToCase} deriving (Show, Read, Eq, Ord, Bounded, Data, Typeable, Generic) #}

{#enum nlopt_result as NloptResult {underscoreToCase} deriving (Show, Read, Eq, Ord, Bounded, Data, Typeable, Generic) #}

-- As far as I'm aware, it's OK to just use 'Double' for things like
-- this.  However, c2hs generates everything with 'CDouble' when it
-- scrapes headers.  Rather than write that stuff all myself, you have
-- to cast or do a '(M)V.map realToFrac' inside these callbacks.

type NloptFunc =
  CUInt
  -- This is actually a const pointer
  -> Ptr CDouble
  -> Ptr CDouble -> Ptr () -> IO CDouble

foreign import ccall unsafe "wrapper"
  mkNloptFunc :: NloptFunc -> IO (FunPtr (NloptFunc))

withNloptFunc :: NloptFunc -> (FunPtr NloptFunc -> IO b) -> IO b
withNloptFunc f a = mkNloptFunc f >>= a

type HNloptFunc a = Word -> V.Vector Double -> a -> Double
type HNloptGradFunc a = Word -> V.Vector Double -> a -> V.Vector Double

wrapNloptFunc :: (HNloptFunc a) -> (HNloptGradFunc a) -> NloptFunc
wrapNloptFunc hf hg n' x' grad' user' = do
  let n = fromIntegral n'
  x <- vOfPtr V.unsafeFromForeignPtr0 n' (castPtr x')
  user <- deRefStablePtr $ castPtrToStablePtr user'
  let cost = realToFrac $ hf n x user
  when (grad' /= nullPtr) $ do
    let costgrad = V.unsafeCast $ hg n x user
    grad <- vOfPtr MV.unsafeFromForeignPtr0 n' grad'
    V.unsafeCopy grad costgrad
  return cost
  where
    vOfPtr f n ptr = do
      fptr <- newForeignPtr_ ptr
      return $ f fptr (fromIntegral n)

type NloptMFunc =
  CUInt -> Ptr CDouble -> CUInt
  -- This is actually a const pointer
  -> Ptr CDouble
  -> Ptr CDouble -> Ptr () -> IO ()

foreign import ccall unsafe "wrapper"
  mkNloptMFunc :: NloptMFunc -> IO (FunPtr (NloptMFunc))

withNloptMFunc :: NloptMFunc -> (FunPtr NloptMFunc -> IO b) -> IO b
withNloptMFunc f a = mkNloptMFunc f >>= a

type NloptPrecond =
  CUInt
  -- These two are actually const pointers
  -> Ptr CDouble -> Ptr CDouble
  -> Ptr CDouble -> Ptr () -> IO ()

foreign import ccall unsafe "wrapper"
  mkNloptPrecond :: NloptPrecond -> IO (FunPtr (NloptPrecond))

withNloptPrecond :: NloptPrecond -> (FunPtr NloptPrecond -> IO b) -> IO b
withNloptPrecond f a = mkNloptPrecond f >>= a

{#pointer *nlopt_opt_s as NloptOpt foreign finalizer nlopt_destroy newtype#}

{- Custom marshalling -}

peekIntConv :: Ptr CInt -> IO Int
peekIntConv = fmap fromIntegral . peek

copyImmutableVector :: Storable a => V.Vector a
                    -> (Ptr a -> IO t) -> IO (t, V.Vector a)
copyImmutableVector v a = do
  mv <- V.thaw v
  ret <- MV.unsafeWith mv a
  rv <- V.unsafeFreeze mv
  return (ret, rv)

withImmutableVector :: Storable a => V.Vector a -> (Ptr a -> IO b) -> IO b
withImmutableVector v a = withForeignPtr vfptr a
  where
    (vfptr, _) = V.unsafeToForeignPtr0 v

withImmutableVectorC :: V.Vector Double -> (Ptr CDouble -> IO b) -> IO b
withImmutableVectorC v a = withImmutableVector v (a . castPtr)

newImmutableVector :: Storable a
                   => Word -> (Ptr a -> IO b) -> IO (b, V.Vector a)
newImmutableVector n action = do
  mv <- MV.new (fromIntegral n)
  let (mvfptr, _) = MV.unsafeToForeignPtr0 mv
  res <- withForeignPtr mvfptr action
  v <- V.unsafeFreeze mv
  return (res, v)

{- Basics -}

{#fun unsafe nlopt_algorithm_name as ^ { `NloptAlgorithm' } -> `CString' #}

{#fun unsafe nlopt_srand as ^ { `CULong' } -> `()' #}

{#fun unsafe nlopt_srand_time as ^ { } -> `()' #}

{#fun unsafe nlopt_version as ^
 { alloca- `Int' peekIntConv*
 , alloca- `Int' peekIntConv*
 , alloca- `Int' peekIntConv* } -> `()' #}

{#fun unsafe nlopt_create as ^ { `NloptAlgorithm', fromIntegral `Word' } -> `NloptOpt' #}

{#fun unsafe nlopt_copy as ^ { `NloptOpt' } -> `NloptOpt' #}

-- I don't quite know how to get all the outputs and the vector
-- allocation lined up for this one, so I've done it by hand.
nloptOptimize :: NloptOpt -> V.Vector Double
              -> IO ((Double, NloptResult), V.Vector Double)
nloptOptimize p i =
  withNloptOpt p $ \p' ->
  copyImmutableVector i $ \i' ->
  alloca $ \optr -> do
    res <- nloptOptimize' p' i' optr
    outval <- peek optr
    return (outval, toEnum . fromIntegral $ res)

foreign import ccall unsafe "NLOpt.chs.h nlopt_optimize"
  nloptOptimize' :: Ptr NloptOpt -> Ptr Double -> Ptr Double -> IO CInt

-- TODO(MP): Can we make anything that passes a StablePtr to the C
-- side also register an extra finalizer on the NloptOpt object that
-- just calls freeStablePtr?  Then we could just accept any old value
-- from the user.

{#fun nlopt_set_min_objective as ^
 { `NloptOpt', withNloptFunc* `NloptFunc', castStablePtrToPtr `StablePtr a'} -> `NloptResult' #}

{#fun nlopt_set_max_objective as ^
 { `NloptOpt', withNloptFunc* `NloptFunc', castStablePtrToPtr `StablePtr a'} -> `NloptResult' #}

{#fun nlopt_set_precond_min_objective as ^
 { `NloptOpt', withNloptFunc* `NloptFunc'
   , withNloptPrecond* `NloptPrecond', castStablePtrToPtr `StablePtr a'}
  -> `NloptResult' #}

{#fun nlopt_set_precond_max_objective as ^
 { `NloptOpt', withNloptFunc* `NloptFunc'
   , withNloptPrecond* `NloptPrecond', castStablePtrToPtr `StablePtr a'}
  -> `NloptResult' #}

{#fun unsafe nlopt_get_dimension as ^ { `NloptOpt' } -> `Word' fromIntegral #}

{#fun unsafe nlopt_get_algorithm as ^ { `NloptOpt' } -> `Word' fromIntegral #}

{- Constraints -}

{#fun unsafe nlopt_set_lower_bounds as ^
 { `NloptOpt', withImmutableVectorC* `V.Vector Double' } -> `NloptResult' #}
{#fun unsafe nlopt_set_lower_bounds1 as ^
 { `NloptOpt', realToFrac `Double' } -> `NloptResult' #}

-- Unfortunately, a lot of the 'get' functions need an extra argument
-- passed in for the vector size.  The other option would be to get it
-- from nloptGetDimension, but either way, I don't know how to write
-- it with a c2hs macro.
nloptGetLowerBounds :: Word -> NloptOpt -> IO (NloptResult, V.Vector Double)
nloptGetLowerBounds n p =
  withNloptOpt p $ \p' ->
  newImmutableVector n $ \mvptr -> do
    ret <- nloptGetLowerBounds' p' mvptr
    return . toEnum . fromIntegral $ ret

foreign import ccall unsafe "NLOpt.chs.h nlopt_get_lower_bounds"
  nloptGetLowerBounds' :: Ptr NloptOpt -> Ptr Double -> IO CInt

{#fun unsafe nlopt_set_upper_bounds as ^
 { `NloptOpt', withImmutableVectorC* `V.Vector Double' } -> `NloptResult' #}
{#fun unsafe nlopt_set_upper_bounds1 as ^
 { `NloptOpt', realToFrac `Double' } -> `NloptResult' #}

nloptGetUpperBounds :: Word -> NloptOpt -> IO (NloptResult, V.Vector Double)
nloptGetUpperBounds n p =
  withNloptOpt p $ \p' ->
  newImmutableVector n $ \mvptr -> do
    ret <- nloptGetUpperBounds' p' mvptr
    return . toEnum . fromIntegral $ ret

foreign import ccall unsafe "NLOpt.chs.h nlopt_get_upper_bounds"
  nloptGetUpperBounds' :: Ptr NloptOpt -> Ptr Double -> IO CInt

{#fun unsafe nlopt_remove_inequality_constraints as ^
 { `NloptOpt' } -> `NloptResult' #}
{#fun unsafe nlopt_add_inequality_constraint as ^
 { `NloptOpt', withNloptFunc* `NloptFunc'
   , castStablePtrToPtr `StablePtr a', realToFrac `Double' } -> `NloptResult' #}
{#fun unsafe nlopt_add_precond_inequality_constraint as ^
 { `NloptOpt', withNloptFunc* `NloptFunc', withNloptPrecond* `NloptPrecond'
   , castStablePtrToPtr `StablePtr a', realToFrac `Double' } -> `NloptResult' #}
{#fun unsafe nlopt_add_inequality_mconstraint as ^
 { `NloptOpt', fromIntegral `Word', withNloptMFunc* `NloptMFunc'
   , castStablePtrToPtr `StablePtr a', withImmutableVectorC* `V.Vector Double' }
 -> `NloptResult' #}

{#fun unsafe nlopt_remove_equality_constraints as ^
 { `NloptOpt' } -> `NloptResult' #}
{#fun unsafe nlopt_add_equality_constraint as ^
 { `NloptOpt', withNloptFunc* `NloptFunc'
   , castStablePtrToPtr `StablePtr a', realToFrac `Double' } -> `NloptResult' #}
{#fun unsafe nlopt_add_precond_equality_constraint as ^
 { `NloptOpt', withNloptFunc* `NloptFunc', withNloptPrecond* `NloptPrecond'
   , castStablePtrToPtr `StablePtr a', realToFrac `Double' } -> `NloptResult' #}
{#fun unsafe nlopt_add_equality_mconstraint as ^
 { `NloptOpt', fromIntegral `Word', withNloptMFunc* `NloptMFunc'
   , castStablePtrToPtr `StablePtr a', withImmutableVectorC* `V.Vector Double' }
 -> `NloptResult' #}

{- Stopping criteria -}

{#fun unsafe nlopt_set_stopval as ^ { `NloptOpt', `Double' } -> `NloptResult' #}
{#fun unsafe nlopt_get_stopval as ^ { `NloptOpt' } -> `Double' #}

{#fun unsafe nlopt_set_ftol_rel as ^ { `NloptOpt', `Double' } -> `NloptResult' #}
{#fun unsafe nlopt_get_ftol_rel as ^ { `NloptOpt' } -> `Double' #}

{#fun unsafe nlopt_set_ftol_abs as ^ { `NloptOpt', `Double' } -> `NloptResult' #}
{#fun unsafe nlopt_get_ftol_abs as ^ { `NloptOpt' } -> `Double' #}

{#fun unsafe nlopt_set_xtol_rel as ^ { `NloptOpt', `Double' } -> `NloptResult' #}
{#fun unsafe nlopt_get_xtol_rel as ^ { `NloptOpt' } -> `Double' #}

{#fun unsafe nlopt_set_xtol_abs1 as ^ { `NloptOpt', `Double' } -> `NloptResult' #}

{#fun unsafe nlopt_set_xtol_abs as ^
  { `NloptOpt', withImmutableVectorC* `V.Vector Double' } -> `NloptResult' #}

nloptGetXtolAbs :: Word -> NloptOpt -> IO (NloptResult, V.Vector Double)
nloptGetXtolAbs n p =
  withNloptOpt p $ \p' ->
  newImmutableVector n $ \mvptr -> do
    ret <- nloptGetXtolAbs' p' mvptr
    return . toEnum . fromIntegral $ ret

foreign import ccall unsafe "NLOpt.chs.h nlopt_set_xtol_abs"
  nloptGetXtolAbs' :: Ptr NloptOpt -> Ptr Double -> IO CInt

{#fun unsafe nlopt_set_maxeval as ^ { `NloptOpt', fromIntegral `Int' } -> `NloptResult' #}
{#fun unsafe nlopt_get_maxeval as ^ { `NloptOpt' } -> `Int' fromIntegral #}

{#fun unsafe nlopt_set_maxtime as ^ { `NloptOpt', `Double' } -> `NloptResult' #}
{#fun unsafe nlopt_get_maxtime as ^ { `NloptOpt' } -> `Double' #}

{#fun unsafe nlopt_force_stop as ^ { `NloptOpt' } -> `NloptResult' #}
{#fun unsafe nlopt_set_force_stop as ^ { `NloptOpt', fromIntegral `Int' } -> `NloptResult' #}
{#fun unsafe nlopt_get_force_stop as ^ { `NloptOpt' } -> `Int' fromIntegral #}

{- Algorithm-specific parameters -}

{#fun nlopt_set_local_optimizer as ^ { `NloptOpt', `NloptOpt' } -> `NloptResult' #}

{#fun nlopt_set_population as ^ { `NloptOpt', fromIntegral `Word' } -> `NloptResult' #}
{#fun nlopt_get_population as ^ { `NloptOpt' } -> `Word' fromIntegral #}

{#fun nlopt_set_vector_storage as ^ { `NloptOpt', fromIntegral `Word' } -> `NloptResult' #}
{#fun nlopt_get_vector_storage as ^ { `NloptOpt' } -> `Word' fromIntegral #}

{#fun nlopt_set_default_initial_step as ^
 { `NloptOpt', withImmutableVectorC* `V.Vector Double' } -> `NloptResult' #}
{#fun nlopt_set_initial_step as ^
 { `NloptOpt', withImmutableVectorC* `V.Vector Double' } -> `NloptResult' #}

{#fun nlopt_set_initial_step1 as ^ { `NloptOpt', `Double' } -> `NloptResult' #}

nloptGetInitialStep :: NloptOpt -> V.Vector Double
                    -> IO (NloptResult, V.Vector Double)
nloptGetInitialStep p x0 =
  withNloptOpt p $ \p' ->
  newImmutableVector (fromIntegral $ V.length x0) $ \mvptr ->
  withImmutableVector x0 $ \x0ptr -> do
    ret <- nloptGetInitialStep' p' x0ptr mvptr
    return . toEnum . fromIntegral $ ret

foreign import ccall unsafe "NLOpt.chs.h nlopt_get_initial_step"
  nloptGetInitialStep' :: Ptr NloptOpt -> Ptr Double -> Ptr Double -> IO CInt
