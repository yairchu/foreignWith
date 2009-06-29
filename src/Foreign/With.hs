{-# OPTIONS -O2 -Wall #-}

module Foreign.With (
  alloca, allocaArray, withForeignPtr
  ) where

import Control.Monad.Trans (MonadIO(..))
import Foreign.ForeignPtr (
  ForeignPtr, newForeignPtr, touchForeignPtr, unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (finalizerFree, malloc)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)

withForeignPtr :: MonadIO m => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtr fptr func = do
  r <- func $ unsafeForeignPtrToPtr fptr
  liftIO $ touchForeignPtr fptr
  return r

alloca :: (MonadIO m, Storable a) => (Ptr a -> m b) -> m b
alloca func = do
  fptr <- liftIO $ newForeignPtr finalizerFree =<< malloc
  withForeignPtr fptr func

allocaArray ::
  (MonadIO m, Storable a, Integral i) =>
  i -> (Ptr a -> m b) -> m b
allocaArray size func = do
  fptr <- liftIO $ newForeignPtr finalizerFree =<<
    mallocArray (fromIntegral size)
  withForeignPtr fptr func

