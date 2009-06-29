{-# OPTIONS -O2 -Wall #-}

module Foreign.With (
  alloca, allocaArray, allocaBytes, withForeignPtr
  ) where

import Control.Monad.Trans (MonadIO(..))
import Foreign.ForeignPtr (
  ForeignPtr, newForeignPtr, touchForeignPtr, unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (finalizerFree, malloc, mallocBytes)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)

withForeignPtr :: MonadIO m => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtr fptr func = do
  r <- func $ unsafeForeignPtrToPtr fptr
  liftIO $ touchForeignPtr fptr
  return r

alloca' :: MonadIO m => IO (Ptr a) -> (Ptr a -> m b) -> m b
alloca' mallocFunc func = do
  fptr <- liftIO $ newForeignPtr finalizerFree =<< mallocFunc
  withForeignPtr fptr func

alloca :: (MonadIO m, Storable a) => (Ptr a -> m b) -> m b
alloca = alloca' malloc

allocaArray ::
  (MonadIO m, Storable a, Integral i) => i -> (Ptr a -> m b) -> m b
allocaArray = alloca' . mallocArray . fromIntegral

allocaBytes :: (MonadIO m, Integral i) => i -> (Ptr a -> m b) -> m b
allocaBytes = alloca' . mallocBytes . fromIntegral

