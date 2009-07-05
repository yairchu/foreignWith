{-# OPTIONS -O2 -Wall #-}

module Foreign.With (
  alloca, allocaArray, allocaBytes, withCString, withForeignPtr
  ) where

import Control.Monad.Trans (MonadIO(..))
import Foreign.C.String (CString, newCString)
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

withFree :: MonadIO m => IO (Ptr a) -> (Ptr a -> m b) -> m b
withFree mallocFunc func = do
  fptr <- liftIO $ newForeignPtr finalizerFree =<< mallocFunc
  withForeignPtr fptr func

withCString :: MonadIO m => String -> (CString -> m a) -> m a
withCString = withFree . newCString

alloca :: (MonadIO m, Storable a) => (Ptr a -> m b) -> m b
alloca = withFree malloc

allocaArray ::
  (MonadIO m, Storable a, Integral i) => i -> (Ptr a -> m b) -> m b
allocaArray = withFree . mallocArray . fromIntegral

allocaBytes :: (MonadIO m, Integral i) => i -> (Ptr a -> m b) -> m b
allocaBytes = withFree . mallocBytes . fromIntegral

