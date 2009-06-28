module Foreign.With (
  alloca, allocaArray, withForeignPtr
  ) where

import Control.Monad.Trans (MonadIO(..))
import Foreign.ForeignPtr (
  ForeignPtr, touchForeignPtr, unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (free, malloc)
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
  p <- liftIO malloc
  r <- func p
  liftIO $ free p
  return r

allocaArray ::
  (MonadIO m, Storable a, Integral i) =>
  i -> (Ptr a -> m b) -> m b
allocaArray size func = do
  p <- liftIO . mallocArray $ fromIntegral size
  r <- func p
  liftIO $ free p
  return r

