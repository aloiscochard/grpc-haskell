module Properties where

import Network.GRPC.Core

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (poke)
-- grpcCompletionQueueCreate :: IO ((CompletionQueue))
-- grpcChannelCreate :: (String) -> (ChannelArgsPtr) -> IO ((Channel))

main :: IO ()
main = do
  cq <- grpcCompletionQueueCreate
  _  <- alloca $ \p -> do
    -- poke p $ []
    poke p $ ChannelArgs []
    grpcChannelCreate "channel0" p
    -- nullPtr
  return ()
