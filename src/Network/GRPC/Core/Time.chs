module Network.GRPC.Core.Time where

-- #if !MIN_VERSION_base(4,8,0)
import Control.Applicative
-- #endif
import Control.Monad
import Foreign.C.Types
import Foreign.Storable
import System.Clock
import Prelude

#include <grpc/support/time.h>

{#context prefix = "grp" #}

newtype CTimeSpec = CTimeSpec { timeSpec :: TimeSpec }

instance Storable CTimeSpec where
  sizeOf _ = {#sizeof gpr_timespec #}
  alignment _ = {#alignof gpr_timespec #}
  peek p = fmap CTimeSpec $ TimeSpec
    <$> liftM fromIntegral ({#get gpr_timespec->tv_sec #} p)
    <*> liftM fromIntegral ({#get gpr_timespec->tv_nsec #} p)
  poke p x = do
    {#set gpr_timespec.tv_sec #} p (fromIntegral $ sec $ timeSpec x)
    {#set gpr_timespec.tv_nsec #} p (fromIntegral $ nsec $ timeSpec x)

