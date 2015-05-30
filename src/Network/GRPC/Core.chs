{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.GRPC.Core where

-- TODO Remove wrapped function once  once https://github.com/haskell/c2hs/issues/117 gets in

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, malloc)
import Foreign.Marshal.Array (peekArray, pokeArray, mallocArray)
import Foreign.Ptr
import Foreign.Storable

import Network.GRPC.Core.Time

#include <grpc/grpc.h>
#include <grpc/status.h>
#include <grpc_haskell.h>

{#context prefix = "grpc" #}

{#pointer *gpr_timespec as CTimeSpecPtr -> CTimeSpec #}
{#enum grpc_status_code as StatusCode {underscoreToCase} deriving (Eq)#}

{#pointer *grpc_completion_queue as CompletionQueue newtype #}
{#pointer *grpc_channel as Channel newtype #}
{#pointer *grpc_server as Server newtype #}
{#pointer *grpc_call as Call newtype #}

deriving instance Storable Call

data ChannelArgs = ChannelArgs [Arg] 
instance Storable ChannelArgs where
  sizeOf _ = {#sizeof grpc_channel_args  #}
  alignment _ = {#alignof grpc_channel_args  #}
  peek p = do
    size  <- {#get grpc_channel_args->num_args #} p
    p'    <- {#get grpc_channel_args->args #} p
    xs    <- peekArray (fromIntegral size) (castPtr p')
    return $ ChannelArgs xs
  poke p (ChannelArgs xs) = do
    {#set grpc_channel_args.num_args #} p (fromIntegral $ length xs)
    -- TODO When does that allocation sholud be freed? - After channel creation, when freeing the whole array of elements.
    p' <- mallocArray $ length xs
    pokeArray p' xs
    {#set grpc_channel_args.args #} p (castPtr p')
    return ()
    
{#pointer *grpc_channel_args as ChannelArgsPtr -> `ChannelArgs' #}

data Arg = Arg { argKey :: CChar, argValue :: ArgVal }
data ArgVal = ArgValS CChar | ArgValI CInt | ArgValP (Ptr ())

instance Storable Arg where
  sizeOf _ = {#sizeof grpc_arg  #}
  alignment _ = {#alignof grpc_arg  #}
  peek p = do
    tpe   <- {#get grpc_arg->type #} p
    pkey  <- {#get grpc_arg->key #} p
    key   <- peek pkey
    value <- case tpe of
      1 -> do
        p' <- {#get grpc_arg->value.string #} p
        s  <- peek p'
        return $ ArgValS s
      2 -> ArgValI <$> {#get grpc_arg->value.integer #} p
      3 -> ArgValP <$> {#get grpc_arg->value.pointer.p #} p
      _ -> error "Unsupported argument type"
    return $ Arg key value
  poke p (Arg key value) =
    case value of
      ArgValS s -> do
        pokeHeader 1
        ps <- malloc -- TODO FREE !
        _  <- poke ps s
        {#set grpc_arg->value.string #} p ps
      ArgValI i -> do
        pokeHeader 2
        {#set grpc_arg->value.integer #} p i
      ArgValP p -> do
        {#set grpc_arg->value.pointer.p #} p p
    where
      pokeHeader tpe = do
        {#set grpc_arg->type #} p tpe
        ps <- malloc -- TODO FREE !
        _  <- poke ps key
        {#set grpc_arg->key #} p ps
      
{#enum grpc_call_error as CallError {underscoreToCase} deriving (Eq)#}
{#enum grpc_op_error as OpError {underscoreToCase} deriving (Eq)#}

{#pointer *grpc_byte_buffer as ByteBuffer newtype #}
{#pointer *grpc_byte_buffer_reader as ByteBufferReader newtype #}

{#enum grpc_completion_type as CompletionType {underscoreToCase} deriving (Eq)#}
{#pointer *grpc_event as Event newtype #}

{#pointer *grpc_metadata as MetadataPtr -> Metadata #}

data Metadata = Metadata
  { key   :: String
  , value :: String }

{#pointer *grpc_metadata_array as MetadatasPtr -> `[Metadata]' #}

{#pointer *grpc_call_details as CallDetailsPtr -> CallDetails #}

data CallDetails = CallDetails
  { method    :: String
  , host      :: String
  , deadline  :: CTimeSpec }

{#enum grpc_op_type as OpType {underscoreToCase} deriving (Eq)#}
{#pointer *grpc_op as Op newtype #}

{#fun grpc_init as ^ {} -> `()'#}
{#fun grpc_shutdown as ^ {} -> `()'#}

{#fun grpc_completion_queue_create as ^ {} -> `CompletionQueue'#}

{#fun grpc_completion_queue_next_ as ^ {`CompletionQueue', `CTimeSpecPtr'} -> `Event'#}
{#fun grpc_completion_queue_pluck_ as ^ {`CompletionQueue', `Ptr ()'} -> `Event'#}

{#fun grpc_event_finish as ^ {`Event'} -> `()'#}

{#fun grpc_completion_queue_shutdown as ^ {`CompletionQueue'} -> `()'#}
{#fun grpc_completion_queue_destroy as ^ {`CompletionQueue'} -> `()'#}

{#fun grpc_channel_create_call_ as ^ {`Channel', `CompletionQueue', `String', `String', `CTimeSpecPtr'} -> `Call'#}
{#fun grpc_channel_create as ^ {`String', `ChannelArgsPtr'} -> `Channel'#}
{#fun grpc_channel_destroy as ^ {`Channel'} -> `()'#}

{#fun grpc_call_start_batch as ^ {`Call', `Op', `Int', `Ptr ()'} -> `CallError'#}
{#fun grpc_call_cancel as ^ {`Call'} -> `()'#}
{#fun grpc_call_cancel_with_status as ^ {`Call', `StatusCode', `String'} -> `()'#}
{#fun grpc_call_destroy as ^ {`Call'} -> `()'#}

{#fun grpc_server_request_call as ^ 
  {`Server', alloca- `Call' peek*, `CallDetailsPtr', `MetadatasPtr', `CompletionQueue', `Ptr ()'} 
  -> `CallError'#}
