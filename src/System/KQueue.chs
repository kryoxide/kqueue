{-# LANGUAGE DeriveDataTypeable
           , EmptyDataDecls
           , ForeignFunctionInterface
           #-}
-- | This module contains a low-level binding to the kqueue interface.
-- It stays close to the C API, changing the types to more native
-- Haskell types, but not significantly changing it.
-- See the kqueue man page or the examples in @examples/@ for usage
-- information.
-- For a higher-level binding, see "System.KQueue.HighLevel".
module System.KQueue
  {-( KQueue
  , kqueue
  , KEvent (..)
  , Filter (..)
  , Flag (..)
  , FFlag (..)
  , kevent
  , KQueueException
  ) -}where

#include <sys/time.h>
#include <sys/event.h>
#include <sys/types.h>

import Control.Applicative ( (<$>), (<*>) )
import Control.Exception   ( Exception, throwIO )
import Data.List           ( foldl' )
import Data.Maybe          ( mapMaybe )
import Data.Time.Clock     ( NominalDiffTime )
import Data.Typeable       ( Typeable )
import Foreign             ( (.|.)
                           , Ptr
                           , Storable (..)
                           , allocaArray
                           , bit
                           , bitSize
                           , maybeWith
                           , testBit
                           , peekArray
                           , with
                           , withArray
                           )
#if __GLASGOW_HASKELL__ >= 704
import Foreign.C           ( CInt (..) )
#else
import Foreign.C           ( CInt )
#endif
import Foreign.C           ( CLong
                           , CShort
                           , CTime
                           , CUInt
                           , CULong
                           , CUShort
                           )

-- | A kernel event queue.
newtype KQueue = KQueue CInt -- The descriptor

-- | Create a new KQueue.
kqueue :: IO KQueue
kqueue = KQueue <$> {#call kqueue as kqueue_ #}

-- | A kernel event.
data KEvent = KEvent
  { ident    :: CULong  -- ^ The identifier for the event, often a file descriptor.
  , evfilter :: Filter  -- ^ The kernel filter (≈btype of event).
  , flags    :: [Flag]  -- ^ Actions to perform on the event.
  , fflags   :: [FFlag] -- ^ Filter-specific flags.
  , data_    :: CLong   -- ^ Filter-specific data value.
  , udata    :: Ptr ()  -- ^ User-defined data, passed through unchanged.
  } deriving (Show, Eq)

-- TODO: nicer types for ident, data_ and udata.

{#enum define Filter { EVFILT_READ   as EvfiltRead
                     , EVFILT_WRITE  as EvfiltWrite
                     , EVFILT_AIO    as EvfiltAio
                     , EVFILT_VNODE  as EvfiltVnode
                     , EVFILT_PROC   as EvfiltProc
                     , EVFILT_SIGNAL as EvfiltSignal
                     , EVFILT_TIMER  as EvfiltTimer
                     , EVFILT_USER   as EvfiltUser
                     } deriving (Show, Eq)
#}

-- | The actions to perform on the event.
{# enum define Flag { EV_ADD      as EvAdd
                    , EV_ENABLE   as EvEnable
                    , EV_DISABLE  as EvDisable
                    , EV_DISPATCH as EvDispatch
                    , EV_DELETE   as EvDelete
                    , EV_RECEIPT  as EvReceipt
                    , EV_ONESHOT  as EvOneshot
                    , EV_CLEAR    as EvClear
                    , EV_EOF      as EvEof
                    , EV_ERROR    as EvError
                    } deriving (Show, Eq)
#}

-- | The filter specific flags.
{#enum define FFlag { NOTE_DELETE as NoteDelete
                    , NOTE_WRITE  as NoteWrite
                    , NOTE_EXTEND as NoteExtend
                    , NOTE_ATTRIB as NoteAttrib
                    , NOTE_LINK   as NoteLink
                    , NOTE_RENAME as NoteRename
                    , NOTE_REVOKE as NoteRevoke
                    , NOTE_LOWAT  as NoteLowat
                    , NOTE_EXIT   as NoteExit
                    , NOTE_FORK   as NoteFork
                    , NOTE_EXEC   as NoteExec
                    , NOTE_SIGNAL as NoteSignal
                    -- NOTE_REAP is deprecated, NOTE_EXIT should be used instead
                    -- , NOTE_REAP as NoteReap
                    } deriving (Show, Eq)
#}

-- | Convert a list of enumeration values to an integer by combining
-- them with bitwise 'or'.
enumToBitmask :: Enum a => [a] -> Int
enumToBitmask = foldl' (.|.) 0 . map fromEnum

-- | Convert an integer to a list of enumeration values by testing
-- each bit, and if set, convert it to an enumeration member.
bitmaskToEnum :: Enum a => Int -> [a]
bitmaskToEnum bm = mapMaybe maybeBit [0 .. bitSize bm - 1]
  where
    maybeBit b | testBit bm b = Just . toEnum . bit $ b
               | otherwise    = Nothing

#c
typedef struct kevent kevent_t;
#endc

instance Storable KEvent where
  sizeOf _ = {#sizeof kevent_t #}
  alignment _ = 24
  peek e = KEvent <$>                                     ({#get kevent_t->ident  #} e)
                  <*> fmap (toEnum . fromIntegral)        ({#get kevent_t->filter #} e)
                  <*> fmap (bitmaskToEnum . fromIntegral) ({#get kevent_t->flags  #} e)
                  <*> fmap (bitmaskToEnum . fromIntegral) ({#get kevent_t->fflags #} e)
                  <*>                                     ({#get kevent_t->data   #} e)
                  <*>                                     ({#get kevent_t->udata  #} e)
  poke e ev =
    do {#set kevent_t->ident  #} e (ident                                   ev)
       {#set kevent_t->filter #} e (fromIntegral . fromEnum . evfilter    $ ev)
       {#set kevent_t->flags  #} e (fromIntegral . enumToBitmask . flags  $ ev)
       {#set kevent_t->fflags #} e (fromIntegral . enumToBitmask . fflags $ ev)
       {#set kevent_t->data   #} e (data_                                   ev)
       {#set kevent_t->udata  #} e (udata                                   ev)

newtype TimeSpec = TimeSpec NominalDiffTime
  deriving (Show, Eq)

#c
typedef struct timespec timespec_t;
#endc

-- TODO: waarom krijg ik geen CTime maar een CLong als seconds bij gebruik van #get/#set?
instance Storable TimeSpec where
  sizeOf _ = {#sizeof timespec_t #}
  alignment _ = 8
  peek t =  mkTimeSpec
        <$> (\ptr -> peekByteOff ptr 0 :: IO CTime)  t
        <*> {#get timespec_t->tv_nsec #} t
    where
      mkTimeSpec s ns = TimeSpec $ realToFrac s + realToFrac ns/1000000000
  poke t (TimeSpec dt) =
    do (\ptr val -> pokeByteOff ptr 0 (val :: CTime)) t (fromInteger s)
       {#set timespec_t->tv_nsec #} t (floor . (* 1000000000) $ ns)
    where
      (s, ns) = properFraction dt

foreign import ccall "kevent" kevent_ :: CInt -> Ptr KEvent -> CInt -> Ptr KEvent -> CInt -> Ptr TimeSpec -> IO CInt

data KQueueException = KQueueException
  deriving (Show, Typeable)

instance Exception KQueueException

-- | Add events to monitor, or retrieve events from the kqueue. If an
-- error occurs, will throw a 'KQueueException' if there is no room in
-- the returned event list. Otherwise, will set 'EvError' on the event
-- and add it to the returned event list.
kevent ::  KQueue               -- ^ The kernel queue to operate on.
       -> [KEvent]              -- ^ The list of events to start monitoring, or changes to retrieve.
       -> Int                   -- ^ The maximum number of events to retrieve.
       -> Maybe NominalDiffTime -- ^ Timeout. When nothing, blocks until an event has occurred.
       -> IO [KEvent]           -- ^ A list of events that have occurred.
kevent (KQueue kq) changelist nevents mtimeout =
  withArray changelist $ \chArray ->
  allocaArray nevents  $ \evArray ->
  maybeWith with (TimeSpec <$> mtimeout) $ \timeout ->
    do ret <- kevent_ kq chArray (fromIntegral . length $ changelist) evArray (fromIntegral nevents) timeout
       case ret of
         -- Error while processing changelist, and no room in return array.
         -1 -> throwIO KQueueException
         -- Timeout.
         0  -> return []
         -- Returned n events. Can contain errors. The change that
         -- failed will be in the event list. EV_ERROR will be set on the
         -- event.
         n  -> peekArray (fromIntegral n) evArray
