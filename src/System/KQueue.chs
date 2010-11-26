{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module System.KQueue where

#include <sys/event.h>

import Control.Applicative
import Data.Bits
import Data.List
import Data.Maybe
import Foreign
import Foreign.C

-- | A kernel event queue.
newtype KQueue = KQueue CInt -- The descriptor

foreign import ccall "kqueue" kqueue_ :: IO CInt

kqueue :: IO KQueue
kqueue = KQueue <$> kqueue_

data KEvent = KEvent
  { ident    :: CULong -- TODO
  , evfilter :: Filter
  , flags    :: [Flag]
  , fflags   :: [FFlag]
  , data_    :: CLong -- TODO
  , udata    :: Ptr () -- TODO
  }

#c
enum Filter
  { EvfiltRead = EVFILT_READ
  , EvfiltWrite = EVFILT_WRITE
  , EvfiltAio = EVFILT_AIO
  , EvfiltVnode = EVFILT_VNODE
  , EvfiltProc = EVFILT_PROC
  , EvfiltSignal = EVFILT_SIGNAL
  , EvfiltTimer = EVFILT_TIMER
// Not on Mac OS X
// , EvfiltUser = EVFILT_USER
  };
#endc

{#enum Filter {}#}

#c
enum Flag
  { EvAdd      = EV_ADD
  , EvEnable   = EV_ENABLE
  , EvDisable  = EV_DISABLE
// Not on Mac OS X
//  , EvDispatch = EV_DISPATCH
  , EvDelete   = EV_DELETE
  , EvReceipt  = EV_RECEIPT
  , EvOneshot  = EV_ONESHOT
  , EvClear    = EV_CLEAR
  , EvEof      = EV_EOF
  , EvError    = EV_ERROR
  };
#endc

{#enum Flag {}#}

#c
enum FFlag
  { NoteDelete = NOTE_DELETE
  , NoteWrite  = NOTE_WRITE
  , NoteExtend = NOTE_EXTEND
  , NoteAttrib = NOTE_ATTRIB
  , NoteLink   = NOTE_LINK
  , NoteRename = NOTE_RENAME
  , NoteRevoke = NOTE_REVOKE
// Seems to have the same value as NoteDelete
//  , NoteLowat  = NOTE_LOWAT
  , NoteExit   = NOTE_EXIT
  , NoteFork   = NOTE_FORK
  , NoteExec   = NOTE_EXEC
  , NoteSignal = NOTE_SIGNAL
  , NoteReap   = NOTE_REAP
  };
#endc

{#enum FFlag {} deriving (Show, Eq) #}

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
  peek e = KEvent <$> ({#get kevent_t->ident  #} e)
                  <*> fmap (toEnum . fromIntegral) ({#get kevent_t->filter #} e)
                  <*> fmap (bitmaskToEnum . fromIntegral) ({#get kevent_t->flags  #} e)
                  <*> fmap (bitmaskToEnum . fromIntegral) ({#get kevent_t->fflags #} e)
                  <*> ({#get kevent_t->data  #} e)
                  <*> ({#get kevent_t->udata  #} e)
  poke e ev =
    do {#set kevent_t->ident#} e (ident ev)
       {#set kevent_t->filter#} e (fromIntegral . fromEnum . evfilter $ ev)
       {#set kevent_t->flags#} e (fromIntegral . enumToBitmask . flags $ ev)
       {#set kevent_t->fflags#} e (fromIntegral . enumToBitmask . fflags $ ev)
       {#set kevent_t->data#} e (data_ ev)
       {#set kevent_t->udata#} e (udata ev)

data TimeSpec

foreign import ccall "kevent" kevent_ :: CInt -> Ptr KEvent -> CInt -> Ptr KEvent -> CInt -> Ptr TimeSpec -> IO CInt

kevent :: KQueue -> [KEvent] -> Int -> TimeSpec -> IO [KEvent]
kevent (KQueue kq) changelist nevents _ = -- TODO: use timespec
  withArray changelist $ \chArray ->
  allocaArray nevents  $ \evArray ->
    do _ <- kevent_ kq chArray (fromIntegral . length $ changelist) evArray (fromIntegral nevents) nullPtr
       -- TODO: throw exception on non-0 return value.
       peekArray nevents evArray
