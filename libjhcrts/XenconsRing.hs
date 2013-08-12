module XenconsRing(xenconsRingInit) where
import Foreign.Ptr
import Data.Word
import Foreign.C.String
import XenconsoleStub
import Xen
import Struct
import Util

foreign import ccall "console/xencons_ring.c hs_get_evtch" getEvtch :: IO Word32
foreign import ccall "console/xencons_ring.c hs_get_mfn" getMfn :: IO Word64
foreign import ccall "console/xencons_ring.c hs_mfn_to_virt" mfnToVirt :: Word64 -> IO (Ptr Word8)
foreign import ccall "console/xencons_ring.c hs_notify_daemon" notifyDaemon :: Ptr ConsfrontDev -> IO ()
foreign import ccall "console/xencons_ring.c hs_handle_input" handleInput :: IO (FunPtr (IO ()))
foreign export ccall "hs_xencons_ring_init" xenconsRingInit :: IO (Ptr Word8)

xenconsRingInit = do evtch <- getEvtch
                     if evtch == 0 then
                       return nullPtr
                     else
                       do nodename <- newCString "device/console"
                          ring     <- mfnToVirt =<< getMfn
                          let dev = ConsfrontDev {
                                      consfrontDevNodename = nodename
                                      , consfrontDevDom     = 0
                                      , consfrontDevBackend = nullPtr
                                      , consfrontDevRingRef = 0
                                      , consfrontDevEvtchn  = evtch
                                      , consfrontDevEvents  = nullPtr
                                      , consfrontDevRing    = castPtr ring
                                    }
                          ptrDev <- toC dev
                          ptrHandleInput <- handleInput
                          err <-  bindEvtchn evtch ptrHandleInput $ castPtr ptrDev
                          if err <= 0 then
                            do printk $ "XEN console request chn bind failed " ++ show err ++ "\n"
                               return nullPtr
                          else
                            do unmaskEvtch evtch
                               notifyDaemon ptrDev
                               return nullPtr
