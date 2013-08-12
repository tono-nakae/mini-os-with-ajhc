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
foreign import ccall "console/xencons_ring.c hs_handle_input" handleInput :: IO (FunPtr (EvtchnPort -> Ptr Word8 -> Ptr Word8 -> IO ()))
foreign import ccall "console/console.c xencons_tx" xenconsTx :: IO ()
foreign import ccall "console/console.c xencons_rx" xenconsRx :: CString -> Word32 -> Ptr Word8 -> IO ()
foreign import ccall "hs_mb" mb :: IO ()
foreign import capi  "MASK_XENCONS_IDX" maskXenconsIdx :: Word32 -> Ptr a -> Word32

foreign export ccall "hs_xencons_ring_init" xenconsRingInit :: IO (Ptr Word8)
foreign export ccall "hs__handle_input" _andleInput :: EvtchnPort -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import unsafe ccall "&hs__handle_input" ptrHandleInput :: FunPtr (IO())

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
--                          ptrHandleInput <- handleInput
                          err <-  bindEvtchn evtch ptrHandleInput $ castPtr ptrDev
                          if err <= 0 then
                            do printk $ "XEN console request chn bind failed " ++ show err ++ "\n"
                               return nullPtr
                          else
                            do unmaskEvtch evtch
                               notifyDaemon ptrDev
                               return nullPtr

{-static inline struct xencons_interface *xencons_interface(void)
{
    return mfn_to_virt(start_info.console.domU.mfn);
} -}

xenconsInterface :: IO (Ptr XenconsInterface)
xenconsInterface = castPtr `fmap` (mfnToVirt =<< getMfn)

_andleInput :: EvtchnPort -> Ptr Word8 -> Ptr Word8 -> IO ()
_andleInput port regs arg =
  do let dev = castPtr arg
     ptrIntf <- xenconsInterface
     intf <- fromC ptrIntf
     let cons = xenconsInterfaceInCons intf
     let prod = xenconsInterfaceInProd intf
     mb
     loop cons prod intf
     mb
     setXenconsInterfaceInCons ptrIntf cons
     notifyDaemon dev
     xenconsTx
     return ()
  where loop cons prod intf =
          if cons == prod then
            return ()
          else
            do let in_ = xenconsInterfaceIn_ intf
               let idx = fromInteger $ toInteger $ maskXenconsIdx cons in_
               xenconsRx (in_ `plusPtr` idx) 1 regs
               loop (cons+1) prod intf

{-
 -
 -static void handle_input(evtchn_port_t port, struct pt_regs *regs, void *data)
{
	struct consfront_dev *dev = (struct consfront_dev *) data;
struct xencons_interface *intf = xencons_interface();
	XENCONS_RING_IDX cons, prod;

	cons = intf->in_cons;
	prod = intf->in_prod;
	mb();
	BUG_ON((prod - cons) > sizeof(intf->in));

	while (cons != prod) {
		xencons_rx(intf->in+MASK_XENCONS_IDX(cons,intf->in), 1, regs);
		cons++;
	}

	mb();
	intf->in_cons = cons;
	notify_daemon(dev);
	xencons_tx();
}
 - -}
