module Gnttab where
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Word
import Util
import GnttabStub
import Struct

type GrantRef= Int32
foreign import primitive "const.DOMID_SELF" domidSelf :: Word16
foreign import primitive "const.PAGE_SIZE" pageSize :: Word32
foreign import primitive "const.GNTTABOP_setup_table" opSetupTable :: Word32
foreign import primitive "const.sizeof(grant_entry_t)" entrySize :: Word32

foreign import ccall "mini-os/gnttab.h HYPERVISOR_grant_table_op" hypervisorGrantTableOp :: Word32 -> Ptr a -> Word32 -> IO ()
foreign import ccall "mini-os/gnttab.h map_frames" mapFrames :: Ptr Word64 -> Word64 -> IO (Ptr a)

foreign import ccall "hs_put_free_entry" putFreeEntry :: GrantRef -> IO ()
foreign import ccall "set_xen_guest_handle" setXenGuestHandle :: Ptr Word64 -> Ptr Word32 -> IO Int
foreign import ccall "hs_set_gnttab_table" setGnttabTable :: Ptr a -> IO ()

foreign export ccall "init_gnttab2" initGnttab :: IO ()

nrGrantFrames :: Word32
nrGrantFrames = 4
nrReservedEntries = 8
nrGrantEntries = nrGrantFrames * pageSize `div` entrySize

-- NR_GRANT_ENTRIES (NR_GRANT_FRAMES * PAGE_SIZE / sizeof(grant_entry_t))
initGnttab :: IO ()
initGnttab = do mapM_ (putFreeEntry.fromInteger.toInteger) [nrReservedEntries..nrGrantFrames-1]
                let setup = GnttabSetupTable {
                  gnttabSetupTableDom = domidSelf,
                  gnttabSetupTableNrFrames = nrGrantFrames,
                  gnttabSetupTableStatus = 0,
                  gnttabSetupTableFrameList = nullPtr
                }
                frames <- mallocArray $ fromInteger $ toInteger nrGrantFrames
                -- todo: pass a pointer
                setXenGuestHandle (gnttabSetupTableFrameList setup) frames
                ptr <- toC setup
                hypervisorGrantTableOp opSetupTable ptr 1
                grantTable <- mapFrames (castPtr frames) $ fromInteger $ toInteger nrGrantFrames
                setGnttabTable grantTable
                printk $ "gnttab_table mapped at" ++ show grantTable ++ ".\n"
{-
void
init_gnttab(void)
{
    struct gnttab_setup_table setup;
    unsigned long frames[NR_GRANT_FRAMES];
    int i;

#ifdef GNT_DEBUG
    memset(inuse, 1, sizeof(inuse));
#endif
    for (i = NR_RESERVED_ENTRIES; i < NR_GRANT_ENTRIES; i++)
        put_free_entry(i);

    setup.dom = DOMID_SELF;
    setup.nr_frames = NR_GRANT_FRAMES;
    set_xen_guest_handle(setup.frame_list, frames);

    HYPERVISOR_grant_table_op(GNTTABOP_setup_table, &setup, 1);
    gnttab_table = map_frames(frames, NR_GRANT_FRAMES);
    printk("gnttab_table mapped at %p.\n", gnttab_table);
}
-}
