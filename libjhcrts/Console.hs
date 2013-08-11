module Console(initConsole) where
import Util
import XenconsRing

--foreign import ccall "console/console.c hs_set_console_initialised" setConsoleInitialised :: Int -> IO ()
foreign export ccall "init_console2" initConsole :: IO ()

initConsole :: IO ()
initConsole = printk "console\n"
