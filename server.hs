import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
import Data.Char
import Data.List
import Text.Read
import Data.Maybe (fromJust)
 
type Msg = (Int, String)

sep = '\0'

main :: IO ()
main = do
    chan <- newChan
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 1280 iNADDR_ANY)
    listen sock 2
    forkIO $ fix $ \loop -> do
        (_, msg) <- readChan chan
        loop
    mainLoop sock chan 0
 
mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan nr = do
    conn <- accept sock
    forkIO (runConn conn chan nr)
    mainLoop sock chan $! nr+1
 
runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan nr = do
    let broadcast msg = writeChan chan (nr, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Salve! What's your name?"
    name <- liftM init (hGetLine hdl)
    hPutStrLn hdl ("Ave, " ++ name ++ "! What is your cipher key?")
    keystring <- liftM init (hGetLine hdl)
    let key = case readMaybe keystring :: Maybe Int of
                Just n  -> n
                Nothing -> romanToInt keystring
    chan' <- dupChan chan
    reader <- forkIO $ fix $ \loop -> do
        (nr', line) <- readChan chan'
        when (nr /= nr') $ hPutStrLn hdl (splitAndDecode key line)
        loop
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- liftM init (hGetLine hdl)
        case line of
         "quit" -> hPutStrLn hdl "Bye!"
         _      -> do
            broadcast (name ++ sep:(encode key line))
            loop
    killThread reader
    broadcast ("<-- " ++ name ++ " left.")
    hClose hdl

specialChars :: String
specialChars = [chr v | v <- ([0..64] ++ [91..96] ++ [123..150])]

drop',take' :: Int -> ([a]->[a])
drop' n = drop (abs n)
take' n = take (abs n)

encode :: Int -> String -> String
encode n string =
  [if n >= 0 then b else a | 
  s <- string, 
  (a,b) <- zip (['a'..'z'] ++ ['A'..'Z'] ++ specialChars) 
  (drop' n ['a'..'z'] ++ take' n ['a'..'z'] ++ drop' n ['A'..'Z'] ++ 
  take' n ['A'..'Z'] ++ specialChars), (if n >= 0 then a else b) == (s)]

decode :: Int -> String -> String
decode n = encode (-n)

splitAndDecode :: Int -> String -> String
splitAndDecode n string =
    case elemIndex sep string of
        Just i  -> name ++ ": " ++ decode n msg
                   where (name, sep:msg) = splitAt i string
        Nothing -> error "Could not parse message."

romanToInt :: String -> Int
romanToInt = fst
    . foldr (\p (t,s) -> if p >= s then (t+p,p) else (t-p,p)) (0,0)
    . map (fromJust . flip lookup (zip "IVXLCDM" [1,5,10,50,100,500,1000]))
