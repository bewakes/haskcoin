import Network.Socket

getAndSendto socket = do
    a <- getLine
    case a of
        "exit" -> do
            sClose socket
            return ""
        otherwise -> do
            send socket a
            getAndSendto socket

main :: IO ()
main = do
        (server:_) <- getAddrInfo Nothing (Just "localhost") (Just "3000")
        s <- socket (addrFamily server) Datagram defaultProtocol
        connect s (addrAddress server)
        getAndSendto s
        sClose s
