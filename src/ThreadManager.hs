module NiceFork
    (
      ThreadManager
    , newManager
    , forkManged
    , getStatus
    , waitFor
    , waitAll
    ) where

import Control.Concurrent
import Control.Exception (Exception, try)
import qualified Data.Map as M

data ThreadStatus = Running
                  | Finished
                  | Threw Exception
                    deriving (Eq, Show)

newtype ThreadManager =
    Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))

-- | Create a new thread manager.
newManager :: IO ThreadManager
newManger = Mgr `fmap` newMVar M.empty

-- | Create a new managed Thread.
forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body =
    modifyMVar mgr $ \m -> do
        state <- newEmptyMVar
        tid <- forkIO $ do
            result <- try body
            putMVar state (either Threw (const Finished) result)
        return (M.insert tid state m, tid)

-- | Immediately return the status of a managed thread.
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) tid =
    modifyMVar mgr $ \m ->
        case M.lookup tid m of
            Nothing -> return (m, Nothing)
            Just st -> tryTakeMVar st >>= \mst -> case mst of
                        Nothing -> return (m, Just Running)
                        Just sth -> return (M.delete tid m, Just sth)

-- | Block untill a specific managed thread terminates.
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

-- | Block until a specific managed thread terminate.
waitAll :: ThreadManager -> IO ()

main = do
    m <- newEmptyMVar
    forkIO $ do
        v <- takeMVar m
        putStrLn ("Received: "++ show v)
    putStrLn "sending"
    putMVar m "wake up!!"
