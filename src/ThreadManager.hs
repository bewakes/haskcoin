module ThreadManager
    (
      ThreadManager
    , newManager
    , forkManaged
    , getStatus
    , waitFor
    , waitAll
    ) where

import Control.Concurrent
import Control.Exception (Exception, try, SomeException)
import qualified Data.Map as M

data ThreadStatus = Running
                  | Finished
                  | Threw String
                    deriving (Eq, Show)

newtype ThreadManager =
    Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))

-- | Create a new thread manager.
newManager :: IO ThreadManager
newManager = Mgr `fmap` newMVar M.empty

-- | Create a new managed Thread.
forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body =
    modifyMVar mgr $ \m -> do
        state <- newEmptyMVar
        tid <- forkIO $ do
            result <- try body
            putMVar state (either (Threw . (\x-> show (x:: SomeException) )) (const Finished) result)
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

-- | Block until a specific managed thread terminates.
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid = do
    maybeDone <- modifyMVar mgr $ \m ->
        return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
            (Nothing, _) -> (m, Nothing)
            (done, m') -> (m', done)
    case maybeDone of
        Nothing -> return Nothing
        Just st -> Just `fmap` takeMVar st

-- | Block until all managed threads terminate.
waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
    where elems m = return (M.empty, M.elems m)

main = do
    m <- newEmptyMVar
    forkIO $ do
        v <- takeMVar m
        putStrLn ("Received: "++ show v)
    putStrLn "sending"
    putMVar m "wake up!!"
