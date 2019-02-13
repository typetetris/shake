{-# LANGUAGE ScopedTypeVariables #-}

-- | A bit like 'Fence', but not thread safe and optimised for avoiding taking the fence
module General.Thread(
    allocateThread
    ) where

import General.Cleanup
import Control.Concurrent.Extra
import Control.Exception
import General.Extra
import Control.Monad.Extra
import Data.IORef


-- | Run the given action in a separate thread.
--   On cleanup, the thread will be killed before continuing.
--   If the action raises an exception it will be rethrown on the parent thread.
allocateThread :: Cleanup -> IO () -> IO ()
allocateThread cleanup act = do
    bar <- newBarrier
    parent <- myThreadId
    ignore <- newIORef False
    void $ allocate cleanup
        (mask_ $ forkIOWithUnmask $ \unmask -> do
            res :: Either SomeException () <- try $ unmask act
            unlessM (readIORef ignore) $ whenLeft res $ throwTo parent
            signalBarrier bar ()
        )
        (\t -> do writeIORef ignore True; killThread t; waitBarrier bar)