{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI.Monad
-- Copyright   :  (c) Bjorn Bringert 2006
-- License     :  BSD-style
--
-- Maintainer  :  John Chee <cheecheeo@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal stuff that most people shouldn't have to use.
-- This module mostly deals with the
-- internals of the CGIT monad transformer.
--
-----------------------------------------------------------------------------

module Network.CGI.Monad (
  -- * CGI monad class
  MonadCGI(..),
  -- * CGI monad transformer
  CGIT(..), CGI,
  runCGIT,
  -- * Request info
  CGIRequest(..),
  -- * Error handling
  throwCGI, catchCGI, tryCGI, handleExceptionCGI,
 ) where

import Prelude hiding ( fail )

import Control.Exception as Exception (SomeException)
import Control.Applicative (Applicative(..))
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask, throwM, catch, try, mask, uninterruptibleMask, generalBracket)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (ReaderT(..), asks)
import Control.Monad.Writer (WriterT(..), tell)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Trans (MonadTrans, MonadIO, liftIO, lift)
import Network.CGI.Protocol


--
-- * CGIT monad transformer
--

-- | A simple CGI monad with just IO.
type CGI a = CGIT IO a

-- | The CGIT monad transformer.
newtype CGIT m a = CGIT { unCGIT :: ReaderT CGIRequest (WriterT Headers m) a }

instance (Functor m) => Functor (CGIT m) where
    fmap f c = CGIT (fmap f (unCGIT c))

instance (Applicative m) => Applicative (CGIT m) where
    pure = CGIT . pure
    f <*> x = CGIT (unCGIT f <*> unCGIT x)

instance Monad m => Monad (CGIT m) where
    c >>= f = CGIT (unCGIT c >>= unCGIT . f)
    return = CGIT . return

instance MonadFail m => MonadFail (CGIT m) where
    fail = CGIT . fail

instance MonadIO m => MonadIO (CGIT m) where
    liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (CGIT m) where
    throwM = CGIT . throwM

instance MonadCatch m => MonadCatch (CGIT m) where
    CGIT m `catch` h = CGIT $ m `catch` (unCGIT . h)

instance MonadMask m => MonadMask (CGIT m) where
    mask a = CGIT $ mask $ \u -> unCGIT $ a $ CGIT . u . unCGIT
    uninterruptibleMask a = CGIT $ uninterruptibleMask $ \u -> unCGIT $ a $ CGIT . u . unCGIT
    generalBracket acquire release f = CGIT $
      generalBracket (unCGIT acquire) (\a b -> unCGIT (release a b)) (unCGIT . f)

instance MonadCatch m => MonadError SomeException (CGIT m) where
    throwError = throwM
    catchError = catch

-- | The class of CGI monads. Most CGI actions can be run in
--   any monad which is an instance of this class, which means that
--   you can use your own monad transformers to add extra functionality.
class Monad m => MonadCGI m where
    -- | Add a response header.
    cgiAddHeader :: HeaderName -> String -> m ()
    -- | Get something from the CGI request.
    cgiGet :: (CGIRequest -> a) -> m a

instance Monad m => MonadCGI (CGIT m) where
    cgiAddHeader n v = CGIT $ lift $ tell [(n,v)]
    cgiGet = CGIT . asks

instance MonadTrans CGIT where
    lift = CGIT . lift . lift

-- | Run a CGI action.
runCGIT :: Monad m => CGIT m a -> CGIRequest -> m (Headers, a)
runCGIT (CGIT c) = fmap (uncurry (flip (,))) . runWriterT . runReaderT c



--
-- * Deprecated error handling functions.
--


{-# DEPRECATED throwCGI "Use Control.Monad.Catch.throwM instead." #-}
-- | Deprecated alias for 'throwM'. Please use 'throwM' instead.
throwCGI :: (MonadThrow m) => SomeException -> m a
throwCGI = throwM

{-# DEPRECATED catchCGI "Use Control.Monad.Catch.catch instead." #-}
-- | Deprecated alias for 'catch'. Please use 'catch' instead.
catchCGI :: (MonadCatch m) => m a -> (SomeException -> m a) -> m a
catchCGI = catch

{-# DEPRECATED tryCGI "Use Control.Monad.Catch.try instead." #-}
-- | Deprecated alias for 'try'. Please use 'try' instead.
tryCGI :: (MonadCatch m) => m a -> m (Either SomeException a)
tryCGI = try

{-# DEPRECATED handleExceptionCGI "Use Control.Monad.Catch.catch instead." #-}
-- | Deprecated alias for 'catch'. Please use 'catch' instead.
handleExceptionCGI :: (MonadCatch m) => m a -> (SomeException -> m a) -> m a
handleExceptionCGI = catch
