-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI.Cookie
-- Copyright   :  (c) Bjorn Bringert 2004-2005
--                (c) Ian Lynagh 2005
-- License     :  BSD-style
--
-- Maintainer  :  John Chee <cheecheeo@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
--  General server side HTTP cookie library.
--  Based on <http://wp.netscape.com/newsref/std/cookie_spec.html>
--
-- TODO
--
-- * Add client side stuff (basically parsing Set-Cookie: value)
--
-- * Update for RFC2109 <http://www.ietf.org/rfc/rfc2109.txt>
--
-----------------------------------------------------------------------------
module Network.CGI.Cookie (
                            Cookie(..)
                            , newCookie
                            , findCookie, deleteCookie
                            , showCookie, readCookies
                           ) where

import Data.Char (isSpace)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (defaultTimeLocale, formatTime, rfc822DateFormat)

--
-- * Types
--

-- | Contains all information about a cookie set by the server.
data Cookie = Cookie {
                      -- | Name of the cookie.
                      cookieName :: String,
                      -- | Value of the cookie.
                      cookieValue :: String,
                      -- | Expiry date of the cookie. If 'Nothing', the
                      --   cookie expires when the browser sessions ends.
                      --   If the date is in the past, the client should
                      --   delete the cookie immediately.
                      cookieExpires :: Maybe UTCTime,
                      -- | The domain suffix to which this cookie will be sent.
                      cookieDomain :: Maybe String,
                      -- | The path to which this cookie will be sent.
                      cookiePath :: Maybe String,
                      -- | 'True' if this cookie should only be sent using
                      --   secure means.
                      cookieSecure :: Bool,
                      -- | 'True' to tell the client's browser to prevent 
                      --   client side scripts from accessing the cookie.
                      cookieHttpOnly :: Bool
                     }
            deriving (Show, Read, Eq, Ord)

--
-- * Constructing cookies
--

-- | Construct a cookie with only name and value set.
--   This client will expire when the browser sessions ends,
--   will only be sent to the server and path which set it
--   and may be sent using any means.
newCookie :: String -- ^ Name
          -> String -- ^ Value
          -> Cookie -- ^ Cookie
newCookie name value = Cookie { cookieName = name,
                                cookieValue = value,
                                cookieExpires = Nothing,
                                cookieDomain = Nothing,
                                cookiePath = Nothing,
                                cookieSecure = False,
                                cookieHttpOnly = False
                              }

--
-- * Getting and setting cookies
--

-- | Get the value of a cookie from a string on the form
--   @\"cookieName1=cookieValue1;...;cookieName2=cookieValue2\"@.
--   This is the format of the @Cookie@ HTTP header.
findCookie :: String -- ^ Cookie name
           -> String -- ^ Semicolon separated list of name-value pairs
           -> Maybe String  -- ^ Cookie value, if found
findCookie name s = maybeLast [ cv | (cn,cv) <- readCookies s, cn == name ]

-- | Delete a cookie from the client by setting the cookie expiry date
--   to a date in the past.
deleteCookie :: Cookie  -- ^ Cookie to delete. The only fields that matter
                        --   are 'cookieName', 'cookieDomain' and 'cookiePath'
             -> Cookie
deleteCookie c = c { cookieExpires = Just epoch }
    where
    epoch = UTCTime (ModifiedJulianDay 40587) 0

--
-- * Reading and showing cookies
--

-- | Show a cookie on the format used as the value of the Set-Cookie header.
showCookie :: Cookie -> String
showCookie c = concat $ intersperse "; " $
                showPair (cookieName c) (cookieValue c)
                 : catMaybes [expires, path, domain, secure, httpOnly]
    where expires = fmap (showPair "expires" . dateFmt) (cookieExpires c)
          domain = fmap (showPair "domain") (cookieDomain c)
          path = fmap (showPair "path") (cookiePath c)
          secure = if cookieSecure c then Just "secure" else Nothing
          httpOnly = if cookieHttpOnly c then Just "HttpOnly" else Nothing
          dateFmt = formatTime defaultTimeLocale rfc822DateFormat

-- | Show a name-value pair. FIXME: if the name or value
--   contains semicolons, this breaks. The problem
--   is that the original cookie spec does not mention
--   how to do escaping or quoting.
showPair :: String -- ^ name
         -> String -- ^ value
         -> String
showPair name value = name ++ "=" ++ value


-- | Gets all the cookies from a Cookie: header value
readCookies :: String             -- ^ String to parse
            -> [(String,String)]  -- ^ Cookie name - cookie value pairs
readCookies s =
    let (xs,ys) = break (=='=') (dropWhile isSpace s)
        (zs,ws) = break (==';') (dropWhile isSpace (drop 1 ys))
     in if null xs then [] else (xs,zs):readCookies (drop 1 ws)

--
-- Utilities
--

-- | Return 'Nothing' is the list is empty, otherwise return
--   the last element of the list.
maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just (last xs)
