-- |
-- In order to make the `browser-cookies' library easier to use, we export
-- an "unwrapped" CookieOpts type, along with a default value which
-- can have fields easily overridden.p

module Calculator.Cookies
  ( setCookie, defaultCookieOpts
  , Opts
  , module CC, module CD
  )
where

import Prelude

import Data.Maybe ( Maybe(..) )
import Data.JSDate ( JSDate )

import Effect ( Effect )

import Browser.Cookie as C
import Browser.Cookie ( getCookies, getCookie, removeCookie ) as CC
import Browser.Cookies.Data ( Cookie, CookieOpts(..), SameSite(..), SetCookie(..) )
import Browser.Cookies.Data ( Cookie(..) ) as CD

type Opts =
  { domain :: Maybe String
  , expires :: Maybe JSDate
  , httpOnly :: Boolean
  , maxAge :: Maybe Number
  , path :: Maybe String
  , samesite :: Maybe SameSite
  , secure :: Boolean
  }

setCookie :: Cookie -> Opts -> Effect Unit
setCookie cookie opts =
  C.setCookie $ SetCookie { cookie, opts: Just (CookieOpts opts) }

-- TODO: Set domains and paths on the default cookie opts

defaultCookieOpts :: Opts
defaultCookieOpts =
  { domain: Nothing
  , expires: Nothing
  , httpOnly: false
  , maxAge: Just 3600.0
  , path: Nothing
  , samesite: Just Strict
  , secure: false
  }
