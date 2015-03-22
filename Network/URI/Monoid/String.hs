module Network.URI.Monoid.String
    (
       -- * The URI type
      URI
    , URIAuth
    , GenURI(..)
    , GenURIAuth(..)
       -- * The operations
    , module Network.URI.Monoid

    -- * Deprecated functions
    , parseabsoluteURI
    , escapeString
    , reserved, unreserved
    , scheme, authority, path, query, fragment
    ) where

import Network.URI.Monoid

type URI = GenURI String
type URIAuth = GenURIAuth String

------------------------------------------------------------
--  Deprecated functions
------------------------------------------------------------

{-# DEPRECATED parseabsoluteURI "use parseAbsoluteURI" #-}
parseabsoluteURI :: String -> Maybe URI
parseabsoluteURI = parseAbsoluteURI

{-# DEPRECATED escapeString "use escapeURIString, and note the flipped arguments" #-}
escapeString :: String -> (Char->Bool) -> String
escapeString = flip escapeURIString

{-# DEPRECATED reserved "use isReserved" #-}
reserved :: Char -> Bool
reserved = isReserved

{-# DEPRECATED unreserved "use isUnreserved" #-}
unreserved :: Char -> Bool
unreserved = isUnreserved

--  Additional component access functions for backward compatibility

{-# DEPRECATED scheme "use uriScheme" #-}
scheme :: URI -> String
scheme = orNull init . uriScheme

{-# DEPRECATED authority "use uriAuthority, and note changed functionality" #-}
authority :: URI -> String
authority = maybe "" uriAuthToString . uriAuthority
   where uriAuthToString a = uriUserInfo a ++ uriRegName a ++ uriPort a

{-# DEPRECATED path "use uriPath" #-}
path :: URI -> String
path = uriPath

{-# DEPRECATED query "use uriQuery, and note changed functionality" #-}
query :: URI -> String
query = orNull tail . uriQuery

{-# DEPRECATED fragment "use uriFragment, and note changed functionality" #-}
fragment :: URI -> String
fragment = orNull tail . uriFragment

orNull :: ([a]->[a]) -> [a] -> [a]
orNull _ [] = []
orNull f as = f as
