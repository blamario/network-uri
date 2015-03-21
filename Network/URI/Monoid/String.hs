module Network.URI.Monoid.String
    (
       -- * The URI type
      URI
    , URIAuth
    , GenURI(..)
    , GenURIAuth(..)
       -- * The operations
    , module Network.URI.Monoid) where

import Network.URI.Monoid

type URI = GenURI String
type URIAuth = GenURIAuth String
