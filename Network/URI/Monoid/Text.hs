module Network.URI.Monoid.Text
    (
       -- * The URI type
      URI
    , URIAuth
    , GenURI(..)
    , GenURIAuth(..)
       -- * The operations
    , module Network.URI.Monoid
    ) where

import Network.URI.Monoid
import Data.Text (Text)

type URI = GenURI Text
type URIAuth = GenURIAuth Text
