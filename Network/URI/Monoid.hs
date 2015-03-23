{-# LANGUAGE Haskell2010, CPP, DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
--  Module      :  Network.URI.Monoid
--  Copyright   :  (c) 2004, Graham Klyne
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Graham Klyne <gk@ninebynine.org>
--  Stability   :  provisional
--  Portability :  portable
--
--  This module is a generalization of "Network.URI". It defines the same functions for handling generic URIs over any
--  base type that instantiates the 'TextualMonoid' class.
--
--------------------------------------------------------------------------------

module Network.URI.Monoid
    (
    -- * The URI type
      GenURI(..)
    , GenURIAuth(..)
    , nullURI

    -- * Parsing
    , parseURI
    , parseURIReference
    , parseRelativeReference
    , parseAbsoluteURI

    -- * Test for strings containing various kinds of URI
    , isURI
    , isURIReference
    , isRelativeReference
    , isAbsoluteURI
    , isIPv6address
    , isIPv4address

    -- * Predicates
    , uriIsAbsolute
    , uriIsRelative

    -- * Relative URIs
    , relativeTo
    , nonStrictRelativeTo
    , relativeFrom

    -- * Operations on URI strings
    -- | Support for putting strings into URI-friendly
    --   escaped format and getting them back again.
    --   This can't be done transparently in all cases, because certain
    --   characters have different meanings in different kinds of URI.
    --   The URI spec [3], section 2.4, indicates that all URI components
    --   should be escaped before they are assembled as a URI:
    --   \"Once produced, a URI is always in its percent-encoded form\"
    , uriToString
    , isReserved, isUnreserved
    , isAllowedInURI, isUnescapedInURI
    , isUnescapedInURIComponent
    , escapeURIChar
    , escapeURIString
    , unEscapeString

    -- * URI Normalization functions
    , normalizeCase
    , normalizeEscape
    , normalizePathSegments
    ) where

import Prelude hiding (concatMap, elem, map, null, reverse)

import Data.Picoparsec
    ( Parser
    , parseOnly, (<?>), try
    , option, many1, count
    , char, satisfyChar, string, endOfInput
    , takeCharsWhile1
    )
import Data.Picoparsec.Combinator (notFollowedBy)

import Control.Applicative
import Control.Monad (void)
import Data.Traversable (sequenceA)
import Control.DeepSeq (NFData(rnf), deepseq)
import Data.Char (ord, chr, isHexDigit, toLower, toUpper, digitToInt)
import Data.Bits ((.&.),shiftR)
import Numeric (showIntAtBase)
import qualified Data.ByteString as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.List as List
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (Monoid, mconcat, mempty, (<>))
import Data.Monoid.Cancellative (stripPrefix)
import Data.Monoid.Null (MonoidNull, null)
import Data.Monoid.Factorial (reverse)
import Data.Monoid.Textual (TextualMonoid, break_, characterPrefix, concatMap, elem, map, mapAccumL, singleton, span_, 
                            split, splitCharacterPrefix)
import Data.Monoid.Instances.ByteString.UTF8 (ByteStringUTF8(..))
import Data.String (IsString, fromString)

import Data.Typeable (Typeable)
#if MIN_VERSION_base(4,0,0)
import Data.Data (Data)
#else
import Data.Generics (Data)
#endif

#if MIN_VERSION_base(4,6,0)
import GHC.Generics (Generic)
#else
#endif

------------------------------------------------------------
--  The URI datatype
------------------------------------------------------------

-- |Represents a general universal resource identifier using
--  its component parts.
--
--  For example, for the URI
--
--  >   foo://anonymous@www.haskell.org:42/ghc?query#frag
--
--  the components are:
--
data GenURI s = URI
    { uriScheme     :: s                    -- ^ @foo:@
    , uriAuthority  :: Maybe (GenURIAuth s) -- ^ @\/\/anonymous\@www.haskell.org:42@
    , uriPath       :: s                    -- ^ @\/ghc@
    , uriQuery      :: s                    -- ^ @?query@
    , uriFragment   :: s                    -- ^ @#frag@
#if MIN_VERSION_base(4,6,0)
    } deriving (Eq, Ord, Typeable, Data, Generic)
#else
    } deriving (Eq, Ord, Typeable, Data)
#endif

instance NFData s => NFData (GenURI s) where
    rnf (URI s a p q f)
        = s `deepseq` a `deepseq` p `deepseq` q `deepseq` f `deepseq` ()

-- |Type for authority value within a URI
data GenURIAuth s = URIAuth
    { uriUserInfo   :: s           -- ^ @anonymous\@@
    , uriRegName    :: s           -- ^ @www.haskell.org@
    , uriPort       :: s           -- ^ @:42@
    } deriving (Eq, Ord, Show, Typeable, Data)

instance NFData s => NFData (GenURIAuth s) where
    rnf (URIAuth ui rn p) = ui `deepseq` rn `deepseq` p `deepseq` ()

-- |Blank URI
nullURI :: Monoid s => GenURI s
nullURI = URI
    { uriScheme     = mempty
    , uriAuthority  = Nothing
    , uriPath       = mempty
    , uriQuery      = mempty
    , uriFragment   = mempty
    }

--  URI as instance of Show.  Note that for security reasons, the default
--  behaviour is to suppress any userinfo field (see RFC3986, section 7.5).
--  This can be overridden by using uriToString directly with first
--  argument @id@ (noting that this returns a ShowS value rather than a string).
--
--  [[[Another design would be to embed the userinfo mapping function in
--  the URIAuth value, with the default value suppressing userinfo formatting,
--  but providing a function to return a new URI value with userinfo
--  data exposed by show.]]]
--
instance (Eq s, Show s, TextualMonoid s) => Show (GenURI s) where
    showsPrec _ u = List.unfoldr splitCharacterPrefix . uriToString defaultUserInfoMap u . fromString

defaultUserInfoMap :: (Eq s, TextualMonoid s) => s -> s
defaultUserInfoMap uinf = user<>newpass
    where
        (user,pass) = break_ False (==':') uinf
        newpass     = if null pass || (pass == "@")
                                   || (pass == ":@")
                        then pass
                        else ":...@"

------------------------------------------------------------
--  Parse a URI
------------------------------------------------------------

-- |Turn a string containing a URI into a 'URI'.
--  Returns 'Nothing' if the string is not a valid URI;
--  (an absolute URI with optional fragment identifier).
--
--  NOTE: this is different from the previous network.URI,
--  whose @parseURI@ function works like 'parseURIReference'
--  in this module.
--
parseURI :: (Show s, TextualMonoid s) => s -> Maybe (GenURI s)
parseURI = parseURIAny uri

-- |Parse a URI reference to a 'URI' value.
--  Returns 'Nothing' if the string is not a valid URI reference.
--  (an absolute or relative URI with optional fragment identifier).
--
parseURIReference :: (Show s, TextualMonoid s) => s -> Maybe (GenURI s)
parseURIReference = parseURIAny uriReference

-- |Parse a relative URI to a 'URI' value.
--  Returns 'Nothing' if the string is not a valid relative URI.
--  (a relative URI with optional fragment identifier).
--
parseRelativeReference :: (Show s, TextualMonoid s) => s -> Maybe (GenURI s)
parseRelativeReference = parseURIAny relativeRef

-- |Parse an absolute URI to a 'URI' value.
--  Returns 'Nothing' if the string is not a valid absolute URI.
--  (an absolute URI without a fragment identifier).
--
parseAbsoluteURI :: (Show s, TextualMonoid s) => s -> Maybe (GenURI s)
parseAbsoluteURI = parseURIAny absoluteURI

-- |Test if string contains a valid URI
--  (an absolute URI with optional fragment identifier).
--
isURI :: (Show s, TextualMonoid s) => s -> Bool
isURI = isValidParse uri

-- |Test if string contains a valid URI reference
--  (an absolute or relative URI with optional fragment identifier).
--
isURIReference :: (Show s, TextualMonoid s) => s -> Bool
isURIReference = isValidParse uriReference

-- |Test if string contains a valid relative URI
--  (a relative URI with optional fragment identifier).
--
isRelativeReference :: (Show s, TextualMonoid s) => s -> Bool
isRelativeReference = isValidParse relativeRef

-- |Test if string contains a valid absolute URI
--  (an absolute URI without a fragment identifier).
--
isAbsoluteURI :: (Show s, TextualMonoid s) => s -> Bool
isAbsoluteURI = isValidParse absoluteURI

-- |Test if string contains a valid IPv6 address
--
isIPv6address :: (Show s, TextualMonoid s) => s -> Bool
isIPv6address = isValidParse ipv6address

-- |Test if string contains a valid IPv4 address
--
isIPv4address :: (Show s, TextualMonoid s) => s -> Bool
isIPv4address = isValidParse ipv4address

--  Helper function for turning a string into a URI
--
parseURIAny :: MonoidNull s => URIParser s (GenURI s) -> s -> Maybe (GenURI s)
parseURIAny parser uristr = case parseAll parser uristr of
        Left  _ -> Nothing
        Right u -> Just u

--  Helper function to test a string match to a parser
--
isValidParse :: MonoidNull s => URIParser s a -> s -> Bool
isValidParse parser uristr = case parseAll parser uristr of
        -- Left  e -> error (show e)
        Left  _ -> False
        Right _ -> True

parseAll :: MonoidNull s => URIParser s a -> s -> Either String a
parseAll parser uristr = parseOnly newparser uristr
    where
        newparser =
            do  { res <- parser
                ; endOfInput
                ; return res
                }

------------------------------------------------------------
--  Predicates
------------------------------------------------------------

uriIsAbsolute :: MonoidNull s => GenURI s -> Bool
uriIsAbsolute (URI {uriScheme = scheme'}) = not (null scheme')

uriIsRelative :: MonoidNull s => GenURI s -> Bool
uriIsRelative = not . uriIsAbsolute

------------------------------------------------------------
--  URI parser body based on Parsec elements and combinators
------------------------------------------------------------

--  Parser parser type.
--  Currently
type URIParser s a = Parser s a

--  RFC3986, section 2.1
--
--  Parse and return a 'pct-encoded' sequence
--
escaped :: TextualMonoid s => URIParser s s
escaped = fromString <$> sequenceA [char '%', hexDigitChar, hexDigitChar]

--  RFC3986, section 2.2
--
-- |Returns 'True' if the character is a \"reserved\" character in a
--  URI.  To include a literal instance of one of these characters in a
--  component of a URI, it must be escaped.
--
isReserved :: Char -> Bool
isReserved c = isGenDelims c || isSubDelims c

isGenDelims :: Char -> Bool
isGenDelims c = c `List.elem` ":/?#[]@"

isSubDelims :: Char -> Bool
isSubDelims c = c `List.elem` "!$&'()*+,;="

subDelims :: TextualMonoid s => URIParser s Char
subDelims = satisfyChar isSubDelims

--  RFC3986, section 2.3
--
-- |Returns 'True' if the character is an \"unreserved\" character in
--  a URI.  These characters do not need to be escaped in a URI.  The
--  only characters allowed in a URI are either \"reserved\",
--  \"unreserved\", or an escape sequence (@%@ followed by two hex digits).
--
isUnreserved :: Char -> Bool
isUnreserved c = isAlphaNumChar c || (c `List.elem` "-_.~")

unreservedChar :: TextualMonoid s => URIParser s Char
unreservedChar = satisfyChar isUnreserved

--  RFC3986, section 3
--
--   URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
--
--   hier-part   = "//" authority path-abempty
--               / path-abs
--               / path-rootless
--               / path-empty

uri :: (Show s, TextualMonoid s) => URIParser s (GenURI s)
uri =
    do  { us <- try uscheme
        -- ; ua <- option Nothing ( do { try (string "//") ; uauthority } )
        -- ; up <- upath
        ; (ua,up) <- hierPart
        ; uq <- option "" ( do { void $ char '?' ; uquery    } )
        ; uf <- option "" ( do { void $ char '#' ; ufragment } )
        ; return $ URI
            { uriScheme    = us
            , uriAuthority = ua
            , uriPath      = up
            , uriQuery     = uq
            , uriFragment  = uf
            }
        }

hierPart :: (Show s, TextualMonoid s) => URIParser s (Maybe (GenURIAuth s),s)
hierPart =
        do  { void $ try (string "//")
            ; ua <- uauthority
            ; up <- pathAbEmpty
            ; return (ua,up)
            }
    <|> do  { up <- pathAbs
            ; return (Nothing,up)
            }
    <|> do  { up <- pathRootLess
            ; return (Nothing,up)
            }
    <|> do  { return (Nothing,"")
            }

--  RFC3986, section 3.1

uscheme :: TextualMonoid s => URIParser s s
uscheme =
    do  { s <- oneThenMany alphaChar (satisfyChar isSchemeChar)
        ; void $ char ':'
        ; return $ fromString s<>":"
        }

--  RFC3986, section 3.2

uauthority :: (Show s, TextualMonoid s) => URIParser s (Maybe (GenURIAuth s))
uauthority =
    do  { uu <- option "" (try userinfo)
        ; uh <- host
        ; up <- option "" port
        ; return $ Just $ URIAuth
            { uriUserInfo = uu
            , uriRegName  = uh
            , uriPort     = up
            }
        }

--  RFC3986, section 3.2.1

userinfo :: TextualMonoid s => URIParser s s
userinfo =
    do  { uu <- many (uchar ";:&=+$,")
        ; void $ char '@'
        ; return (mconcat uu <>"@")
        }

--  RFC3986, section 3.2.2

host :: (Show s, TextualMonoid s) => URIParser s s
host = ipLiteral <|> try ipv4address <|> regName

ipLiteral :: (Show s, TextualMonoid s) => URIParser s s
ipLiteral =
    do  { void $ char '['
        ; ua <- ( ipv6address <|> ipvFuture )
        ; void $ char ']'
        ; return $ "[" <> ua <> "]"
        }
    <?> "IP address literal"

ipvFuture :: TextualMonoid s => URIParser s s
ipvFuture =
    do  { void $ char 'v'
        ; h <- hexDigitChar
        ; void $ char '.'
        ; a <- takeCharsWhile1 isIpvFutureChar
        ; return (fromString ['v',h,'.'] <> a)
        }

isIpvFutureChar :: Char -> Bool
isIpvFutureChar c = isUnreserved c || isSubDelims c || (c==';')

ipv6address :: (Show s, TextualMonoid s) => URIParser s s
ipv6address =
        try ( do
                { a2 <- count 6 h4c
                ; a3 <- ls32
                ; return $ mconcat a2 <> a3
                } )
    <|> try ( do
                { void $ string "::"
                ; a2 <- count 5 h4c
                ; a3 <- ls32
                ; return $ "::" <> mconcat a2 <> a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 0
                ; void $ string "::"
                ; a2 <- count 4 h4c
                ; a3 <- ls32
                ; return $ a1 <> "::" <> mconcat a2 <> a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 1
                ; void $ string "::"
                ; a2 <- count 3 h4c
                ; a3 <- ls32
                ; return $ a1 <> "::" <> mconcat a2 <> a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 2
                ; void $ string "::"
                ; a2 <- count 2 h4c
                ; a3 <- ls32
                ; return $ a1 <> "::" <> mconcat a2 <> a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 3
                ; void $ string "::"
                ; a2 <- h4c
                ; a3 <- ls32
                ; return $ a1 <> "::" <> a2 <> a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 4
                ; void $ string "::"
                ; a3 <- ls32
                ; return $ a1 <> "::" <> a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 5
                ; void $ string "::"
                ; a3 <- h4
                ; return $ a1 <> "::" <> a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 6
                ; void $ string "::"
                ; return $ a1 <> "::"
                } )
    <?> "IPv6 address"

opt_n_h4c_h4 :: TextualMonoid s => Int -> URIParser s s
opt_n_h4c_h4 n = option "" $
    do  { a1 <- countMinMax 0 n h4c
        ; a2 <- h4
        ; return $ mconcat a1 <> a2
        }

ls32 :: (Show s, TextualMonoid s) => URIParser s s
ls32 =  try ( do
                { a1 <- h4c
                ; a2 <- h4
                ; return (a1<>a2)
                } )
    <|> ipv4address

h4c :: TextualMonoid s => URIParser s s
h4c = try $
    do  { a1 <- h4
        ; void $ char ':'
        ; void $ notFollowedBy (char ':')
        ; return $ a1 <> ":"
        }

h4 :: TextualMonoid s => URIParser s s
h4 = fromString <$> countMinMax 1 4 hexDigitChar

ipv4address :: (Show s, TextualMonoid s) => URIParser s s
ipv4address =
    do  { a1 <- decOctet ; void $ char '.'
        ; a2 <- decOctet ; void $ char '.'
        ; a3 <- decOctet ; void $ char '.'
        ; a4 <- decOctet
        ; notFollowedBy nameChar
        ; return $ a1<>"."<>a2<>"."<>a3<>"."<>a4
        }
    <?> "IPv4 Address"

decOctet :: TextualMonoid s => URIParser s s
decOctet =
    do  { a1 <- countMinMax 1 3 digitChar
        ; if (read a1 :: Integer) > 255 then
            fail "Decimal octet value too large"
          else
            return $ fromString a1
        }

regName :: TextualMonoid s => URIParser s s
regName =
    do  { ss <- countMinMax 0 255 nameChar
        ; return $ mconcat ss
        }
    <?> "Registered name"

nameChar :: TextualMonoid s => URIParser s s
nameChar = singleton <$> unreservedChar <|> escaped <|> singleton <$> subDelims

--  RFC3986, section 3.2.3

port :: TextualMonoid s => URIParser s s
port =
    do  { void $ char ':'
        ; p <- many digitChar
        ; return $ fromString (':':p)
        }

--
--  RFC3986, section 3.3
--
--   path          = path-abempty    ; begins with "/" or is empty
--                 / path-abs        ; begins with "/" but not "//"
--                 / path-noscheme   ; begins with a non-colon segment
--                 / path-rootless   ; begins with a segment
--                 / path-empty      ; zero characters
--
--   path-abempty  = *( "/" segment )
--   path-abs      = "/" [ segment-nz *( "/" segment ) ]
--   path-noscheme = segment-nzc *( "/" segment )
--   path-rootless = segment-nz *( "/" segment )
--   path-empty    = 0<pchar>
--
--   segment       = *pchar
--   segment-nz    = 1*pchar
--   segment-nzc   = 1*( unreserved / pct-encoded / sub-delims / "@" )
--
--   pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

{-
upath :: URIParser s s
upath = pathAbEmpty
    <|> pathAbs
    <|> pathNoScheme
    <|> pathRootLess
    <|> pathEmpty
-}

pathAbEmpty :: TextualMonoid s => URIParser s s
pathAbEmpty =
    do  { ss <- many slashSegment
        ; return $ mconcat ss
        }

pathAbs :: TextualMonoid s => URIParser s s
pathAbs =
    do  { void $ char '/'
        ; ss <- option "" pathRootLess
        ; return $ "/" <> ss
        }

pathNoScheme :: TextualMonoid s => URIParser s s
pathNoScheme =
    do  { s1 <- segmentNzc
        ; ss <- many slashSegment
        ; return $ mconcat (s1:ss)
        }

pathRootLess :: TextualMonoid s => URIParser s s
pathRootLess =
    do  { s1 <- segmentNz
        ; ss <- many slashSegment
        ; return $ mconcat (s1:ss)
        }

slashSegment :: TextualMonoid s => URIParser s s
slashSegment =
    do  { void $ char '/'
        ; s <- segment
        ; return ("/" <> s)
        }

segment :: TextualMonoid s => URIParser s s
segment =
    do  { ps <- many pchar
        ; return $ mconcat ps
        }

segmentNz :: TextualMonoid s => URIParser s s
segmentNz =
    do  { ps <- many1 pchar
        ; return $ mconcat ps
        }

segmentNzc :: TextualMonoid s => URIParser s s
segmentNzc =
    do  { ps <- many1 (uchar "@")
        ; return $ mconcat ps
        }

pchar :: TextualMonoid s => URIParser s s
pchar = uchar ":@"

-- helper function for pchar and friends
uchar :: TextualMonoid s => String -> URIParser s s
uchar extras =
        singleton <$> unreservedChar
    <|> escaped
    <|> singleton <$> subDelims
    <|> do { c <- satisfyChar (`elem` extras) ; return (singleton c) }

--  RFC3986, section 3.4

uquery :: TextualMonoid s => URIParser s s
uquery =
    do  { ss <- many $ uchar (":@"<>"/?")
        ; return $ "?"<>mconcat ss
        }

--  RFC3986, section 3.5

ufragment :: TextualMonoid s => URIParser s s
ufragment =
    do  { ss <- many $ uchar (":@"<>"/?")
        ; return $ "#"<>mconcat ss
        }

--  Reference, Relative and Absolute URI forms
--
--  RFC3986, section 4.1

uriReference :: (TextualMonoid s, Show s) => URIParser s (GenURI s)
uriReference = uri <|> relativeRef

--  RFC3986, section 4.2
--
--   relative-URI  = relative-part [ "?" query ] [ "#" fragment ]
--
--   relative-part = "//" authority path-abempty
--                 / path-abs
--                 / path-noscheme
--                 / path-empty

relativeRef :: (TextualMonoid s, Show s) => URIParser s (GenURI s)
relativeRef =
    do  { notMatching uscheme
        -- ; ua <- option Nothing ( do { try (string "//") ; uauthority } )
        -- ; up <- upath
        ; (ua,up) <- relativePart
        ; uq <- option "" ( do { void $ char '?' ; uquery    } )
        ; uf <- option "" ( do { void $ char '#' ; ufragment } )
        ; return $ URI
            { uriScheme    = ""
            , uriAuthority = ua
            , uriPath      = up
            , uriQuery     = uq
            , uriFragment  = uf
            }
        }

relativePart :: (Show s, TextualMonoid s) => URIParser s (Maybe (GenURIAuth s),s)
relativePart =
        do  { void $ try (string "//")
            ; ua <- uauthority
            ; up <- pathAbEmpty
            ; return (ua,up)
            }
    <|> do  { up <- pathAbs
            ; return (Nothing,up)
            }
    <|> do  { up <- pathNoScheme
            ; return (Nothing,up)
            }
    <|> do  { return (Nothing,"")
            }

--  RFC3986, section 4.3

absoluteURI :: (Show s, TextualMonoid s) => URIParser s (GenURI s)
absoluteURI =
    do  { us <- uscheme
        -- ; ua <- option Nothing ( do { try (string "//") ; uauthority } )
        -- ; up <- upath
        ; (ua,up) <- hierPart
        ; uq <- option "" ( do { void $ char '?' ; uquery    } )
        ; return $ URI
            { uriScheme    = us
            , uriAuthority = ua
            , uriPath      = up
            , uriQuery     = uq
            , uriFragment  = ""
            }
        }

--  Imports from RFC 2234

    -- NOTE: can't use isAlphaNum etc. because these deal with ISO 8859
    -- (and possibly Unicode!) chars.
    -- [[[Above was a comment originally in GHC Network/URI.hs:
    --    when IRIs are introduced then most codepoints above 128(?) should
    --    be treated as unreserved, and higher codepoints for letters should
    --    certainly be allowed.
    -- ]]]

isAlphaChar :: Char -> Bool
isAlphaChar c    = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

isDigitChar :: Char -> Bool
isDigitChar c    = (c >= '0' && c <= '9')

isAlphaNumChar :: Char -> Bool
isAlphaNumChar c = isAlphaChar c || isDigitChar c

isHexDigitChar :: Char -> Bool
isHexDigitChar c = isHexDigit c

isSchemeChar :: Char -> Bool
isSchemeChar c   = (isAlphaNumChar c) || (c `List.elem` "+-.")

alphaChar :: TextualMonoid s => URIParser s Char
alphaChar = satisfyChar isAlphaChar         -- or: Parsec.letter ?

digitChar :: TextualMonoid s => URIParser s Char
digitChar = satisfyChar isDigitChar         -- or: Parsec.digit ?

hexDigitChar :: TextualMonoid s => URIParser s Char
hexDigitChar = satisfyChar isHexDigitChar   -- or: Parsec.hexDigit ?

--  Additional parser combinators for common patterns

oneThenMany :: Monoid s =>Parser s a -> Parser s a -> Parser s [a]
oneThenMany p1 pr =
    do  { a1 <- p1
        ; ar <- many pr
        ; return (a1:ar)
        }

countMinMax :: Monoid s => Int -> Int -> Parser s a -> Parser s [a]
countMinMax m n p | m > 0 =
    do  { a1 <- p
        ; ar <- countMinMax (m-1) (n-1) p
        ; return (a1:ar)
        }
countMinMax _ n _ | n <= 0 = return []
countMinMax _ n p = option [] $
    do  { a1 <- p
        ; ar <- countMinMax 0 (n-1) p
        ; return (a1:ar)
        }

notMatching :: (Monoid s, Show a) => Parser s a -> Parser s ()
notMatching p = do { a <- try p ; fail (show a) } <|> return ()

------------------------------------------------------------
--  Reconstruct a URI string
------------------------------------------------------------
--
-- |Turn a 'URI' into a string.
--
--  Uses a supplied function to map the userinfo part of the URI.
--
--  The Show instance for URI uses a mapping that hides any password
--  that may be present in the URI.  Use this function with argument @id@
--  to preserve the password in the formatted output.
--
uriToString :: (IsString s, MonoidNull s) => (s->s) -> GenURI s -> s->s
uriToString userinfomap URI { uriScheme=myscheme
                            , uriAuthority=myauthority
                            , uriPath=mypath
                            , uriQuery=myquery
                            , uriFragment=myfragment
                            } =
    (myscheme<>) . (uriAuthToString userinfomap myauthority)
               . (mypath<>) . (myquery<>) . (myfragment<>)

uriAuthToString :: (IsString s, MonoidNull s) => (s->s) -> (Maybe (GenURIAuth s)) -> s->s
uriAuthToString _           Nothing   = id          -- shows ""
uriAuthToString userinfomap
        (Just URIAuth { uriUserInfo = myuinfo
                      , uriRegName  = myregname
                      , uriPort     = myport
                      } ) =
    ("//"<>) . (if null myuinfo then id else ((userinfomap myuinfo)<>))
             . (myregname<>)
             . (myport<>)

------------------------------------------------------------
--  Character classes
------------------------------------------------------------

-- | Returns 'True' if the character is allowed in a URI.
--
isAllowedInURI :: Char -> Bool
isAllowedInURI c = isReserved c || isUnreserved c || c == '%' -- escape char

-- | Returns 'True' if the character is allowed unescaped in a URI.
--
isUnescapedInURI :: Char -> Bool
isUnescapedInURI c = isReserved c || isUnreserved c

-- | Returns 'True' if the character is allowed unescaped in a URI component.
--
isUnescapedInURIComponent :: Char -> Bool
isUnescapedInURIComponent c = not (isReserved c || not (isUnescapedInURI c))

------------------------------------------------------------
--  Escape sequence handling
------------------------------------------------------------

-- |Escape character if supplied predicate is not satisfied,
--  otherwise return character as singleton string.
--
escapeURIChar :: (Char->Bool) -> Char -> String
escapeURIChar p c
    | p c       = [c]
    | otherwise = List.concatMap (\i -> '%' : myShowHex i "") (utf8EncodeChar c)
    where
        myShowHex :: Int -> ShowS
        myShowHex n r =  case showIntAtBase 16 (toChrHex) n r of
            []  -> "00"
            [x] -> ['0',x]
            cs  -> cs
        toChrHex d
            | d < 10    = chr (ord '0' + fromIntegral d)
            | otherwise = chr (ord 'A' + fromIntegral (d - 10))

-- From http://hackage.haskell.org/package/utf8-string
-- by Eric Mertens, BSD3
-- Returns [Int] for use with showIntAtBase
utf8EncodeChar :: Char -> [Int]
utf8EncodeChar = List.map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

-- |Can be used to make a string valid for use in a URI.
--
escapeURIString
    :: TextualMonoid s =>
       (Char->Bool)     -- ^ a predicate which returns 'False'
                        --   if the character should be escaped
    -> s                -- ^ the string to process
    -> s                -- ^ the resulting URI string
escapeURIString p s = concatMap (fromString . escapeURIChar p) s

-- |Turns all instances of escaped characters in the string back
--  into literal characters.
--
unEscapeString :: TextualMonoid s => s -> s
unEscapeString uristr = prefix <> Foldable.foldMap (either fromUTF8 id) (List.foldr joinLefts [] $ List.concatMap unEscapeOne rest)
   where prefix:rest = split (== '%') uristr
         unEscapeOne chunk
            | Just (h1, cs1) <- splitCharacterPrefix chunk
            , Just (h2, cs2) <- splitCharacterPrefix cs1
            , isHexDigit h1 && isHexDigit h2 =
              Left (ByteString.singleton $ toEnum $ digitToInt h1*16+digitToInt h2) : if null cs2 then [] else [Right cs2]
            | otherwise = [Right $ singleton '%' <> chunk]
         joinLefts (Left bytes1) (Left bytes2 : chunks) = Left (bytes1 <> bytes2) : chunks
         joinLefts chunk chunks = chunk : chunks
         fromUTF8 = Factorial.foldMap (singleton . fromMaybe '\xfffd' . characterPrefix) . ByteStringUTF8

------------------------------------------------------------
-- Resolving a relative URI relative to a base URI
------------------------------------------------------------

-- |Returns a new 'URI' which represents the value of the
--  first 'URI' interpreted as relative to the second 'URI'.
--  For example:
--
--  > "foo" `relativeTo` "http://bar.org/" = "http://bar.org/foo"
--  > "http:foo" `nonStrictRelativeTo` "http://bar.org/" = "http://bar.org/foo"
--
--  Algorithm from RFC3986 [3], section 5.2.2
--

nonStrictRelativeTo :: (Eq s, TextualMonoid s) => GenURI s -> GenURI s -> GenURI s
nonStrictRelativeTo ref base = relativeTo ref' base
    where
        ref' = if uriScheme ref == uriScheme base
               then ref { uriScheme="" }
               else ref

isDefined :: MonoidNull s => s -> Bool
isDefined s = not (null s)

-- | Returns a new 'URI' which represents the value of the first 'URI'
-- interpreted as relative to the second 'URI'.
--
-- Algorithm from RFC3986 [3], section 5.2
relativeTo :: (Eq s, TextualMonoid s) => GenURI s -> GenURI s -> GenURI s
relativeTo ref base
    | isDefined ( uriScheme ref ) =
        just_segments ref
    | isJust ( uriAuthority ref ) =
        just_segments ref { uriScheme = uriScheme base }
    | isDefined ( uriPath ref ) =
        if (characterPrefix (uriPath ref) == Just '/') then
            just_segments ref
                { uriScheme    = uriScheme base
                , uriAuthority = uriAuthority base
                }
        else
            just_segments ref
                { uriScheme    = uriScheme base
                , uriAuthority = uriAuthority base
                , uriPath      = mergePaths base ref
                }
    | isDefined ( uriQuery ref ) =
        just_segments ref
            { uriScheme    = uriScheme base
            , uriAuthority = uriAuthority base
            , uriPath      = uriPath base
            }
    | otherwise =
        just_segments ref
            { uriScheme    = uriScheme base
            , uriAuthority = uriAuthority base
            , uriPath      = uriPath base
            , uriQuery     = uriQuery base
            }
    where
        just_segments u =
            u { uriPath = removeDotSegments (uriPath u) }
        mergePaths b r
            | isJust (uriAuthority b) && null pb = "/" <> pr
            | otherwise                          = dropLast pb <> pr
            where
                pb = uriPath b
                pr = uriPath r
        dropLast = fst . splitLast -- reverse . dropWhile (/='/') . reverse

--  Remove dot segments, but protect leading '/' character
removeDotSegments :: (Eq s, TextualMonoid s) => s -> s
removeDotSegments s 
   | Just ('/', ps) <- splitCharacterPrefix s = "/" <> elimDots ps mempty
   | otherwise = elimDots s mempty

--  Second arg accumulates segments processed so far in reverse order
elimDots :: (Eq s, TextualMonoid s) => s -> [s] -> s
-- elimDots ps rs | traceVal "\nps " ps $ traceVal "rs " rs $ False = error ""
elimDots ps rs 
   | null ps, null rs = mempty
   | null ps = mconcat (reverse rs)
   | Just ps' <- stripPrefix "./" ps = elimDots ps' rs
   | ps == "." = elimDots mempty rs
   | Just ps' <- stripPrefix "../" ps = elimDots ps' (drop 1 rs)
   | ps == ".." = elimDots mempty (drop 1 rs)
   | otherwise = elimDots ps1 (r:rs)
   where
      (r,ps1) = nextSegment ps

--  Returns the next segment and the rest of the path from a path string.
--  Each segment ends with the next '/' or the end of string.
--
nextSegment :: TextualMonoid s => s -> (s, s)
nextSegment ps =
    case break_ False (=='/') ps of
        (r,ps1) | Just ('/', ps2) <- splitCharacterPrefix ps1 -> (r<>"/",ps2)
                | otherwise -> (r,mempty)

--  Split last (name) segment from path, returning (path,name)
splitLast :: TextualMonoid s => s -> (s, s)
splitLast p = (reverse revpath,reverse revname)
    where
        (revname,revpath) = break_ False (=='/') $ reverse p

------------------------------------------------------------
-- Finding a URI relative to a base URI
------------------------------------------------------------

-- |Returns a new 'URI' which represents the relative location of
--  the first 'URI' with respect to the second 'URI'.  Thus, the
--  values supplied are expected to be absolute URIs, and the result
--  returned may be a relative URI.
--
--  Example:
--
--  > "http://example.com/Root/sub1/name2#frag"
--  >   `relativeFrom` "http://example.com/Root/sub2/name2#frag"
--  >   == "../sub1/name2#frag"
--
--  There is no single correct implementation of this function,
--  but any acceptable implementation must satisfy the following:
--
--  > (uabs `relativeFrom` ubase) `relativeTo` ubase == uabs
--
--  For any valid absolute URI.
--  (cf. <http://lists.w3.org/Archives/Public/uri/2003Jan/0008.html>
--       <http://lists.w3.org/Archives/Public/uri/2003Jan/0005.html>)
--
relativeFrom :: (Eq s, TextualMonoid s) => GenURI s -> GenURI s -> GenURI s
relativeFrom uabs base
    | diff uriScheme    uabs base = uabs
    | diff uriAuthority uabs base = uabs { uriScheme = "" }
    | diff uriPath      uabs base = uabs
        { uriScheme    = ""
        , uriAuthority = Nothing
        , uriPath      = relPathFrom (removeBodyDotSegments $ uriPath uabs)
                                     (removeBodyDotSegments $ uriPath base)
        }
    | diff uriQuery     uabs base = uabs
        { uriScheme    = ""
        , uriAuthority = Nothing
        , uriPath      = ""
        }
    | otherwise = uabs          -- Always carry fragment from uabs
        { uriScheme    = ""
        , uriAuthority = Nothing
        , uriPath      = ""
        , uriQuery     = ""
        }
    where
        diff :: Eq b => (a -> b) -> a -> a -> Bool
        diff sel u1 u2 = sel u1 /= sel u2
        -- Remove dot segments except the final segment
        removeBodyDotSegments p = removeDotSegments p1 <> p2
            where
                (p1,p2) = splitLast p

relPathFrom :: (Eq s, TextualMonoid s) => s -> s -> s
relPathFrom pabs base 
   | null pabs = "/"
   | null base = pabs                
   | sa1 == sb1 =                       
      -- Construct a relative path segments
      -- if the paths share a leading segment
      -- other than a leading '/'
      if (sa1 == "/")
      then if (sa2 == sb2)
           then relPathFrom1 ra2 rb2
           else pabs
      else relPathFrom1 ra1 rb1
   | otherwise = pabs
    where
        (sa1,ra1) = nextSegment pabs
        (sb1,rb1) = nextSegment base
        (sa2,ra2) = nextSegment ra1
        (sb2,rb2) = nextSegment rb1

--  relPathFrom1 strips off trailing names from the supplied paths,
--  and calls difPathFrom to find the relative path from base to
--  target
relPathFrom1 :: (Eq s, TextualMonoid s) => s -> s -> s
relPathFrom1 pabs base = relName
    where
        (sa,na) = splitLast pabs
        (sb,nb) = splitLast base
        rp      = relSegsFrom sa sb
        relName = if null rp then
                      if (na == nb) then ""
                      else if protect na then "./" <> na
                      else na
                  else
                      rp<>na
        -- Precede name with some path if it is null or contains a ':'
        protect s = null s || ':' `elem` s

--  relSegsFrom discards any common leading segments from both paths,
--  then invokes difSegsFrom to calculate a relative path from the end
--  of the base path to the end of the target path.
--  The final name is handled separately, so this deals only with
--  "directory" segtments.
--
relSegsFrom :: (Eq s, TextualMonoid s) => s -> s -> s
{-
relSegsFrom sabs base
    | traceVal "\nrelSegsFrom\nsabs " sabs $ traceVal "base " base $
      False = error ""
-}
relSegsFrom sabs base
   | null sabs && null base = mempty -- paths are identical
   | sa1 == sb1 = relSegsFrom ra1 rb1
   | otherwise = difSegsFrom sabs base
   where
      (sa1,ra1) = nextSegment sabs
      (sb1,rb1) = nextSegment base

--  difSegsFrom calculates a path difference from base to target,
--  not including the final name at the end of the path
--  (i.e. results always ends with '/')
--
--  This function operates under the invariant that the supplied
--  value of sabs is the desired path relative to the beginning of
--  base.  Thus, when base is empty, the desired path has been found.
--
difSegsFrom :: TextualMonoid s => s -> s -> s
{-
difSegsFrom sabs base
    | traceVal "\ndifSegsFrom\nsabs " sabs $ traceVal "base " base $
      False = error ""
-}
difSegsFrom sabs base 
   | null base = sabs
   | otherwise = difSegsFrom ("../" <> sabs) (snd $ nextSegment base)

------------------------------------------------------------
--  Other normalization functions
------------------------------------------------------------

-- |Case normalization; cf. RFC3986 section 6.2.2.1
--  NOTE:  authority case normalization is not performed
--
normalizeCase :: TextualMonoid s => s -> s
normalizeCase uristr 
   | Just ':' <- characterPrefix rest = map toLower scheme <> ncEscapes rest
   | otherwise = ncEscapes uristr
   where (scheme, rest) = span_ False isSchemeChar uristr
         ncEscapes = snd . mapAccumL ncEscape 0
         ncEscape :: Int -> Char -> (Int, Char)
         ncEscape 0 '%' = (1, '%')
         ncEscape 1 c = (2, toUpper c)
         ncEscape 2 c = (0, toUpper c)
         ncEscape ~0 c = (0, c)

-- |Encoding normalization; cf. RFC3986 section 6.2.2.2
--
normalizeEscape :: TextualMonoid s => s -> s
normalizeEscape uristr = prefix <> Foldable.foldMap normalizeOne rest
   where prefix:rest = split (== '%') uristr
         normalizeOne chunk
            | Just (h1, cs1) <- splitCharacterPrefix chunk
            , Just (h2, cs2) <- splitCharacterPrefix cs1
            , isHexDigit h1 && isHexDigit h2 
            , escval <- chr (digitToInt h1*16+digitToInt h2)
            , isUnreserved escval = 
               singleton escval <> cs2
            | otherwise = singleton '%' <> chunk

-- |Path segment normalization; cf. RFC3986 section 6.2.2.3
--
normalizePathSegments :: (Eq s, Show s, TextualMonoid s) => s -> s
normalizePathSegments uristr = normstr juri
    where
        juri = parseURI uristr
        normstr Nothing  = uristr
        normstr (Just u) = uriToString defaultUserInfoMap (normuri u) mempty
        normuri u = u { uriPath = removeDotSegments (uriPath u) }

--------------------------------------------------------------------------------
--
--  Copyright (c) 2004, G. KLYNE.  All rights reserved.
--  Distributed as free software under the following license.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--  - Redistributions of source code must retain the above copyright notice,
--  this list of conditions and the following disclaimer.
--
--  - Redistributions in binary form must reproduce the above copyright
--  notice, this list of conditions and the following disclaimer in the
--  documentation and/or other materials provided with the distribution.
--
--  - Neither name of the copyright holders nor the names of its
--  contributors may be used to endorse or promote products derived from
--  this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND THE CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDERS OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
--  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
--  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
--  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
--  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
--  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
--  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--------------------------------------------------------------------------------
