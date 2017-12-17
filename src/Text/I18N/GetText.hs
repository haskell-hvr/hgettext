-- | This library provides basic internationalization capabilities

module Text.I18N.GetText (
                          getText,
                          nGetText,
                          dGetText,
                          dnGetText,
                          dcGetText,
                          dcnGetText,
                          bindTextDomain,
                          textDomain
                         ) where

import           Data.Maybe              (fromMaybe)
import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
import           System.Locale.SetLocale


foreign import ccall unsafe "libintl.h gettext" c_gettext
    :: CString -> IO CString

foreign import ccall unsafe "libintl.h dgettext" c_dgettext
    :: CString -> CString -> IO CString

foreign import ccall unsafe "libintl.h dcgettext" c_dcgettext
    :: CString -> CString -> CInt -> IO CString

foreign import ccall unsafe "libintl.h ngettext" c_ngettext
    :: CString -> CString -> CULong -> IO CString

foreign import ccall unsafe "libintl.h dngettext" c_dngettext
    :: CString -> CString -> CString -> CULong -> IO CString

foreign import ccall unsafe "libintl.h dcngettext" c_dcngettext
    :: CString -> CString -> CString -> CULong -> CInt -> IO CString

foreign import ccall unsafe "libintl.h bindtextdomain" c_bindtextdomain
    :: CString -> CString -> IO CString

foreign import ccall unsafe "libintl.h textdomain" c_textdomain
    :: CString -> IO CString

fromCString :: CString -> IO (Maybe String)
fromCString x | x == nullPtr = return Nothing
              | otherwise = peekCString x >>= return . Just

fromCStringError :: String -> CString -> IO String
fromCStringError err x | x == nullPtr = throwErrno err
                       | otherwise = peekCString x

fromCStringDefault :: String -> CString -> IO String
fromCStringDefault d x = fromCString x >>= \r -> return (fromMaybe d r)

fromCStringPluralDefault :: (Eq a, Num a) => String -> String -> a -> CString -> IO String
fromCStringPluralDefault def def_plural n s
    | n == 1 = fromCStringDefault def s
    | otherwise = fromCStringDefault def_plural s


withCStringMaybe :: Maybe String -> (CString -> IO a) -> IO a
withCStringMaybe Nothing f    = f nullPtr
withCStringMaybe (Just str) f = withCString str f

-- |getText wraps GNU gettext function. It returns translated string for the
-- input messages. If translated string not found the input string will be
-- returned.
--
-- The most common usage of this function is to declare function __:
--
-- > __ = unsafePerformIO . getText
--
-- and wrap all text strings into this function, e.g.
--
-- > printHello = putStrLn (__ "Hello")
--
getText :: String -> IO String
getText s =
    withCString s $ \s' ->
        c_gettext s' >>= fromCStringDefault s

-- |dGetText wraps GNU dgettext function. It works similar to 'getText'
-- but also could take domain name.
--
dGetText :: Maybe String        -- ^ domain name, if 'Nothing' ---
                                -- default domain will be used
         -> String              -- ^ message id
         -> IO String           -- ^ return value
dGetText domainname msgid =
    withCStringMaybe domainname $ \dn' ->
        withCString msgid $ \msg' ->
            c_dgettext dn' msg' >>= fromCStringDefault msgid

-- |dcGetText wraps GNU dcgettext function. It works similar to 'dGetText'
-- but also takes category id
dcGetText :: Maybe String       -- ^ domain name, if 'Nothing' ---
                                -- default domain will be used
          -> Category           -- ^ locale facet
          -> String             -- ^ message id
          -> IO String          -- ^ return value
dcGetText domainname cat msgid =
    withCStringMaybe domainname $ \dn' ->
        withCString msgid $ \msg' ->
            c_dcgettext dn' msg' (categoryToCInt cat) >>=
            fromCStringDefault msgid

-- |nGetText wraps GNU ngettext function. It translates text string in the
-- user's native language, by lookilng up the approppiate plural form of the
-- message.
--
nGetText :: String              -- ^ msgid in singular form
         -> String              -- ^ msgid in plural form
         -> Integer             -- ^ number, used to choose appropriate form
         -> IO String           -- ^ result string, by default if number is 1 than
                                -- singular form of msgid is returned, otherwise ---
                                -- plural
nGetText msgid msgid_plural n =
    withCString msgid $ \msgid' ->
        withCString msgid_plural $ \msgid_plural' ->
            c_ngettext msgid' msgid_plural' (fromInteger n) >>=
            fromCStringPluralDefault msgid msgid_plural n

-- |dnGetText wraps GNU dngettext function. It works similar to 'nGetText' but
-- also takes domain name
--
dnGetText :: Maybe String       -- ^ domain name, if 'Nothing' ---
                                -- default domain will be used
          -> String             -- ^ msgid in singular form
          -> String             -- ^ msgid in plural form
          -> Integer            -- ^ number, used to choose appropriate form
          -> IO String          -- ^ result string, by default if number is 1 than
                                -- singular form of msgid is returned, otherwise ---
                                -- plural
dnGetText domainname msgid msgid_plural n =
    withCStringMaybe domainname $ \dn' ->
        withCString msgid $ \msgid' ->
            withCString msgid_plural $ \msgid_plural' ->
                c_dngettext dn' msgid' msgid_plural' (fromInteger n) >>=
                fromCStringPluralDefault msgid msgid_plural n

-- |dcnGetText wraps GNU dcngettext function. It works similar to 'dnGetText' but
-- also takes category id
--
dcnGetText :: Maybe String      -- ^ domain name, if 'Nothing' ---
                                -- default domain will be used
          -> Category           -- ^ locale facet
          -> String             -- ^ msgid in singular form
          -> String             -- ^ msgid in plural form
          -> Integer            -- ^ number, used to choose appropriate form
          -> IO String          -- ^ result string, by default if number is 1 than
                                -- singular form of msgid is returned, otherwise ---
                                -- plural
dcnGetText domainname cat msgid msgid_plural n =
    withCStringMaybe domainname $ \dn' ->
        withCString msgid $ \msgid' ->
            withCString msgid_plural $ \msgid_plural' ->
                c_dcngettext dn' msgid' msgid_plural'
                             (fromInteger n) (categoryToCInt cat) >>=
                fromCStringPluralDefault msgid msgid_plural n

-- |bindTextDomain sets the base directory of the hierarchy
-- containing message catalogs for a given message domain.
--
-- Throws 'IOError' if fails
--
bindTextDomain :: String        -- ^ domain name
               -> Maybe String  -- ^ path to the locale folder or 'Nothing' to return
                                -- base directory for domain
               -> IO String     -- ^ return value
bindTextDomain domainname dirname =
  withCString domainname $ \domain ->
      withCStringMaybe dirname $ \dir ->
          c_bindtextdomain domain dir >>= fromCStringError "bindTextDomain fails"

-- |textDomain sets domain for future 'getText' call
--
-- Throws 'IOError' if fails
--
textDomain :: Maybe String      -- ^ domain name, if 'Nothing' than returns
                                -- current domain name
           -> IO String         -- ^ return value
textDomain domainname =
    withCStringMaybe domainname $ \domain ->
        c_textdomain domain >>= fromCStringError "textDomain fails"
