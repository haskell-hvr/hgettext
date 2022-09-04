-- | This library extends the Distribution with internationalization support.
--
-- It performs two functions:
--
-- * compiles and installs PO files to the specified directory
--
-- * tells the application where files were installed to make it able
-- to bind them to the code
--
-- Each PO file will be placed to the
-- @{datadir}\/locale\/{loc}\/LC_MESSAGES\/{domain}.mo@ where:
--
--  [@datadir@] Usually @prefix/share@ but could be different, depends
--  on system.
--
--  [@loc@] Locale name (language code, two characters). This module
--  supposes, that each PO file has a base name set to the proper
--  locale, e.g. @de.po@ is the German translation of the program, so
--  this file will be placed under @{datadir}\/locale\/de@ directory
--
--  [@domain@] Program domain. A unique identifier of single
--  translational unit (program). By default domain will be set to the
--  package name, but its name could be configured in the @.cabal@ file.
--
-- The module defines following @.cabal@ fields:
--
--  [@x-gettext-domain-name@] Name of the domain. One or more
--  alphanumeric characters separated by hyphens or underlines. When
--  not set, package name will be used.
--
--  [@x-gettext-po-files@] List of files with translations. Could be
--  used a limited form of wildcards, e.g.:
--  @x-gettext-po-files: po/*.po@
--
--  [@x-gettext-domain-def@] Name of the macro, in which domain name
--  will be passed to the program. Default value is
--  @__MESSAGE_CATALOG_DOMAIN__@
--
--  [@x-gettext-msg-cat-def@] Name of the macro, in which path to the
--  message catalog will be passed to the program. Default value is
--  @__MESSAGE_CATALOG_DIR__@
--
-- The last two parameters are used to send configuration data to the
-- code during its compilation. The most common usage example is:
--
--
-- > ...
-- > prepareI18N = do
-- >    setLocale LC_ALL (Just "")
-- >    bindTextDomain __MESSAGE_CATALOG_DOMAIN__ (Just __MESSAGE_CATALOG_DIR__)
-- >    textDomain __MESSAGE_CATALOG_DOMAIN__
-- >
-- > main = do
-- >    prepareI18N
-- >    ...
-- >
-- > ...
--
--
-- __NOTE:__ files, passed in the @x-gettext-po-files@ are not
-- automatically added to the source distribution, so they should be
-- also added to the @extra-source-files@ parameter, along with
-- translation template file (usually @message.pot@)
--
-- __WARNING:__ sometimes, when only configuration targets changes, code
-- will not recompile, thus you should execute @cabal clean@ to
-- cleanup the build and restart it again from the configuration. This
-- is temporary bug, it will be fixed in next releases.
--

module Distribution.Simple.I18N.GetText
    ( installGetTextHooks
    , gettextDefaultMain
    ) where

import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.InstallDirs    as I
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           Distribution.Verbosity

import           Control.Arrow                      (second)
import           Control.Monad
import           Data.List                          (nub, unfoldr)
import           Data.Maybe                         (fromMaybe, listToMaybe)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process

import           Internal                           (fromPackageName, matchFileGlob)

-- | Default main function, same as
--
-- > defaultMainWithHooks $ installGetTextHooks simpleUserHooks
--
gettextDefaultMain :: IO ()
gettextDefaultMain = defaultMainWithHooks $ installGetTextHooks simpleUserHooks

-- | Installs hooks, used by GetText module to install
-- PO files to the system.
--
-- Pre-existing hook handlers are executed before the GetText
-- handlers.
--
installGetTextHooks :: UserHooks -- ^ initial user hooks
                    -> UserHooks -- ^ patched user hooks
installGetTextHooks uh =
    uh { confHook = \a b -> do
           lbi <- (confHook uh) a b
           return (updateLocalBuildInfo lbi)

       , postInst = \args iflags pd lbi -> do
           postInst uh args iflags pd lbi
           installPOFiles (fromFlagOrDefault maxBound (installVerbosity iflags)) lbi

       , postCopy = \args cflags pd lbi -> do
           postCopy uh args cflags pd lbi
           installPOFiles (fromFlagOrDefault maxBound (copyVerbosity cflags)) lbi
       }


updateLocalBuildInfo :: LocalBuildInfo -> LocalBuildInfo
updateLocalBuildInfo l =
    let sMap = getCustomFields l
        domDef = getDomainDefine sMap
        catDef = getMsgCatalogDefine sMap
        dom = getDomainNameDefault sMap (getPackageName l)
        tar = targetDataDir l
        catMS = formatMacro domDef dom
        domMS = formatMacro catDef tar
    in appendCPPOptions [domMS,catMS] $ appendExtension [EnableExtension CPP] l

installPOFiles :: Verbosity -> LocalBuildInfo -> IO ()
installPOFiles verb l =
    let sMap = getCustomFields l
        destDir = targetDataDir l
        dom = getDomainNameDefault sMap (getPackageName l)
        installFile file = do
          let fname = takeFileName file
          let bname = takeBaseName fname
          let targetDir = destDir </> bname </> "LC_MESSAGES"
          -- ensure we have directory destDir/{loc}/LC_MESSAGES
          createDirectoryIfMissing True targetDir
          ph <- runProcess "msgfmt" [ "--output-file=" ++ (targetDir </> dom <.> "mo"), file ]
                           Nothing Nothing Nothing Nothing Nothing
          ec <- waitForProcess ph
          case ec of
            ExitSuccess   -> return ()
            -- only warn for now, as the package may still be usable even if the msg catalogs are missing
            ExitFailure n -> warn verb ("'msgfmt' exited with non-zero status (rc = " ++ show n ++ ")")
    in do
      filelist <- getPoFilesDefault verb l sMap
      -- copy all whose name is in the form of dir/{loc}.po to the
      -- destDir/{loc}/LC_MESSAGES/dom.mo
      -- with the 'msgfmt' tool
      mapM_ installFile filelist

forBuildInfo :: LocalBuildInfo -> (BuildInfo -> BuildInfo) -> LocalBuildInfo
forBuildInfo l f =
    let a = l{localPkgDescr = updPkgDescr (localPkgDescr l)}
        updPkgDescr x = x{library = updLibrary (library x),
                          executables = updExecs (executables x)}
        updLibrary Nothing  = Nothing
        updLibrary (Just x) = Just $ x{libBuildInfo = f (libBuildInfo x)}
        updExecs x = map updExec x
        updExec x = x{buildInfo = f (buildInfo x)}
    in a

appendExtension :: [Extension] -> LocalBuildInfo -> LocalBuildInfo
appendExtension exts l =
    forBuildInfo l updBuildInfo
    where updBuildInfo x = x{defaultExtensions = updExts (defaultExtensions x)}
          updExts s = nub (s ++ exts)

appendCPPOptions :: [String] -> LocalBuildInfo -> LocalBuildInfo
appendCPPOptions opts l =
    forBuildInfo l updBuildInfo
    where updBuildInfo x = x{cppOptions = updOpts (cppOptions x)}
          updOpts s = nub (s ++ opts)

formatMacro :: Show a => String -> a -> String
formatMacro name value = "-D" ++ name ++ "=" ++ show value

targetDataDir :: LocalBuildInfo -> FilePath
targetDataDir l =
    let dirTmpls = installDirTemplates l
        prefix' = prefix dirTmpls
        data' = datadir dirTmpls
        dataEx = I.fromPathTemplate $ I.substPathTemplate [(PrefixVar, prefix')] data'
    in dataEx ++ "/locale"

getPackageName :: LocalBuildInfo -> String
getPackageName = fromPackageName . packageName . localPkgDescr

getCustomFields :: LocalBuildInfo -> [(String, String)]
getCustomFields = customFieldsPD . localPkgDescr

findInParametersDefault :: [(String, String)] -> String -> String -> String
findInParametersDefault al name def = (fromMaybe def . lookup name) al

getDomainNameDefault :: [(String, String)] -> String -> String
getDomainNameDefault al d = findInParametersDefault al "x-gettext-domain-name" d

getDomainDefine :: [(String, String)] -> String
getDomainDefine al = findInParametersDefault al "x-gettext-domain-def" "__MESSAGE_CATALOG_DOMAIN__"

getMsgCatalogDefine :: [(String, String)] -> String
getMsgCatalogDefine al = findInParametersDefault al "x-gettext-msg-cat-def" "__MESSAGE_CATALOG_DIR__"

getPoFilesDefault :: Verbosity -> LocalBuildInfo  -> [(String, String)] -> IO [String]
getPoFilesDefault verb l al = toFileList $ findInParametersDefault al "x-gettext-po-files" ""
    where toFileList "" = return []
          toFileList x  = liftM concat $ mapM (matchFileGlob verb (localPkgDescr l)) $ split' x
          -- from Blow your mind (HaskellWiki)
          -- splits string by newline, space and comma
          split' x = concatMap lines $ concatMap words $ unfoldr (\b -> fmap (const . (second $ drop 1) . break (==',') $ b) . listToMaybe $ b) x
