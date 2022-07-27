module Main (main) where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Data.Generics.Uniplate.Data
import           Data.List.Split             (splitOn)
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Data.Version                (showVersion)
import qualified Language.Haskell.Exts       as H
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO                   (IOMode(WriteMode), hPutStr, hSetEncoding, utf8, withFile)

import           Paths_hgettext              (version)

data Options = Options
  { outputFile   :: FilePath
  , keyword      :: String
  , extensions   :: [H.Extension]
  , printVersion :: Bool
  } deriving Show

options :: [OptDescr (Options->Options)]
options =
  [ Option ['o'] ["output"]
           (ReqArg (\o opts -> opts {outputFile = o}) "FILE")
           "write output to specified file"
  , Option ['d'] ["default-domain"]
           (ReqArg (\d opts -> opts {outputFile = d ++ ".po"}) "NAME")
           "use NAME.po instead of messages.po"
  , Option ['k'] ["keyword"]
           (ReqArg (\d opts -> opts {keyword = d}) "WORD")
           "function name, in which wrapped searched words"
  , Option ['e'] ["lang-exts"]
           (ReqArg (\es opts -> opts {extensions = map (\e -> H.parseExtension e) (splitOn "," es)}) "EXTENSION...")
           "language extensions to enable/disable when parsing input (prefix \"No\" to an extension to disable it)"
  , Option [] ["version"]
           (NoArg (\opts -> opts {printVersion = True}))
           "print version of hgettexts"
  ]

defaultOptions :: Options
defaultOptions = Options "messages.po" "__" [] False

parseArgs :: [String] -> IO (Options, [String])
parseArgs args =
    case getOpt Permute options args of
      (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
      (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: hgettext [OPTION] [INPUTFILE] ..."


toTranslate :: String -> H.Module H.SrcSpanInfo -> [(Int, String)]
toTranslate f z = [ (H.srcSpanStartLine (H.srcInfoSpan loc), s)
                  | H.App _ (H.Var _
                              (H.UnQual _
                                (H.Ident  _ x)))
                            (H.Lit _
                              (H.String loc s _slit))
                    <- universeBi z :: [H.Exp H.SrcSpanInfo]
                  , x == f]

showStringLit :: String -> String
showStringLit s0 = '"' : concatMap showChar s0 ++ "\""
    where
      showChar '"' = "\\\""
      showChar '\\' = "\\\\"
      showChar '\n' = "\\n"
      showChar c = return c

formatMessage :: String -> [(FilePath, Int)] -> String
formatMessage s locs = unlines $
                       map (uncurry formatLoc) locs ++
                       [ "msgid " ++ (showStringLit s)
                       , "msgstr \"\""
                       , ""
                       ]
  where
    formatLoc src l = "#: " ++ src ++ ":" ++ (show l)


writePOTFile :: [String] -> String
writePOTFile l = concat $ [potHeader] ++ l
    where potHeader = unlines ["# Translation file",
                               "",
                               "msgid \"\"",
                               "msgstr \"\"",
                               "",
                               "\"Project-Id-Version: PACKAGE VERSION\\n\"",
                               "\"Report-Msgid-Bugs-To: \\n\"",
                               "\"POT-Creation-Date: 2009-01-13 06:05-0800\\n\"",
                               "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"",
                               "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"",
                               "\"Language-Team: LANGUAGE <LL@li.org>\\n\"",
                               "\"MIME-Version: 1.0\\n\"",
                               "\"Content-Type: text/plain; charset=UTF-8\\n\"",
                               "\"Content-Transfer-Encoding: 8bit\\n\"",
                               ""]

writeFileUtf8 :: FilePath -> String -> IO ()
writeFileUtf8 fp content =
  withFile fp WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h content

process :: Options -> [FilePath] -> IO ()
process Options{printVersion = True} _ =
    putStrLn $ "hgettext, version " ++ (showVersion version)
process opts fl = do
    dat <- forM fl $ \fn -> do
      m <- readSource fn
      evaluate $ force [ (s,(fn,loc)) | (loc,s) <- toTranslate (keyword opts) m ]

    let entries = Map.fromListWith Set.union [ (s,Set.singleton (fn,loc)) | d <- dat, (s,(fn,loc)) <- d ]

    writeFileUtf8 (outputFile opts) $ do
      writePOTFile [ formatMessage s (Set.toList locs) | (s,locs) <- Map.toList entries ]
  where
    readSource "-" = do
      c <- getContents
      case H.parseFileContentsWithExts (extensions opts) c of
        H.ParseFailed loc msg -> do
          putStrLn (concat [ "<stdin>:", show (H.srcLine loc), ":", show (H.srcColumn loc), ": error: ", msg ])
          exitFailure
        H.ParseOk m -> return m
    readSource f = do
      pm <- H.parseFileWithExts (extensions opts) f
      case pm of
        H.ParseFailed loc msg -> do
          putStrLn (concat [ f, ":", show (H.srcLine loc), ":", show (H.srcColumn loc), ": error: ", msg ])
          exitFailure
        H.ParseOk m -> return m

main :: IO ()
main = getArgs >>= parseArgs >>= uncurry process
