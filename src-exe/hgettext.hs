module Main (main) where

import qualified Language.Haskell.Exts       as H

import           System.Console.GetOpt
import           System.Environment
import           System.Exit

import           Data.Generics.Uniplate.Data

import           Data.List

import           Data.Version                (showVersion)
import           Paths_hgettext              (version)

data Options = Options {
      outputFile   :: String,
      keyword      :: String,
      printVersion :: Bool
    } deriving Show

options :: [OptDescr (Options->Options)]
options =
    [
     Option ['o'] ["output"]
                (ReqArg (\o opts -> opts {outputFile = o}) "FILE")
                "write output to specified file",
     Option ['d'] ["default-domain"]
            (ReqArg (\d opts -> opts {outputFile = d ++ ".po"}) "NAME")
            "use NAME.po instead of messages.po",
     Option ['k'] ["keyword"]
            (ReqArg (\d opts -> opts {keyword = d}) "WORD")
            "function name, in which wrapped searched words",
     Option [] ["version"]
            (NoArg (\opts -> opts {printVersion = True}))
            "print version of hgettexts"
    ]

defaultOptions :: Options
defaultOptions = Options "messages.po" "__" False

parseArgs :: [String] -> IO (Options, [String])
parseArgs args =
    case getOpt Permute options args of
      (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
      (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: hgettext [OPTION] [INPUTFILE] ..."


toTranslate :: String -> H.Module H.SrcSpanInfo -> [(Int, String)]
toTranslate f z = nub [ (0, s) | H.App _
                                     (H.Var _
                                         (H.UnQual _
                                             (H.Ident  _ x)))
                                     (H.Lit _
                                         (H.String _ s _slit)) <- universeBi z :: [H.Exp H.SrcSpanInfo]
                                  , x == f]

-- Create list of messages from a single file
formatMessages :: String -> [(Int, String)] -> String
formatMessages src l0 = concat $ map potEntry l0
    where potEntry (l, s) = unlines [
                             "#: " ++ src ++ ":" ++ (show l),
                             "msgid " ++ (show s),
                             "msgstr \"\"",
                             ""
                            ]


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

process :: Options -> [String] -> IO ()
process Options{printVersion = True} _ =
    putStrLn $ "hgettext, version " ++ (showVersion version)
process opts fl = do
    t <- mapM readSource fl
    writeFile (outputFile opts) $ do
      writePOTFile [ formatMessages n $ toTranslate (keyword opts) c | (n,c) <- t ]
  where
    readSource "-" = do
      c <- getContents
      case H.parseFileContents c of
        H.ParseFailed loc msg -> do
          putStrLn (concat [ "<stdin>:", show (H.srcLine loc), ":", show (H.srcColumn loc), ": error: ", msg ])
          exitFailure
        H.ParseOk m -> return ("-", m)
    readSource f = do
      pm <- H.parseFile f
      case pm of
        H.ParseFailed loc msg -> do
          putStrLn (concat [ f, ":", show (H.srcLine loc), ":", show (H.srcColumn loc), ": error: ", msg ])
          exitFailure
        H.ParseOk m -> return (f, m)

main :: IO ()
main = getArgs >>= parseArgs >>= uncurry process
