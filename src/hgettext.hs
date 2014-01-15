module Main where

import qualified Language.Haskell.Exts.Annotated as H

import System.Environment
import System.Console.GetOpt

import Data.Generics.Uniplate.Data

import Distribution.Simple.PreProcess.Unlit

import Data.List
import Data.Char
import Data.Function (on)
import System.FilePath

import Paths_hgettext (version)
import Data.Version (showVersion)

data Options = Options {
      outputFile :: String,
      keyword :: String,
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


defaultOptions = Options "messages.po" "__" False

parseArgs :: [String] -> IO (Options, [String])
parseArgs args = 
    case getOpt Permute options args of
      (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
      (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: hgettext [OPTION] [INPUTFILE] ..."


toTranslate :: String
            -> H.ParseResult (H.Module H.SrcSpanInfo)
            -> [(Int, String)]
toTranslate f (H.ParseOk z) =
    nub [ (H.srcSpanStartLine (H.srcInfoSpan l), s)
        | H.App _ (H.Var _ (H.UnQual _ (H.Ident _ x)))
                  (H.Lit _ (H.String l s _)) <- universeBi z
        , x == f
        ]
toTranslate _ _ = []

formatMessage :: String
              -> [(String, Int)]
              -> String
formatMessage s locs = unlines $
                            map (uncurry formatLoc) locs ++
                            [
                             "msgid " ++ (show s),
                             "msgstr \"\"",
                             ""
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

parseFile :: String
          -> IO (H.ParseResult (H.Module H.SrcSpanInfo))
parseFile "-" = fmap H.parseFileContents getContents
parseFile f   = H.parseFile f

process :: Options -> [String] -> IO ()
process Options{printVersion = True} _ = 
    putStrLn $ "hgettext, version " ++ (showVersion version)

process opts fl = do
  parseResults <- mapM parseFile fl

  let messages = [ (str, (filename, lineNumber))
                 | (filename, ast) <- zip fl parseResults
                 , (lineNumber, str) <- toTranslate (keyword opts) ast
                 ]

      formatted = [ formatMessage s (map snd xs)
                  | xs@((s, _):_) <- groupBy ((==) `on` fst) $ sort messages
                  ]
  writeFile (outputFile opts) $ writePOTFile formatted

main = 
    getArgs >>= parseArgs >>= uncurry process

