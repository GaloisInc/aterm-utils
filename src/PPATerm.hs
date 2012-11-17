module Main where

import System.Console.GetOpt
import System.Environment ( getArgs )

import Control.Applicative
import Control.Monad ( when )
import Data.List ( foldl', isSuffixOf )

-- Project specific imports
import ATerm.Pretty ( ppATerm )
import qualified ATerm.Utilities as U

---------------------------------------------------------------------
-- Command Line options
---------------------------------------------------------------------
data Options = Options
  { optSource :: Maybe FilePath -- ^ Nothing means read from stdin
                                --   otherwise read this file
  }
  deriving (Read, Show, Eq)

options :: [ OptDescr (Options -> Options) ]
options =
  [ Option "s" ["source"]
      (ReqArg (\arg opt -> opt { optSource = Just arg }) "FILE")
      "ATerm file to pretty print"
  ]

defaultOptions :: Options
defaultOptions = Options { optSource = Nothing }

header :: String
header = unlines
  [ "usage: [-s FILE]"
  , "  If no file is given, input is taken from stdin"
  ]

---------------------------------------------------------------------
-- Main program
---------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  let (flags, _, errors) = getOpt RequireOrder options args
      opts = foldl' (.) id flags defaultOptions
  when (not (null errors)) $ error (concat errors ++ usageInfo header options)
  cs <- removeTrailingDot <$> maybe getContents readFile (optSource opts)
  let aterms = U.readATerm cs
  print (ppATerm aterms)
  where
  removeTrailingDot s
    | length s >= 2 && ".\n" `isSuffixOf` s = init (init s)
    | otherwise = s
