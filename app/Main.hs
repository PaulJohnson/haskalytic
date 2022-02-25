module Main where

import Algebra.Graph.AdjacencyMap
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Set as S
import HieDb
import HieDb.Compat
import HieDb.HieGraph
import HieDb.Run
import Options.Applicative
import System.Console.ANSI
import System.Directory
import System.Environment
import System.IO

main :: IO ()
main = do
   defaultLoc <- getXdgDirectory XdgData $ "default_"++ show dB_VERSION ++".hiedb"
   defdb <- fromMaybe defaultLoc <$> lookupEnv "HIEDB"
   colr <- hSupportsANSIColor stdout
   hSetBuffering stdout NoBuffering
   hSetBuffering stderr NoBuffering
   (opts, moduleArg) <- execParser $ haskalyticParseInfo defdb colr
   withHieDb (database opts) $ \hiedb -> do
      graph <- graphComponents <$> getGraph hiedb
      let
         comps = sortOn componentModule $ vertexList graph
         comps2 = case moduleArg of
            Nothing -> comps
            Just moduleName -> filter ((moduleName ==) . componentModule) comps
      forM_ comps2 $ \c -> do
         let users = map moduleNameString $ S.toList $ componentClients c graph
         unless (null users) $ do
            putStrLn $ displayComponent c
            putStrLn $ "   used in " <> intercalate ", " users


-- | Command line info and parsing for standard @hiedb@ options.
haskalyticParseInfo :: FilePath -> Bool -> ParserInfo (Options, Maybe ModuleName)
haskalyticParseInfo db colr = info (haskalyticCommandParser db colr) fullDesc

haskalyticCommandParser :: FilePath -> Bool -> Parser (Options, Maybe ModuleName)
haskalyticCommandParser db colr = (,) <$> optParser db colr <*> optional moduleNameParser
