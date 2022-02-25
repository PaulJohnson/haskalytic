{- |
Module              : HieDb.HieGraph
Copyright           : Ⓒ 2022 Paul Johnson
License             : BSD3
Maintainer          : Paul Johnson

-}

module HieDb.HieGraph (
   Component (..),
   componentModule,
   displayComponent,
   graphComponents,
   componentClients,
   displayVertices,
   displayVertex
) where

import Algebra.Graph.AdjacencyMap
import qualified Algebra.Graph.Acyclic.AdjacencyMap as AC
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NE
import Control.Lens
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Set(Set)
import qualified Data.Set as S
import HieDb
import HieDb.Compat


-- | The graph is divided into strongly connected components (where every vertex depends on every
-- other vertex) and simple components (vertices not involved in a strongly connected component.)
data Component =
   Connected ModuleName [String]
   | Simple Vertex
   deriving (Eq, Ord)


-- | The module where a component is declared.
componentModule :: Component -> ModuleName
componentModule (Connected n _) = n
componentModule (Simple v) = mkModuleName $ v ^. _1


-- | Display a component as a string.
displayComponent :: Component -> String
displayComponent (Connected mn vs) =
      "Component in " <> moduleNameString mn <> ": " <> intercalate ", " (map showName vs)
   where
      showName v = let (t,n) = displayName v in t <> " " <> n
displayComponent (Simple v) =
   "Vertex " <> displayVertex v


-- | Convert a vertex graph into an acyclic component graph.
graphComponents :: AdjacencyMap Vertex -> AdjacencyMap Component
graphComponents = gmap (toComponent . NE.vertexList1) . AC.fromAcyclic . AC.scc
   where
      toComponent (v :| []) = Simple v
      toComponent (v :| vs) =
         let
            modName = v ^. _1
         in
            if all ((modName ==) . view _1) vs
               then Connected (mkModuleName modName) $ map (view _3) $ v:vs
               else
                  error $ "Strongly connected component spread over multiple modules: " <>
                     displayVertices 3 (v:vs)


-- | The set of modules in which this component is used, minus its own.
componentClients :: Component -> AdjacencyMap Component -> Set ModuleName
componentClients c graph = users
   where
      owner = componentModule c
      users = S.delete owner $ S.map componentModule $ preSet c graph


-- | A measure of the distance between two sets. Equal to the number of items that appear in one
-- set but not the other.
setDistance :: (Ord a) => Set a -> Set a -> Int
setDistance a b = S.size (a `S.difference` b) + S.size (b `S.difference` a)


-- | Pretty-print a list of graph vertices, one per line.
displayVertices :: Int -> [Vertex] -> String
displayVertices indent = concatMap showLine
   where
      showLine v = replicate indent ' ' <> displayVertex v <> "\n"


-- | A vertex tuple in "HieDb" contains @(module, hie-file, occurance-name, start-line, start-char,
-- end-line, end-char)@. The name is prefixed with @v@ for a value, @t@ for a type, @c@ for data,
-- and @z@ for a type variable.
displayVertex :: Vertex -> String
displayVertex (moduleName, _, "", _, _, _, _) = "<Error: empty name>"
displayVertex (moduleName, _, c:name, _, _, _, _) =
      namespace c <> " " <> moduleName <> "." <> name


-- | Unpack the single character namespace prefix to a full namespace idenity.
namespace :: Char -> String
namespace c = case c of
      'v' -> "Value"
      't' -> "Type"
      'c' -> "Data"
      'z' -> "tVar"
      _ -> "?"


-- | A name consists of a single character prefix for the name space, followed by the name itself.
-- This function returns the namespace and the name.
displayName :: String -> (String, String)
displayName "" = ("?", "")
displayName (c:ns) = (namespace c, ns)
