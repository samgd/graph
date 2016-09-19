module Graph.Parse
  ( parse
  ) where

import           Control.Monad     (guard)
import           Data.Graph
import           Text.Parsec       (anyToken, notFollowedBy)
import qualified Text.Parsec       as TP
import           Text.Parsec.Char  (char)
import           Text.Parsec.Utils (nat)
import           Text.Read         (readMaybe)

-- | 'parse' parses the given 'String' into a 'Graph' according to the file
-- format given for task 1 at http://www.ics.uci.edu/~wayne/research/students/
--
-- /"The first line of the file is N, the number of nodes. You will name the/
-- /nodes from 0 through N-1. The remaining lines will have two integers per/
-- /line, representing an edge."/
--
-- prop> parse "10\n0 2\n1 0"   == Just (buildG (0, 9) [(0, 2), (1, 0)])
--
-- prop> parse "5" == Just (buildG (0, 4) [])
--
-- Trailing new lines:
--
-- prop> parse "5" == Just (buildG (0, 4) [])
--
-- prop> parse "10\n0 2\n1 0\n" == Just (buildG (0, 9) [(0, 2), (1, 0)])
--
-- Invalid:
--
-- prop> parse "5a" == Nothing
--
-- prop> parse "5\n0 1a" == Nothing
--
parse :: String -> Maybe Graph
parse dat =
  case lines dat of
    []                 -> Nothing
    (sizeStr:edgesStr) -> do
      bnds <- parseNodes sizeStr >>= getBounds
      edgs <- mapM parseEdge edgesStr
      pure $ buildG bnds edgs

parseNodes :: String -> Maybe Int
parseNodes sizeStr = do
  n <- readMaybe sizeStr :: Maybe Integer
  guard (n > 0 || n < fromIntegral (maxBound :: Int))
  pure $ fromIntegral n

parseEdge :: String -> Maybe Edge
parseEdge edgeStr =
  let numbers = do
        n1 <- nat
        char ' '
        n2 <- nat
        notFollowedBy anyToken
        pure (n1, n2)
  in case TP.parse numbers "" edgeStr of
       Left _  -> Nothing
       Right e -> pure e

getBounds :: Int -> Maybe Bounds
getBounds i = do
  -- Must be a natural number. i.e. we can't have a negative number of nodes.
  guard (i > 0)
  case i of
    0 -> pure (0, 0)
    n -> pure (0, n - 1)

