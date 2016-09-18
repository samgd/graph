module Graph.Parse
  ( parse
  ) where

import           Control.Monad     (guard)
import           Data.Graph
import           Text.Parsec       (anyToken, notFollowedBy)
import qualified Text.Parsec       as TP
import           Text.Parsec.Char  (char, digit)
import           Text.Parsec.Utils (nat)
import           Text.Read         (readMaybe)

parse :: String -> Maybe Graph
parse dat =
  case lines dat of
    []                 -> Nothing
    (sizeStr:edgesStr) -> do
      bounds <- parseNodes sizeStr >>= getBounds
      edges <- mapM parseEdge edgesStr
      pure $ buildG bounds edges

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
       Left err -> error $ show err
       Right e  -> pure e

getBounds :: Int -> Maybe Bounds
getBounds i = do
  -- Must be a natural number. i.e. we can't have a negative number of nodes.
  guard (i > 0)
  case i of
    0 -> pure (0, 0)
    n -> pure (0, n - 1)

