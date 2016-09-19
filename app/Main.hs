import           Data.Graph         (components, outdegree)
import qualified Graph.Parse
import           System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  (path:_) <- getArgs
  dat <- readFile path
  case Graph.Parse.parse dat of
    Nothing    -> do
      prog <- getProgName
      putStrLn $ prog ++ ": failed to parse data file"
    Just graph -> do
      print $ length $ components graph
      print $ outdegree graph

