module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import ListT hiding (repeat)
import Data.List
import qualified Data.Map as M

type Action = StateT Network (ExceptT String IO)

data Network = Network { nodes :: M.Map NodeIndex Node
                       , ready :: Bool
                       }

data Node = Node { op :: NodeOp
                 , children :: [NodeIndex]
                 , value :: Double
                 , derivative :: Double
                 }

data NodeOp = NodeOp { eval :: [Double] -> Double
                     , grad :: [Double] -> [Double]
                     }

type NodeIndex = Int

compute :: Action a -> IO (Either String a)
compute m = runExceptT $ evalStateT m $ Network M.empty True

getNode :: NodeIndex -> Action Node
getNode i = get >>= maybe (throwError "Illegal node") pure . M.lookup i . nodes

changeNode :: NodeIndex -> (Node -> Node) -> Action ()
changeNode i f = get >>= \x -> put $ x { nodes = M.adjust f i $ nodes x }

changeReady :: Bool -> Action ()
changeReady r = get >>= \x -> put $ x { ready = r }

setOp :: NodeIndex -> NodeOp -> Action ()
setOp i op = changeNode i (\x -> x { op = op }) >> changeReady False

getDerivative :: NodeIndex -> Action Double
getDerivative = getNode >=> pure . derivative

getValue :: NodeIndex -> Action Double
getValue i = do
    node <- getNode i
    r <- ready <$> get
    if r then
        pure $ value node
    else do
        value <- (eval . op) node <$> mapM getValue (children node)

        changeNode i $ \x -> x { value = value }
        changeReady True

        pure value

calculateDerivativesOf :: NodeIndex -> Action ()
calculateDerivativesOf i = init >> go [i]
    where init = get >>= \(Network nodes ready) -> put $ Network (M.mapWithKey f nodes) ready
          f j node = if i == j then node { derivative = 1 } else node { derivative = 0 }

          go prevLayerIdxs = do
              prevLayer <- mapM getNode prevLayerIdxs
              conns <- toList $ do
                  prev <- fromFoldable prevLayer
                  values <- lift $ mapM getValue $ children prev
                  let gradients = (grad . op) prev values
                  fromFoldable $ zip3 (repeat prev) (children prev) gradients

              forM_ conns $ \(node, i, g) -> do
                  changeNode i $ \x -> x { derivative = derivative x + derivative node * g }
              go $ nub $ map (\(_, i, _) -> i) conns

node :: NodeOp -> [NodeIndex] -> Action NodeIndex
node op children = do
    (Network nodes ready) <- get
    value <- if ready then eval op <$> mapM getValue children else pure 0
    let new = Node op children value 0
        i = length nodes
    put $ Network (M.insert i new nodes) ready
    pure i

main :: IO ()
main = pure ()
