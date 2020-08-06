module DefaultMap where

import qualified Data.Map as Map
import Data.Maybe

data DefaultMap k v = DefaultMap v (Map.Map k v)

empty :: v -> DefaultMap k v
empty v = DefaultMap v Map.empty


insert :: (Ord k) => k -> v -> DefaultMap k v -> DefaultMap k v
insert k v (DefaultMap dv m) = DefaultMap dv (Map.insert k v m)

lookup :: Ord k => k -> DefaultMap k v -> v
lookup k (DefaultMap dv m) = fromMaybe dv (Map.lookup k m)

-- mapVal :: Ord k => (v -> v) -> k -> DefaultMap k v -> DefaultMap k v
-- mapVal f k m = 

