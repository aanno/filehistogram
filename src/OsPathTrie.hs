{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OsPathTrie where

import System.OsPath
import System.OsPath.Types
import qualified System.FilePath as FP
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.List (foldl')

-- | A trie structure for filesystem paths using OsPath internally
data PathTrie a = PathTrie
  { value :: Maybe a                        -- Value stored at this node
  , children :: M.Map OsPath (PathTrie a)   -- Child nodes indexed by OsPath components
  } deriving (Show, Eq)

-- | Type class for paths that can be used with the trie
class PathLike p where
  toOsPath :: p -> IO OsPath
  fromOsPath :: OsPath -> IO p

instance PathLike OsPath where
  toOsPath = return
  fromOsPath = return

instance PathLike FilePath where
  toOsPath = encodeFS
  fromOsPath = decodeFS

-- | Empty path trie
empty :: PathTrie a
empty = PathTrie Nothing M.empty

-- | Insert a path with a value into the trie
insert :: PathLike p => p -> a -> PathTrie a -> IO (PathTrie a)
insert path val trie = do
  osPath <- toOsPath path
  let components = splitPath osPath
  return $ insertComponents components val trie
  where
    insertComponents [] v (PathTrie _ children) = 
      PathTrie (Just v) children
    insertComponents (x:xs) v (PathTrie nodeVal children) =
      PathTrie nodeVal (M.alter (Just . insertComponents xs v . fromMaybe empty) x children)

-- | Lookup a value by path
lookup :: PathLike p => p -> PathTrie a -> IO (Maybe a)
lookup path trie = do
  osPath <- toOsPath path
  let components = splitPath osPath
  return $ lookupComponents components trie
  where
    lookupComponents [] (PathTrie val _) = val
    lookupComponents (x:xs) (PathTrie _ children) = 
      M.lookup x children >>= lookupComponents xs

-- | Delete a path from the trie
delete :: PathLike p => p -> PathTrie a -> IO (PathTrie a)
delete path trie = do
  osPath <- toOsPath path
  let components = splitPath osPath
  return $ deleteComponents components trie
  where
    deleteComponents [] (PathTrie _ children) = 
      PathTrie Nothing children
    deleteComponents (x:xs) (PathTrie nodeVal children) =
      PathTrie nodeVal (M.update (Just . deleteComponents xs) x children)

-- | Check if a path exists in the trie
member :: PathLike p => p -> PathTrie a -> IO Bool
member path trie = do
  result <- OsPathTrie.lookup path trie
  return $ case result of
    Nothing -> False
    Just _  -> True

-- | Combine path components using (</>)
combinePath :: [OsPath] -> OsPath
combinePath [] = mempty  -- Empty path
combinePath [x] = x
combinePath (x:xs) = foldl' (</>) x xs

-- | Get all paths with their values (returns OsPath)
toListOs :: PathTrie a -> [(OsPath, a)]
toListOs = toListWithPrefix []
  where
    toListWithPrefix prefix (PathTrie val children) =
      let currentPath = combinePath prefix
          current = case val of
            Nothing -> []
            Just v  -> [(currentPath, v)]
          subtrees = concatMap (\(name, child) -> 
            toListWithPrefix (prefix ++ [name]) child) (M.toList children)
      in current ++ subtrees

-- | Get all paths with their values (generic version)
toList :: PathLike p => PathTrie a -> IO [(p, a)]
toList trie = do
  let osPaths = toListOs trie
  mapM (\(osPath, val) -> do
    path <- fromOsPath osPath
    return (path, val)) osPaths

-- | Get all paths that have the given prefix (returns OsPath)
findWithPrefixOs :: OsPath -> PathTrie a -> [(OsPath, a)]
findWithPrefixOs prefix trie =
  let components = splitPath prefix
  in case findNode components trie of
    Nothing -> []
    Just subtrie -> 
      let subList = toListOs subtrie
      in map (\(p, v) -> 
        if p == mempty  -- If empty path (root of subtrie)
          then (prefix, v)
          else (prefix </> p, v)) subList
  where
    findNode [] node = Just node
    findNode (x:xs) (PathTrie _ children) = 
      M.lookup x children >>= findNode xs

-- | Get all paths that have the given prefix (generic version)
findWithPrefix :: PathLike p => p -> PathTrie a -> IO [(p, a)]
findWithPrefix prefix trie = do
  osPrefix <- toOsPath prefix
  let osPaths = findWithPrefixOs osPrefix trie
  mapM (\(osPath, val) -> do
    path <- fromOsPath osPath
    return (path, val)) osPaths

-- | Map a function over all values in the trie
mapValues :: (a -> b) -> PathTrie a -> PathTrie b
mapValues f (PathTrie val children) = 
  PathTrie (fmap f val) (M.map (mapValues f) children)

-- | Filter paths based on a predicate (using OsPath)
filterOs :: (OsPath -> a -> Bool) -> PathTrie a -> PathTrie a
filterOs pred = filterWithPrefix []
  where
    filterWithPrefix prefix (PathTrie val children) =
      let currentPath = combinePath prefix
          newVal = case val of
            Nothing -> Nothing
            Just v -> if pred currentPath v then Just v else Nothing
          newChildren = M.mapMaybe (\child ->
            let filtered = filterWithPrefix (prefix ++ [M.keys children !! 0]) child
            in if isEmptyTrie filtered then Nothing else Just filtered) children
      in PathTrie newVal newChildren

-- | Filter paths based on a predicate (generic version)
filter :: PathLike p => (p -> a -> IO Bool) -> PathTrie a -> IO (PathTrie a)
filter pred trie = do
  -- Convert to a pure predicate by collecting results
  let collectFiltered (PathTrie val children) prefix = do
        currentPath <- fromOsPath (combinePath prefix)
        newVal <- case val of
          Nothing -> return Nothing
          Just v -> do
            keep <- pred currentPath v
            return $ if keep then Just v else Nothing
        newChildren <- M.traverseMaybeWithKey (\name child -> do
          filtered <- collectFiltered child (prefix ++ [name])
          return $ if isEmptyTrie filtered then Nothing else Just filtered) children
        return $ PathTrie newVal newChildren
  collectFiltered trie []

-- | Check if a trie is empty
isEmptyTrie :: PathTrie a -> Bool
isEmptyTrie (PathTrie Nothing children) = M.null children
isEmptyTrie _ = False

-- | Get the size (number of stored paths) of the trie
size :: PathTrie a -> Int
size (PathTrie val children) = 
  (if isJust val then 1 else 0) + sum (map size (M.elems children))
  where
    isJust Nothing = False
    isJust (Just _) = True

-- | Fold over the trie with a path accumulator (using OsPath)
foldWithPathOs :: (OsPath -> a -> b -> b) -> b -> PathTrie a -> b
foldWithPathOs f acc = foldWithPrefix [] f acc
  where
    foldWithPrefix prefix fn acc' (PathTrie val children) =
      let currentPath = combinePath prefix
          acc'' = case val of
            Nothing -> acc'
            Just v  -> fn currentPath v acc'
      in M.foldr (\child acc''' ->
          foldWithPrefix (prefix ++ [M.keys children !! 0]) fn acc''' child) acc'' children

-- | Fold over the trie with a path accumulator (generic version)
foldWithPath :: PathLike p => (p -> a -> b -> IO b) -> b -> PathTrie a -> IO b
foldWithPath f initialAcc trie = do
  -- We need to traverse the trie and accumulate in IO
  let go prefix acc (PathTrie val children) = do
        let currentPath = combinePath prefix
        acc' <- case val of
          Nothing -> return acc
          Just v -> do
            p <- fromOsPath currentPath
            f p v acc
        M.foldrWithKey (\name child accIO -> do
          acc'' <- accIO
          go (prefix ++ [name]) acc'' child) (return acc') children
  go [] initialAcc trie

-- | Helper to traverse Map with IO
traverseMaybeWithKey :: (Ord k) => (k -> a -> IO (Maybe b)) -> M.Map k a -> IO (M.Map k b)
traverseMaybeWithKey f m = do
  pairs <- mapM (\(k, v) -> do
    result <- f k v
    return (k, result)) (M.toList m)
  return $ M.fromList [(k, v) | (k, Just v) <- pairs]

-- Example usage showing both OsPath and FilePath:
example :: IO ()
example = do
  let trie0 = empty
  
  -- Using FilePath (with type annotations to resolve ambiguity)
  trie1 <- insert ("/home/user/documents/file1.txt" :: FilePath) "content1" trie0
  trie2 <- insert ("/home/user/documents/file2.txt" :: FilePath) "content2" trie1
  
  -- Using OsPath
  path3 <- encodeFS "/home/user/downloads/file3.pdf"
  trie3 <- insert path3 "content3" trie2
  
  -- Mixed usage
  trie4 <- insert ("/home/admin/config.ini" :: FilePath) "content4" trie3
  path5 <- encodeFS "/etc/hosts"
  trie5 <- insert path5 "content5" trie4
  
  putStrLn "All paths (as FilePath):"
  allPaths <- toList trie5 :: IO [(FilePath, String)]
  mapM_ print allPaths
  
  putStrLn "\nAll paths (as OsPath):"
  allPathsOs <- toList trie5 :: IO [(OsPath, String)]
  mapM_ (\(p, v) -> do
    pStr <- decodeFS p
    putStrLn $ pStr ++ " -> " ++ v) allPathsOs
  
  putStrLn "\nPaths under /home/user (using FilePath):"
  prefixPaths <- findWithPrefix ("/home/user" :: FilePath) trie5 :: IO [(FilePath, String)]
  mapM_ print prefixPaths
  
  putStrLn "\nLookup using FilePath:"
  result1 <- OsPathTrie.lookup ("/home/user/documents/file1.txt" :: FilePath) trie5
  print result1
  
  putStrLn "\nLookup using OsPath:"
  result2 <- OsPathTrie.lookup path3 trie5
  print result2
  
  putStrLn "\nSize of trie:"
  print (size trie5)
