{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module VisitedPathTrie
    ( VisitedTrie
    , VisitStatus(..)
    , empty
    , markVisitedSTM
    , markCompletelyVisited
    , markCompleteAndCleanup
    , checkAndMarkVisited
    , isVisited
    , isCompletelyVisited
    , hasVisitedPrefix
    , deleteCompletedPrefix
    , size
    , stats
    ) where

import System.OsPath
import System.OsPath.Types
import qualified Data.Map.Strict as M
import Control.Concurrent.STM
import GHC.Generics (Generic)

-- | Status of a path in the trie
data VisitStatus 
    = NotVisited
    | Visiting      -- Currently being processed
    | CompletelyVisited  -- This path and all its descendants have been visited
    deriving (Show, Eq, Generic)

-- | A trie structure for tracking visited paths
data VisitedTrie = VisitedTrie
    { status :: !VisitStatus
    , children :: !(M.Map OsPath VisitedTrie)
    } deriving (Show, Eq, Generic)

-- | Empty visited trie
empty :: VisitedTrie
empty = VisitedTrie NotVisited M.empty

-- | Mark a path as being visited (but not yet complete)
markVisited :: [OsPath] -> VisitedTrie -> VisitedTrie
markVisited [] trie = trie { status = Visiting }
markVisited (x:xs) trie@(VisitedTrie s children) =
    id trie -- do nothing
--     trie { children = M.alter updateChild x children }
--   where
--     updateChild Nothing = Just $ markVisited xs empty
--     updateChild (Just child) = Just $ markVisited xs child

-- | Mark a path as completely visited (including all descendants)
markCompletelyVisited :: [OsPath] -> VisitedTrie -> VisitedTrie
markCompletelyVisited [] trie = VisitedTrie CompletelyVisited M.empty  -- Clear children
markCompletelyVisited (x:xs) trie@(VisitedTrie s children) =
    id trie -- do nothing
--     trie { children = M.alter updateChild x children }
--   where
--     updateChild Nothing = Just $ markCompletelyVisited xs empty
--     updateChild (Just child) = Just $ markCompletelyVisited xs child

-- | Check if a path has been visited (or is being visited)
isVisited :: [OsPath] -> VisitedTrie -> Bool
isVisited components trie = 
    case lookupStatus components trie of
        NotVisited -> False
        _ -> True

-- | Check if a path has been completely visited
isCompletelyVisited :: [OsPath] -> VisitedTrie -> Bool
isCompletelyVisited components trie =
    case lookupStatus components trie of
        CompletelyVisited -> True
        _ -> False

-- | Check if any prefix of the path has been completely visited
-- This is crucial for the optimization: if /a/b is completely visited,
-- then /a/b/c/d is also implicitly visited
hasVisitedPrefix :: [OsPath] -> VisitedTrie -> Bool
hasVisitedPrefix = checkPrefix
  where
    checkPrefix [] (VisitedTrie CompletelyVisited _) = True
    checkPrefix [] _ = False
    checkPrefix (x:xs) (VisitedTrie CompletelyVisited _) = True  -- Found complete prefix
    checkPrefix (x:xs) (VisitedTrie _ children) =
        case M.lookup x children of
            Nothing -> False
            Just child -> checkPrefix xs child

-- | Lookup the status of a path
lookupStatus :: [OsPath] -> VisitedTrie -> VisitStatus
lookupStatus [] (VisitedTrie s _) = s
lookupStatus (x:xs) (VisitedTrie _ children) =
    case M.lookup x children of
        Nothing -> NotVisited
        Just child -> lookupStatus xs child

-- | Delete completed prefixes to save memory
-- This removes all descendants of paths marked as CompletelyVisited
deleteCompletedPrefix :: [OsPath] -> VisitedTrie -> VisitedTrie
deleteCompletedPrefix [] trie@(VisitedTrie CompletelyVisited _) = 
    trie { children = M.empty }  -- Clear all children
deleteCompletedPrefix [] trie = trie
deleteCompletedPrefix (x:xs) trie@(VisitedTrie s children) =
    case s of
        CompletelyVisited -> trie  -- Already complete, no need to go deeper
        _ -> trie { children = M.update (Just . deleteCompletedPrefix xs) x children }

-- | Get the size (number of nodes) in the trie
size :: VisitedTrie -> Int
size (VisitedTrie _ children) = 
    1 + sum (map size (M.elems children))

-- | Get statistics about the trie
stats :: VisitedTrie -> (Int, Int, Int, Int)  -- (total, notVisited, visiting, complete)
stats = go (0, 0, 0, 0)
  where
    go (!total, !notVis, !vis, !comp) (VisitedTrie s children) =
        let (t', n', v', c') = case s of
                NotVisited -> (1, 1, 0, 0)
                Visiting -> (1, 0, 1, 0)
                CompletelyVisited -> (1, 0, 0, 1)
            acc = (total + t', notVis + n', vis + v', comp + c')
        in M.foldl' go acc children

-- STM operations for thread-safe access

-- | Thread-safe mark as visited
markVisitedSTM :: [OsPath] -> TVar VisitedTrie -> STM ()
markVisitedSTM path tvar = modifyTVar' tvar (markVisited path)

-- | Thread-safe mark as completely visited
markCompletelyVisitedSTM :: [OsPath] -> TVar VisitedTrie -> STM ()
markCompletelyVisitedSTM path tvar = modifyTVar' tvar (markCompletelyVisited path)

-- | Thread-safe check if visited
isVisitedSTM :: [OsPath] -> TVar VisitedTrie -> STM Bool
isVisitedSTM path tvar = isVisited path <$> readTVar tvar

-- | Thread-safe check if any prefix is completely visited
hasVisitedPrefixSTM :: [OsPath] -> TVar VisitedTrie -> STM Bool
hasVisitedPrefixSTM path tvar = hasVisitedPrefix path <$> readTVar tvar

-- | Thread-safe delete completed prefixes
deleteCompletedPrefixSTM :: [OsPath] -> TVar VisitedTrie -> STM ()
deleteCompletedPrefixSTM path tvar = modifyTVar' tvar (deleteCompletedPrefix path)

-- | Combined operation: check if visited and mark if not
checkAndMarkVisited :: [OsPath] -> TVar VisitedTrie -> STM Bool
checkAndMarkVisited path tvar = do
    trie <- readTVar tvar
    if hasVisitedPrefix path trie || isVisited path trie
        then return True  -- Already visited
        else do
            writeTVar tvar (markVisited path trie)
            return False  -- Not visited, but marked now

-- | Mark directory scan complete and clean up
markCompleteAndCleanup :: [OsPath] -> TVar VisitedTrie -> STM ()
markCompleteAndCleanup path tvar = do
    modifyTVar' tvar (markCompletelyVisited path)
    modifyTVar' tvar (deleteCompletedPrefix path)
