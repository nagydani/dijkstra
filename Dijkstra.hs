module Dijkstra (dijkstra) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (foldl', lookup)

type CostMap cost state = (M.Map state cost, S.Set (cost, state))

nearest ::
    (Ord state, Ord cost)
    => CostMap cost state
    -> Maybe state
nearest (_, s) = if S.null s
    then Nothing
    else Just $ snd $ S.findMin s

insert ::
    (Ord state, Ord cost)
    => state
    -> cost
    -> CostMap cost state
    -> CostMap cost state
insert p d (m, s) =
    if M.notMember p m
        then (M.insert p d m, S.insert (d, p) s)
        else let d' = m M.! p
             in if d' < d
                 then (m, s)
                 else (M.insert p d m, S.insert (d, p) $ S.delete (d', p) s)

delete ::
    (Ord state, Ord cost)
    => state
    -> CostMap cost state
    -> CostMap cost state
delete p (m, s) =
    if M.notMember p m
        then (m, s)
        else (M.delete p m, S.delete (m M.! p, p) s)

singleton ::
    (Ord state, Num cost)
    => state
    -> CostMap cost state
singleton p = (M.singleton p 0, S.singleton (0, p))

addCost ::
    (Ord state, Ord cost, Num cost)
    => state
    -> (CostMap cost state, M.Map state state)
    -> (state, cost)
    -> (CostMap cost state, M.Map state state)
addCost current ((m, s), previous) (p, d) =
    if M.notMember current m
        then ((m, s), previous)
        else ( insert p ((m M.! current) + d) (m, s)
             , M.insert p current previous)

path ::
    (Ord state)
    => M.Map state state
    -> state
    -> [state]
path previous current
    | M.notMember current previous = [current]
    | otherwise = current : (path previous $ previous M.! current)

dijkstra' ::
    (Ord state, Ord cost, Num cost)
    => (state -> [(state, cost)])
    -> (state -> Bool)
    -> S.Set state
    -> M.Map state state
    -> CostMap cost state
    -> state
    -> Maybe (cost, [state])
dijkstra' next found visited previous costs current =
    let unvisitedNeighbors = filter (\(x, _) -> S.notMember x visited) $ 
                             next current
    in let (costs', previous') = foldl' (addCost current)
                                     (costs, previous) unvisitedNeighbors
           visited' = S.insert current visited
           costs'' = delete current costs'
       in if found current
           then Just ( fst costs' M.! current
                     , reverse $ path previous current)
           else do
               current' <- nearest costs''
               dijkstra' next found visited' previous' costs'' current'

-- | @dijkstra next found initial@ performs a shortest-path search over
-- a set of states using Dijkstra's algorithm, starting with @initial@,
-- generating neighboring states with associated incremenal costs with
-- @next@. This will find the least-costly path from an initial state to a
-- state for which @found@ returns 'True'. Returns 'Nothing' if no path to a
-- solved state is possible.
dijkstra ::
    (Ord state, Ord cost, Num cost)
    => (state -> [(state, cost)])
    -- ^ Function to generate list of neighboring states with associated costs
    -> (state -> Bool)
    -- ^ Predicate to determine if solution found. 'dijkstra' returns the
    -- shortest path to the first state for which this predicate returns 'True'.
    -> state
    -- ^ Initial state
    -> Maybe (cost, [state])
    -- ^ (Total cost, list of steps) for the first path found which satisfies
    -- the given predicate
dijkstra next found initial =
    dijkstra' next found S.empty M.empty (singleton initial) initial
