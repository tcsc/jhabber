module RoutingTable(Resource,
                    Registration,
                    RoutingTable,
                    newRoutingTable,
                    newRegistration,
                    lookupRegistration,
                    updateRegistration,
                    newResource,
                    lookupResource,
                    updateResource,
                    resId,
                    resActive,
                    resConn) where

import qualified Data.Map as Map
import Test.HUnit
import {-# SOURCE #-} Connection
import Xmpp


-- | Holds the details of a bound resource
data Resource = Resource {
  resId :: JID,
  resConn :: Connection,
  resActive :: Bool
}

-- | Holds a mapping between a single user login and the various resources bound
--   to that login. The map is indexed by the resource id.
type Registration = Map.Map String Resource

-- | Holds a mapping between logged in users and the resource maps to their
--   bound resources
type RoutingTable = Map.Map String Registration

{- -------------------------------------------------------------------------- -}

newRoutingTable :: RoutingTable
newRoutingTable = Map.empty

newRegistration :: Registration
newRegistration = Map.empty

lookupRegistration :: JID -> RoutingTable -> Maybe Registration
lookupRegistration (JID node _ _) table = Map.lookup node table

updateRegistration :: JID -> Registration -> RoutingTable -> RoutingTable
updateRegistration (JID node _ _) reg table =
  Map.insert node reg table

newResource :: JID -> Connection -> Resource
newResource jid conn = Resource {resId = jid, resConn = conn, resActive = False }

lookupResource :: JID -> RoutingTable -> Maybe (Registration, Resource)
lookupResource jid@(JID node _ rid) table = do
    reg <- lookupRegistration jid table
    res <- getResource rid reg
    return (reg, res)
  where
    getResource :: String -> Registration -> Maybe Resource
    getResource "" r = Just $ second $ Map.elemAt 0 r
    getResource id r = Map.lookup id r

    second (_,x) = x


addResource :: Resource -> Registration -> Registration
addResource res reg =
  let rid = jidResource (resId res)
  in Map.insert rid res reg

updateResource :: JID -> Registration -> Resource -> RoutingTable -> RoutingTable
updateResource jid reg res table =
  let reg' = addResource res reg
  in updateRegistration jid reg' table

{- -------------------------------------------------------------------------- -}

tests = test [
  "Reg Created Empty" ~: 0 ~=? (Map.size newRegistration)
  ]
