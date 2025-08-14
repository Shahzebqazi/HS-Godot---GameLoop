-- | A dedicated module for Entity-Component-System (ECS) types.
-- This module defines the core components of the ECS architecture,
-- which will be used to generate modular game logic in Godot.
module ECS where

import qualified Data.Map as M

-- | An Entity is a simple identifier, typically an integer.
newtype Entity = Entity Int deriving (Show, Eq, Ord)

-- | A Component is a piece of data associated with an Entity.
-- The String here is a placeholder for the actual data structure
-- that would be defined for each component (e.g., a Position component).
type Component = String

-- | The core data structure of the ECS.
-- A map from an Entity to a map of its Components.
type World = M.Map Entity (M.Map String Component)

-- | A data type representing an ECS-style system.
-- Systems are run on a fixed timestep and contain their own logic.
data System = System {
  systemName :: String,
  systemLogic :: String -- GDScript code for the system's logic
} deriving (Show)
