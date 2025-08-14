-- | A dedicated module for game state management types.
-- This module defines the different states the game can be in,
-- allowing the main game loop to behave differently for each.
module StateManagement where

-- | Represents the different states of the game.
-- This is a simple sum type that can be extended as needed.
data GameState =
    MainMenu
  | InGame
  | Paused
  | GameOver
  deriving (Show, Eq)
