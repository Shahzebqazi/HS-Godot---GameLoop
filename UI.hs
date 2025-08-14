-- | A dedicated module for UI types, used for generating Godot UI nodes.
module UI where

-- | A simple type representing a UI element.
-- The String is a placeholder for the element's properties (e.g., text, position).
data UIElement = Button String | Label String | Container [UIElement]
  deriving (Show, Eq)

-- | A type for a complete UI scene.
newtype GameUI = GameUI [UIElement]
  deriving (Show, Eq)
