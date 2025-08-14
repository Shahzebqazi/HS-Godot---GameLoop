-- | A dedicated module for keyboard input types.
-- This keeps the main GameLoop module clean and organized.
module Keyboard where

-- | Represents a standard keyboard key. This can be extended to include all
-- keys supported by a game.
data Key = KeyW | KeyA | KeyS | KeyD | KeySpace | KeyEscape | KeyEnter
  deriving (Show, Eq, Enum, Bounded)

-- | Represents a modifier key that can be held down.
data Modifier = ModShift | ModCtrl | ModAlt
  deriving (Show, Eq)

-- | A polling rate specific to the keyboard device.
newtype PollingRate = PollingRate Int deriving (Show, Eq)

-- | Represents a sequence of inputs that form a macro.
type Macro = [Key]

-- | Represents analog input from a keyboard, for devices with analog keys.
newtype AnalogInput = AnalogInput Double deriving (Show, Eq)

-- | The complete state of the keyboard device.
data KeyboardInput = KeyboardInput {
  keyboardPollingRate :: PollingRate,
  keyboardKeys :: [(Key, Bool)],
  keyboardModifiers :: [Modifier],
  keyboardMacros :: [Macro],
  keyboardAnalogInputs :: [(Key, AnalogInput)]
} deriving (Show, Eq)
