-- | A dedicated module for mouse input types.
-- This keeps the main GameLoop module clean and organized.
module Mouse where

-- | The core data type for mouse input settings.
-- We use newtype for type safety, ensuring we can't mix up DPI and polling rate.
newtype DPI = DPI Int deriving (Show, Eq)
newtype PollingRate = PollingRate Int deriving (Show, Eq)

-- | Represents the various mouse buttons.
data MouseButton = MB1 | MB2 | MB3 | MB4 | MB5 | WheelUp | WheelDown
  deriving (Show, Eq, Enum, Bounded)

-- | Represents the state of a button, either pressed or released.
data ButtonState = Pressed | Released deriving (Show, Eq)

-- | Represents a mouse button event.
type ButtonEvent = (MouseButton, ButtonState)

-- | Represents mouse movement as a vector.
data MouseMovement = MouseMovement {
  xMovement :: Double,
  yMovement :: Double
} deriving (Show, Eq)

-- | The complete state of the mouse device.
data MouseInput = MouseInput {
  mouseDpi :: DPI,
  mousePollingRate :: PollingRate,
  mouseButtons :: [ButtonEvent],
  mousePosition :: (Double, Double),
  mouseMovement :: MouseMovement
} deriving (Show, Eq)
