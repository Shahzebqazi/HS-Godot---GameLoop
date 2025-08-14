## 🙋 User Stories

- As a developer, I want to use a type-safe language like Haskell to generate reliable Godot scripts and catch errors early.
- As a developer, I want my game to have a clear state machine to handle transitions between the main menu, gameplay, and pause screens.
- As a developer, I want my game to accept both mouse and keyboard inputs, with context-aware logic for menus and gameplay.
- As a developer, I want my game's logic to run at a fixed timestep for consistent gameplay, independent of hardware performance.
- As a developer, I want to use interpolation for smooth visuals, even with a fixed timestep.
- As a developer, I want to use a modular ECS architecture to organize game logic and make it scalable.
- As a developer, I want to use multithreading for heavy tasks to maintain a high, stable frame rate.

## ✅ Functional Requirements

- The Haskell program must generate a syntactically correct and runnable GDScript file.
- The generated GDScript must implement a fixed timestep accumulator for game logic and a variable update rate for rendering.
- The script must calculate and provide an alpha value for interpolation in a `render_interpolated` function.
- The script must correctly handle both mouse and keyboard input events.
- The generated code must use a Godot enum for `GameState` and a match statement for state-based logic.
- The script must generate a Godot class for each `System` defined in `ECS.hs`.
- The script must include a placeholder for multithreading and UI event handling.

## ⚙️ Non-Functional Requirements

- **Performance:** The generated code must be optimized to run efficiently without introducing unnecessary overhead.
- **Maintainability:** The GDScript must be clean, well-commented, and easy for a developer to understand and modify.
- **Extensibility:** The Haskell data types must be easy to extend with new input types or ECS system parameters.
- **Robustness:** The generated code must gracefully handle variable frame rates to ensure stable game logic.
```
