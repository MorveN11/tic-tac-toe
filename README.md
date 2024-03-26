# Tic-Tac-Toe Game in Haskell by Manuel Morales

## Introduction
In this project, two implementations of the Tic-Tac-Toe game were developed in Haskell: one for the console and another with a graphical interface using the Gloss library. Both implementations share the same logic behind the game, separating the rendering part of the application from the game logic itself.

## Implementation Approach
- **Modular Structure:** The game logic was separated into an independent module that handles the Tic-Tac-Toe rules, while another module takes care of rendering, whether in the console or using the Gloss graphical interface.
  
- **Record Syntax Usage:** Record syntax was employed to define the game's data structure, including the board, players, and game state (ongoing or finished).

- **Gloss Graphical Interface:** The Gloss library was chosen for implementing the graphical interface due to its simplicity and ease of use. Functions were created to handle user interaction with the graphical interface and update the game state.

- **Move Handling:** Both implementations share the same logic for handling player moves, verifying the validity of moves, and determining if there is a winner or a draw.

## Design Decisions
- **Responsibility Separation:** Separating the game logic from the graphical interface allowed for greater modularity and code reusability.

- **Record Syntax:** Using record syntax made it easier to manipulate different components of the game, such as the board and players.

- **Gloss Library Usage:** Although the Gloss library presented challenges, its simplicity and graphical functionalities were crucial for implementing the graphical interface.

## Challenges Encountered
The main challenge during this project was understanding and using the Gloss library for the graphical interface. Obtaining data and understanding the logic behind its methods and functions required detailed and careful learning. However, once this challenge was overcome, the graphical interface was successfully integrated with the game logic.

## Conclusions
The implementation of the Tic-Tac-Toe game in Haskell has been successful, meeting the set objectives. The separation of game logic and the graphical interface allowed for a modular and extensible implementation, while the use of record syntax facilitated data manipulation. Despite the challenges encountered, the combination of the Gloss library and the game logic provides a satisfactory gaming experience for users.
