# Battleship
Battleship in functional programming (Scala)

## Installation

### Requirements

* Scala > 2.12.5 (requires Java 8)
* SBT > 1.1.1


## Usage

Place yourself in the main directory (Batteleship/battleship)
You can use the commands
* `sbt compile` to compile the project
* `sbt run` to run the program


## How to play

You will be asked to chose your game mode between:
* Human vs Human
* Human vs AI
* AI vs AI

#### Levels of Artificial Intelligence
You will have access to 3 different AI levels
* Level EASY: always shoots randomly (with duplicates)
* Level MEDIUM: shoots randomly but never twice on the same spot
* Level HARD: determines the next shoot depending on the last cell hit (when it finds a ship it shoots on it until it is sunk)
