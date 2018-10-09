package battleship

import scala.annotation.tailrec
import scala.collection.immutable.Map
import scala.util.Random

import Board._
import CellType._
import GameState._
import Player._
import Ship._
import Utils._

/**
  * Entry point of the application (main)
  */
object Game extends App {

    val ships: Map[String, Int] = Map("Carrier" -> 5, "BattleShip" -> 4, "Cruiser" -> 3, "Submarine" -> 3, "Destroyer" -> 2)

    initializeConsole()
    displayMessage(s"Let's play!")
    chooseMode()


    /**
      * Choose playing mode between:
      * - human vs human
      * - human vs IA (with different levels of difficulty)
      * - IA vs IA (with different levels of difficulty)
      */
    def chooseMode(): Unit = {
        val mode = askUserChooseMode()
        mode match {
            case 1 =>  {
                val player1 = createPlayer(1)
                val player2 = createPlayer(2)
                val initialGameState = GameState(player1, player2, player1)
                mainLoop(initialGameState)
            }
            case 2 => {
                val player1 = createPlayer(1)
                val player2 = createAI("AI-easy")
                val initialGameState = GameState(player1, player2, player1)
                mainLoop(initialGameState)
            }
            case 3 => {
                val player1 = createPlayer(1)
                val player2 = createAI("AI-medium")
                val initialGameState = GameState(player1, player2, player1)
                mainLoop(initialGameState)
            }
            case 4 => {
                val player1 = createPlayer(1)
                val player2 = createAI("AI-hard")
                val initialGameState = GameState(player1, player2, player1)
                mainLoop(initialGameState)
            }
            case 5 => {
                val numberOfSimulations = askNumberOfSimulations()
                val player1 = createAI("AI-easy")
                val player2 = createAI("AI-medium")
                val initialGameState = GameState(player1, player2, player1)
                mainLoopAIvsAI(initialGameState, numberOfSimulations, 1)
            }
            case 6 => {
                val numberOfSimulations = askNumberOfSimulations()
                val player1 = createAI("AI-easy")
                val player2 = createAI("AI-hard")
                val initialGameState = GameState(player1, player2, player1)
                mainLoopAIvsAI(initialGameState, numberOfSimulations, 1)
            }
            case 7 => {
                val numberOfSimulations = askNumberOfSimulations()
                val player1 = createAI("AI-medium")
                val player2 = createAI("AI-hard")
                val initialGameState = GameState(player1, player2, player1)
                mainLoopAIvsAI(initialGameState, numberOfSimulations, 1)
            }
            case 8 => {
                displayError("Coming soon!")
//                val numberOfSimulations = askNumberOfSimulations()
//                val player1 = createAI("AI-easy")
//                val player2 = createAI("AI-medium")
//                val player3 = createAI("AI-hard")
//
//                val initialGameState1 = GameState(player1, player2, player1)
//                val initialGameState2 = GameState(player1, player3, player1)
//                val initialGameState3 = GameState(player2, player3, player2)
//
//                mainLoopAIvsAI(initialGameState1, numberOfSimulations, 1)
//                mainLoopAIvsAI(initialGameState2, numberOfSimulations, 1)
//                mainLoopAIvsAI(initialGameState3, numberOfSimulations, 1)
            }
            case _ => {
                displayError("This is not a valid option!")
                chooseMode()
            }
        }
    }

    /**
      * Main loop of the game (human vs human or human vs IA): place ships then play turn by turn
      * @param gameState the current state of the game (players state, mostly for scores)
      * @return the last state of the game
      */
    @tailrec
    def mainLoop(gameState: GameState): GameState = {
        clearConsole()

        val activePlayerWithShips = if(gameState.active.isHuman) userPlaceShips(ships, gameState.active) else aiPlaceShips(ships, gameState.active)

        displayBoard(activePlayerWithShips.shipsBoard)
        if(gameState.active.isHuman) askUserToContinue()
        clearConsole()

        val opponentWithShips = if(gameState.opponent.isHuman) userPlaceShips(ships, gameState.opponent) else aiPlaceShips(ships, gameState.opponent)

        displayBoard(opponentWithShips.shipsBoard)
        if(gameState.active.isHuman) askUserToContinue()
        clearConsole()

        val updatedGameState = gameState.copy(active = activePlayerWithShips, opponent = opponentWithShips)

        /**
          * Logic for a player turn: fire at opponent
          * @param gameState the current state of the players (grids and fleet)
          * @return the new state of the game after the player's turn (grids and fleet updated)
          */
        @tailrec
        def playerTurn(gameState: GameState): GameState = {

            displayMessage(s"${gameState.active.name}'s turn\n")

            displayMessage("--> Your hits board")
            displayBoard(gameState.active.hitsBoard)
            displayMessage("--> Your ships board")
            displayBoard(gameState.active.shipsBoard)

            val aiShootPos = gameState.active.chooseTarget(gameState.active.name, Random, Random)

            val shotXPos = if(gameState.active.isHuman) askUserForPosition("x") else aiShootPos._1
            val shotYPos = if(gameState.active.isHuman) askUserForPosition("y") else aiShootPos._2

            if(gameState.active.hitsBoard.isPositionValid(shotXPos, shotYPos)) {
                val updatedPlayersAfterShot = gameState.active.fireAtCell(shotXPos, shotYPos, gameState.opponent)

                if(gameState.active.isHuman) askUserToContinue()

                val updatedGameState = gameState.copy(active = updatedPlayersAfterShot._1, opponent = updatedPlayersAfterShot._2)

                if(!updatedGameState.opponent.isAlive) {
                    clearConsole()

                    displayMessage(s"${updatedGameState.active.name} wins!")
                    val updatedScore = updatedGameState.active.score + 1
                    val updatedWinner = updatedGameState.active.copy(score = updatedScore)

                    val newPlayer1 = resetPlayer(updatedWinner)
                    val newPlayer2 = resetPlayer(updatedGameState.opponent)

                    val newGameState = updatedGameState.firstPlayer match {
                        case updatedGameState.opponent => updatedGameState.copy(active = newPlayer1, opponent = newPlayer2, firstPlayer = newPlayer1)
                        case _ => updatedGameState.copy(active = newPlayer2, opponent = newPlayer1, firstPlayer = newPlayer2)
                    }

                    val isNewGame = askUserForNewGame()
                    isNewGame match {
                        case 1 => newGameState
                        case 2 => {
                            displayMessage(s"END OF GAME")
                            if(newGameState.active.score == newGameState.opponent.score) displayMessage("It's a tie!")
                            else {
                                val overallWinner = if(newGameState.active.score < newGameState.opponent.score) newGameState.opponent.name else newGameState.active.name
                                displayMessage(s"$overallWinner is the overall winner!")
                            }
                            displayMessage(s"Scores :  ${newGameState.active.name}  ${newGameState.active.score} - ${newGameState.opponent.score}  ${newGameState.opponent.name}")
                            writeToCSV(newGameState)
                            askUserChooseMode()
                            newGameState
                        }
                        case _ => {
                            displayMessage(s"END OF GAME")
                            if(newGameState.active.score == newGameState.opponent.score) displayMessage("It's a tie!")
                            else {
                                val overallWinner = if(newGameState.active.score < newGameState.opponent.score) newGameState.opponent.name else newGameState.active.name
                                displayMessage(s"$overallWinner is the overall winner!")
                            }
                            displayMessage(s"Scores :  ${newGameState.active.name}  ${newGameState.active.score} - ${newGameState.opponent.score}  ${newGameState.opponent.name}")
                            writeToCSV(newGameState)
                            newGameState
                        }
                    }
                }
                else {
                    clearConsole()
                    playerTurn(updatedGameState.switchPlayers)
                }
            }
            else {
                displayError("This position is off grid!")
                playerTurn(gameState)
            }
        }
        val newGameState = playerTurn(updatedGameState)
        mainLoop(newGameState)
    }


    /**
      * Main loop of the game for AI vs AI: place ships then play turn by turn
      * @param gameState the current state of the game (players state, mostly for scores)
      * @param numberOfGamesToPlay
      * @param currentGameNumber
      * @return
      */
    @tailrec
    def mainLoopAIvsAI(gameState: GameState, numberOfGamesToPlay: Int, currentGameNumber: Int): GameState = {
        clearConsole()
        displayMessage(s"Game number : $currentGameNumber/$numberOfGamesToPlay")

        val activePlayerWithShips = aiPlaceShips(ships, gameState.active)
        val opponentWithShips = aiPlaceShips(ships, gameState.opponent)

        val updatedGameState = gameState.copy(active = activePlayerWithShips, opponent = opponentWithShips)

        /**
          * Logic for an AI player turn: fire at opponent
          * @param gameState the current state of the players (grids and fleet)
          * @return the new state of the game after the player's turn (grids and fleet updated)
          */
        @tailrec
        def playerTurn(gameState: GameState): GameState = {

            val aiShootPos = gameState.active.chooseTarget(gameState.active.name, Random, Random)

            if(gameState.active.hitsBoard.isPositionValid(aiShootPos._1, aiShootPos._2)) {
                val updatedPlayersAfterShot = gameState.active.fireAtCell(aiShootPos._1, aiShootPos._2, gameState.opponent)

                val updatedGameState = gameState.copy(active = updatedPlayersAfterShot._1, opponent = updatedPlayersAfterShot._2)

                if(!updatedGameState.opponent.isAlive) {
                    val updatedScore = updatedGameState.active.score + 1
                    val updatedWinner = updatedGameState.active.copy(score = updatedScore)

                    val newPlayer1 = resetPlayer(updatedWinner)
                    val newPlayer2 = resetPlayer(updatedGameState.opponent)

                    val newGameState = updatedGameState.firstPlayer match {
                        case updatedGameState.opponent => updatedGameState.copy(active = newPlayer1, opponent = newPlayer2, firstPlayer = newPlayer1)
                        case _ => updatedGameState.copy(active = newPlayer2, opponent = newPlayer1, firstPlayer = newPlayer2)
                    }

                    val isNewGame = if(currentGameNumber < numberOfGamesToPlay) 1 else 2
                    isNewGame match {
                        case 1 => newGameState
                        case 2 => {
                            displayMessage(s"END OF GAME")
                            if(newGameState.active.score == newGameState.opponent.score) displayMessage("It's a tie!")
                            else {
                                val overallWinner = if(newGameState.active.score < newGameState.opponent.score) newGameState.opponent.name else newGameState.active.name
                                displayMessage(s"$overallWinner is the overall winner!")
                            }
                            displayMessage(s"Scores :  ${newGameState.active.name}  ${newGameState.active.score} - ${newGameState.opponent.score}  ${newGameState.opponent.name}")
                            writeToCSV(newGameState)
                            chooseMode()
                            newGameState
                        }
                        case _ => {
                            displayMessage(s"END OF GAME")
                            if(newGameState.active.score == newGameState.opponent.score) displayMessage("It's a tie!")
                            else {
                                val overallWinner = if(newGameState.active.score < newGameState.opponent.score) newGameState.opponent.name else newGameState.active.name
                                displayMessage(s"$overallWinner is the overall winner!")
                            }
                            displayMessage(s"Scores :  ${newGameState.active.name}  ${newGameState.active.score} - ${newGameState.opponent.score}  ${newGameState.opponent.name}")
                            writeToCSV(newGameState)
                            newGameState
                        }
                    }
                }
                else {
                    playerTurn(updatedGameState.switchPlayers)
                }
            }
            else {
                playerTurn(gameState)
            }
        }
        val newGameState = playerTurn(updatedGameState)
        mainLoopAIvsAI(newGameState, numberOfGamesToPlay, currentGameNumber + 1)
    }

    /**
      * Reset player grids but keep all other informations
      * @param player the player to reset the grids for
      * @return the reset player
      */
    def resetPlayer(player: Player): Player = {
        val emptyShipsGrid = Board(List.fill(10)(List.fill(10)(WATER)))
        val emptyHitsGrid = Board(List.fill(10)(List.fill(10)(WATER)))
        player.copy(shipsBoard = emptyShipsGrid, hitsBoard = emptyHitsGrid, fleet = List())
    }

    /**
      * Create a human player
      * @param num the number of the player (for display purpose)
      * @return the new Player created
      */
    def createPlayer(num: Int): Player = {
        clearConsole()
        val name = askUserForName(num)
        val emptyShipsGrid = Board(List.fill(10)(List.fill(10)(WATER)))
        val emptyHitsGrid = Board(List.fill(10)(List.fill(10)(WATER)))
        Player(name, true, emptyShipsGrid, emptyHitsGrid)
    }

    /**
      * Create an AI player
      * @param aiName the name of the AI (corresponding to it's level)
      * @return the new Player created
      */
    def createAI(aiName: String): Player = {
        val emptyShipsGrid = Board(List.fill(10)(List.fill(10)(WATER)))
        val emptyHitsGrid = Board(List.fill(10)(List.fill(10)(WATER)))
        Player(aiName, false, emptyShipsGrid, emptyHitsGrid)
    }

    /**
      * User place ships on board
      * @param shipsToPlace Map of the ships to place
      * @param player the player placing its ships
      * @return the player with placed ships
      */
    def userPlaceShips(shipsToPlace: Map[String, Int], player: Player): Player = {
        askUserToPlaceShips(player.name)

        /**
          * Recursive funcftion to place the ships on the board
          * @param shipsToPlace the list of ships still not placed
          * @param player the player placing its ships
          * @return the player with placed ships
          */
        @tailrec
        def placeShips(shipsToPlace: Map[String, Int], player: Player): Player = {
            if(shipsToPlace.isEmpty) player
            else {
                val shipToPlace = shipsToPlace.head

                displayBoard(player.shipsBoard)
                askUserToPlaceShip(shipToPlace._1, shipToPlace._2)

                val xPos = askUserForPosition("x")
                val yPos = askUserForPosition("y")

                val direction = askUserForShipDirection() match {
                    case 1 => "VERTICAL"
                    case _ => "HORIZONTAL"
                }

                val newShip = Ship(shipToPlace._1, shipToPlace._2, direction)
                val newShipWithPos = newShip.createPositions(xPos, yPos, List())

                if (player.shipsBoard.canPlaceShip(newShipWithPos)) {
                    val updatedPlayerShipsBoard = player.shipsBoard.placeShip(newShipWithPos)
                    val updatedPlayerFleet = newShipWithPos :: player.fleet
                    val updatedPlayer = player.copy(shipsBoard = updatedPlayerShipsBoard, fleet = updatedPlayerFleet)

                    clearConsole()

                    placeShips(shipsToPlace.tail, updatedPlayer)
                }
                else {
                    displayError(s"The ship is outside the grid or overlaps another ship!")
                    placeShips(shipsToPlace, player)
                }
            }
        }
        placeShips(shipsToPlace, player)
    }


    /**
      * AI place ships randomly on board
      * @param shipsToPlace Map of the ships to place
      * @param player the player placing its ships
      * @return the player with placed ships
      */
    def aiPlaceShips(shipsToPlace: Map[String, Int], player: Player): Player = {

        /**
          * Recursive funcftion to place the ships on the board
          * @param shipsToPlace the list of ships still not placed
          * @param player the player placing its ships
          * @return the player with placed ships
          */
        @tailrec
        def placeShips(shipsToPlace: Map[String, Int], player: Player): Player = {
            if(shipsToPlace.isEmpty) player
            else {
                val shipToPlace = shipsToPlace.head

                val randPos = generateRandomPosition(Random, Random)
                val direction = generateRandomDirection(Random)

                val newShip = Ship(shipToPlace._1, shipToPlace._2, direction)
                val newShipWithPos = newShip.createPositions(randPos._1, randPos._2, List())

                if (player.shipsBoard.canPlaceShip(newShipWithPos)) {
                    val updatedPlayerShipsBoard = player.shipsBoard.placeShip(newShipWithPos)
                    val updatedPlayerFleet = newShipWithPos :: player.fleet
                    val updatedPlayer = player.copy(shipsBoard = updatedPlayerShipsBoard, fleet = updatedPlayerFleet)

                    placeShips(shipsToPlace.tail, updatedPlayer)
                }
                else {
                    placeShips(shipsToPlace, player)
                }
            }
        }
        placeShips(shipsToPlace, player)
    }
}