package battleship

import scala.annotation.tailrec
import scala.collection.immutable.Map

import GameState._
import Player._
import Board._
import Ship._
import Utils._
import CellType._

/**
  * Entry point of the application (main)
  */
object Game extends App {

//    val ships: Map[String, Int] = Map("Carrier" -> 5, "BattleShip" -> 4, "Cruiser" -> 3, "Submarine" -> 3, "Destroyer" -> 2)
    val ships: Map[String, Int] = Map("Cruiser" -> 3)

    initializeConsole()
    displayMessage(s"Let's play!")
    choseMode()


    def choseMode(): Unit = {
        val mode = askUserChoseMode()
        mode match {
            case 1 =>  {
                val player1 = createPlayer(1, true)
                val player2 = createPlayer(2, true)
                val initialGameState = GameState(player1, player2, player1)
                mainLoop(initialGameState)
            }
            case 2 => {
                displayMessage(s"Human vs AI (easy)")
            }
            case 3 => {
                displayMessage(s"Human vs AI (medium)")
            }
            case 4 => {
                displayMessage(s"Human vs AI (hard)")
            }
            case _ => {
                displayError("This is not a valid option!")
                choseMode()
            }
        }
    }

    def mainLoop(gameState: GameState): GameState = {
        clearConsole()

        val activePlayerWithShips = userPlaceShips(ships, gameState.active)

        // boardToString(activePlayerWithShips.shipsBoard)
        clearConsole()

        val opponentWithShips = userPlaceShips(ships, gameState.opponent)

        // boardToString(opponentWithShips.shipsBoard)
        clearConsole()

        val newGameState = gameState.copy(active = activePlayerWithShips, opponent = opponentWithShips)

        @tailrec
        def playerTurn(gameState: GameState): GameState = {

            // TODO: display active player's boards


            displayMessage(s"${gameState.active.name} to shoot")
            val shotXPos = askUserForPosition("x")
            val shotYPos = askUserForPosition("y")

            if(gameState.active.hitsBoard.isPositionValid(shotXPos, shotYPos)) {
                val updatedPlayersAfterShot = gameState.active.fireAtCell(shotXPos, shotYPos, gameState.opponent)

                askUserToContinue()

                val updatedGameState = gameState.copy(active = updatedPlayersAfterShot.head, opponent = updatedPlayersAfterShot.last)

                if(!updatedGameState.opponent.isAlive) {
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
                        case 1 => mainLoop(newGameState)
                        case _ => {
                            displayMessage(s"END OF GAME")
                            displayMessage(s"Scores : ")
                            displayMessage(s"${newGameState.active.name}  ${newGameState.active.score} - ${newGameState.opponent.score}  ${newGameState.opponent.name}")
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
        playerTurn(newGameState)
    }

    def resetPlayer(player: Player): Player = {
        val emptyShipsGrid = Board(List.fill(10)(List.fill(10)(WATER)))
        val emptyHitsGrid = Board(List.fill(10)(List.fill(10)(WATER)))
        player.copy(shipsBoard = emptyShipsGrid, hitsBoard = emptyHitsGrid, fleet = List())
    }

    def createPlayer(num: Int, isHuman: Boolean): Player = {
        clearConsole()
        val name = askUserForName(num)
        val emptyShipsGrid = Board(List.fill(10)(List.fill(10)(WATER)))
        val emptyHitsGrid = Board(List.fill(10)(List.fill(10)(WATER)))
        Player(name, isHuman, emptyShipsGrid, emptyHitsGrid)
    }

    def userPlaceShips(shipsToPlace: Map[String, Int], player: Player): Player = {
        askUserToPlaceShips(player.name)
        @tailrec
        def placeShips(shipsToPlace: Map[String, Int], player: Player): Player = {
            if(shipsToPlace.isEmpty) player
            else {
                val shipToPlace = shipsToPlace.head

                // TODO: display ships board

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
}