package battleship

// import GameState._
// import Player._
import Board._
import Ship._
import Utils._
import CellType._

object Game extends App {

    displayMessage(s"Let's play!")
    mainLoop()

    def mainLoop(): Unit = {
        val mode = askUserChoseMode()
        mode match {
            case 1 =>  { //humanVsHuman()
                displayMessage(s"Human vs Human")
                val player1 = createPlayer()
                val player2 = createPlayer()
                displayMessage(s"$player1.name plays against $player2.name")
            }
            case 2 => {
                displayMessage(s"Human vs AI (Begginer)")
            }
            case 3 => {
                displayMessage(s"Human vs AI (Intermediate)")
            }
            case 4 => {
                displayMessage(s"Human vs AI (Difficult)")
            }
            case _ => {
                displayError("This is not a valid option!")
                mainLoop()
            }
        }
    }

    def createPlayer(): Player = {
        val name = askUserForName()
        val emptyShipsGrid = Board(List.fill(10)(List.fill(10)(WATER)))
        val emptyHitsGrid = Board(List.fill(10)(List.fill(10)(WATER)))
        Player(name, emptyShipsGrid, emptyHitsGrid, 0)
    }

    // def humanVsHuman(player1: Player, Player2: Player): Unit = {}

    // def userPlaceShips(shipsToPlace: Map[String, Int], player: Player): Player = {
    //     askUserToPlaceShips(player.name)
    //     def plac
    // }
}