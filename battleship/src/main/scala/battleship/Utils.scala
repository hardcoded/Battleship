package battleship

import scala.io.StdIn.readLine
import scala.io.StdIn.readInt
import Console.{BLUE_B, WHITE_B, RED, RED_B, WHITE, BLACK_B, RESET, println}
import scala.annotation.tailrec

import CellType._

object Utils {

    def initializeConsole(): Unit = {
        clearConsole()
        val header = s"""
            ********************************
            **                            **
            **          BATTLESHIP        **
            **                            **
            ********************************
        """
        println(Console.WHITE + header)
    }

    def clearConsole(): Unit = print("\033c")

    def getUserInputInt(): Int = readInt()
    def getUserInputString(): String = readLine().trim()
    def askUserToContinue(): String = {
        displayMessage("Press Enter to continue")
        try {
            readLine()
        }
        catch {
            case e: Exception => {
                displayError("An error has occured!")
                askUserToContinue()
            }
        }
    }

    def displayMessage(m: String): Unit = println(s"$WHITE $m")

    def displayError(m: String): Unit = println(s"$RED $m $WHITE")

    def askUserForName(number: Int): String = {
        try {
            displayMessage(s"Player $number enter a user name")
            val userInput = getUserInputString()
            return userInput
        }
        catch {
            case e: Exception => {
                displayError("An error has occured!")
                askUserForName(number)
            }
        }
    }

    def askUserToPlaceShips(userName: String): Unit = displayMessage(s"$userName place your ships")
    def askUserToPlaceShip(shipName: String, shipSize: Int): Unit = displayMessage(s"Place your $shipName of size $shipSize")

    def askUserForPosition(axis: String): Int = {
        try {
            displayMessage(s"Chose $axis value (between 0 and 9)")
            val userInput = getUserInputInt()
            println(s"user input : $userInput")
            return userInput
        }
        catch {
            case e: NumberFormatException => {
                displayError("Please enter a number!")
                askUserForPosition(axis)
            }
        }
    }

    def askUserForShipDirection(): Int = {
        val string = s"""
            Chose ship direction (enter corresponding number, HORIZONTAL by default) :
                1 - VERTICAL
                2 - HORIZONTAL
        """
        try {
            displayMessage(string)
            val userInput = getUserInputInt()
            return userInput
        }
        catch {
            case e: NumberFormatException => {
                displayError("Please enter a number!")
                askUserForShipDirection()
            }
        }
    }

    def askUserChoseMode(): Int = {
        val string = s"""
            Please chose your mode (enter corresponding number) :
                1 - Human vs Human
                2 - Human vs AI (Beginner)
                3 - Human vs AI (Intermediate)
                4 - Human vs AI (Difficult)
        """
        try {
            displayMessage(string)
            val userInput = getUserInputInt()
            return userInput
        }
        catch {
            case e: NumberFormatException => {
                displayError("Please enter a number!")
                askUserChoseMode()
            }
        }
    }

    def askUserForNewGame(): Int = {
        val string = s"""
            \nDo you want to play again ? (enter corresponding number)
                1 - Yes
                2 - No
        """
        try {
            displayMessage(string)
            val userInput = getUserInputInt()
            return userInput
        }
        catch {
            case e: NumberFormatException => {
                displayError("Please enter a number!")
                askUserForNewGame()
            }
        }
    }

    def displayBoard(board: Board): Unit = {
        println(s"$WHITE   0   1   2   3   4   5   6   7   8   9")
        @tailrec
        def displayGrid(grid: List[List[CellType]], lineNumber: Int): Unit = {
            if(grid.isEmpty) println()
            else {
                val line = grid.head
                print(s"$WHITE$lineNumber")
                line.foreach(cell => cellToString(cell))
                print(s"$WHITE|")
                println()
                displayGrid(grid.tail, lineNumber + 1)
            }
        }
        displayGrid(board.grid, 0)
    }

    def cellToString(cellType: CellType): Unit = {
        cellType match {
            case WATER => print(s"$WHITE|$BLUE_B   $RESET")
            case SHIP => print(s"$WHITE|$BLACK_B   $RESET")
            case MISS => print(s"$WHITE|$WHITE_B   $RESET")
            case HIT => print(s"$WHITE|$RED_B   $RESET")
        }
    }

}