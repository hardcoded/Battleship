package battleship


import java.io.{BufferedWriter, FileWriter, File}

import scala.io.StdIn.readLine
import scala.io.StdIn.readInt
import Console.{BLACK_B, BLUE_B, RED, RED_B, RESET, WHITE, WHITE_B, println}
import scala.annotation.tailrec
import scala.util.Random

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

    def clearConsole(): Unit = print("\033[H\033[2J")

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

    def generateRandomPosition(randomX: Random, randomY: Random): (Int, Int) = {
        val xPos = randomX.nextInt(10)
        val yPos = randomY.nextInt(10)
        (xPos, yPos)
    }

    def generateRandomDirection(random: Random): String = {
        val dir = random.nextInt()
        if(dir % 2 == 0) "VERTICAL"
        else "HORIZONTAL"
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

    def askUserChooseMode(): Int = {
        val string = s"""Please chose your mode (enter corresponding number) :
                1 - Human vs Human
                2 - Human vs AI (EASY)
                3 - Human vs AI (MEDIUM)
                4 - Human vs AI (HARD) - not implemented yet
                5 - AI (EASY) vs AI (MEDIUM)
                6 - AI (MEDIUM) vs AI (HARD) - not implemented yet
                7 - AI (EASY) vs AI (HARD)  - not implemented yet
        """
        try {
            displayMessage(string)
            val userInput = getUserInputInt()
            return userInput
        }
        catch {
            case e: NumberFormatException => {
                displayError("Please enter a number!")
                askUserChooseMode()
            }
        }
    }

    def askUserForNewGame(): Int = {
        val string = s"\nDo you want to play again ? (enter corresponding number) \n    1 - Yes \n    2 - Yes but change mode \n    3 - No"
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

    def askNumberOfSimulations(): Int = {
        try {
            displayMessage("How many simulations do you want to run?")
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
        println(s"$WHITE                           y")
        println()
        println(s"$WHITE       0   1   2   3   4   5   6   7   8   9")
        @tailrec
        def displayGrid(grid: List[List[CellType]], lineNumber: Int): Unit = {
            if(grid.isEmpty) println()
            else {
                val line = grid.head
                if(lineNumber == (line.size/2)) print(s"$WHITE x  $lineNumber") else print(s"$WHITE    $lineNumber")
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

    // TODO: append result if file exist
    def writeToCSV(gameState: GameState): Unit = {
        val outputFile = new BufferedWriter(new FileWriter("ai-proof.csv", true))
        val results = s"\n${gameState.active.name}, ${gameState.active.score}, ${gameState.opponent.score}, ${gameState.opponent.name}"
        outputFile.write(results)
        outputFile.close()
    }
}