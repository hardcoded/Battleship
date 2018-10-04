package battleship

import scala.io.StdIn.readLine
import scala.io.StdIn.readInt
import Console.println

object Utils {

    def initializeConsole(): Unit = {
        val header = s"""
            ********************************
            **                            **
            **          BATTLESHIP        **
            **                            **
            ********************************
        """
        println(Console.WHITE + header)
    }

    def getUserInputInt(): Int = readInt()
    def getUserInputString(): Int = readLine().trim()

    def displayMessage(m: String): Unit = println(m)

    def displayError(m: String): Unit = println(Console.RED + m + Console.WHITE)

    def askUserForName(): String = {
        try {
            displayMessage(s"Please enter a user name")
            val userInput = getUserInputString()
            return userInput
        }
        catch {
            case e: Throwable => {
                displayError("An error has occured!")
                askUserForName()
            }
        }
    }
    
    def askUserForPosition(axis: String): Int = {
        try {
            displayMessage(s"Please chose $axis value")
            val userInput = getUserInputInt()
            return userInput
        }
        catch {
            case e: Throwable => {
                displayError("An error has occured!")
                askUserForPosition()
            }
        }
    }

    def askUserToPlaceShips(userName: String): Unit = displayMessage(s"$userName place your ships")

    def askUserToPlaceShip(shipName: String, shipSize: Int): Unit = displayMessage(s"Please place your $shipName of size $shipSize")

    def askUserChoseMode(): Int = {
        val string = s"""
            \nPlease chose your mode (enter corresponding number) :
                1 - Human vs Human
                2 - Human vs AI (Begginer)
                3 - Human vs AI (Intermediate)
                4 - Human vs AI (Difficult)
        """
        try {
            displayMessage(string)
            val userInput = getUserInputInt()
            return userInput
        }
        catch {
            case e: Throwable => {
                displayError("An error has occured!")
                askUserChoseMode()
            }
        }
    }

    // def boardtoString

}