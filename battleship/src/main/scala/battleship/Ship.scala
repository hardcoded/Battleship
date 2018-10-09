package battleship

import scala.annotation.tailrec

<<<<<<< HEAD
case class Ship(name: String, size: Int, direction: String, positions: List[(Int,Int)] = List(), positionsShot: Set[(Int,Int)] = Set()) {

=======
/**
  * Represents a ship
  * @param name the name of the ship
  * @param size the size (number of cells) of the ship
  * @param direction the direction of the ship (HORIZONTAL or VERTICAL)
  * @param positions the positions occupied by the ship
  * @param positionsShot the ship's positions that has been shot
  */
case class Ship(name: String, size: Int, direction: String, positions: List[(Int,Int)] = List(), positionsShot: Set[(Int,Int)] = Set()) {

    /**
      * Add a shoot to the list of ship's hit positions
      * @param x the x coordinate of the cell
      * @param y the y coordinate of the cell
      * @return the updated ship
      */
>>>>>>> develop
    def addShoot(x: Int, y: Int): Ship = {
        val pos = (x,y)
        val newPositionShot = this.positionsShot + pos
        copy(positionsShot = newPositionShot)
    }

<<<<<<< HEAD
=======
    /**
      * Check if the ship has been sunk, i.e: all the positions have been shot
      * @return true if the ship is sunk, false otherwise
      */
>>>>>>> develop
    def isSunk: Boolean = {
        if(this.positions.length == this.positionsShot.size) true
        else false
    }

<<<<<<< HEAD
=======
    /**
      * Create recursively the postions the ship will occupy
      * @param x the x coordinate where the ship starts
      * @param y the y coordinate where the ship starts
      * @param positions the positions generated
      * @return the ship updated
      */
>>>>>>> develop
    def createPositions(x: Int, y: Int, positions: List[(Int,Int)]): Ship = {
        @tailrec
        def createPosition(x: Int, y: Int, positions: List[(Int,Int)]): Ship = {
            if(positions.length == this.size) copy(positions = positions)
            else {
                this.direction match {
                    case "HORIZONTAL" => {
                        val newPos = (x, y)
                        val newPosList = newPos :: positions
                        val nextY = y + 1
                        createPosition(x, nextY, newPosList)
                    }
                    case "VERTICAL" => {
                        val newPos = (x, y)
                        val newPosList = newPos :: positions
                        val nextX = x + 1
                        createPosition(nextX, y, newPosList)
                    }
                }
            }
        }
        createPosition(x, y, positions)
    }
<<<<<<< HEAD

=======
>>>>>>> develop
}
