package battleship

import scala.annotation.tailrec

case class Ship(name: String, size: Int, direction: String, positions: List[(Int,Int)] = List(), positionsShot: Set[(Int,Int)] = Set()) {

    def isShot(x: Int, y: Int): Ship = {
        val pos = (x,y)
        val newPositionShot = this.positionsShot + pos
        copy(positionsShot = newPositionShot)
    }

    def isSunk: Boolean = {
        if(this.positions.length == this.positionsShot.size) true
        else false
    }

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

}
