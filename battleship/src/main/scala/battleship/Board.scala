package battleship

import scala.annotation.tailrec
import scala.collection.immutable.List
import CellType._

case class Board(grid: List[List[CellType]]) {

    def placeShip(ship: Ship): Board = {
        @tailrec
        def updatePositionsState(positions: List[(Int, Int)], updatedGrid: List[List[CellType]]): Board = {
            if(positions.isEmpty) copy(grid = updatedGrid)
            else {
                val pos = positions.head
                val newGrid = updateCellState(pos._1, pos._2, updatedGrid, SHIP)
                updatePositionsState(positions.tail, newGrid)
            }
        }
        updatePositionsState(ship.positions, this.grid)
    }

    def isPositionValid(x: Int, y: Int): Boolean = {
        if((x < 0 || x > 9) || (y < 0 || y > 9)) false
        else true
    }

    def positionIsShot(x: Int, y: Int): Boolean = this.getCellState(x, y) match {
        case HIT => true
        case MISS => true
        case _ => false
    }

    def canPlaceShipOnPosition(x: Int, y: Int): Boolean = {
        if(isPositionValid(x,y)) grid(x)(y) match {
            case WATER => true
            case _ => false
        }
        else false
    }

    def canPlaceShip(ship: Ship): Boolean = {
        @tailrec
        def checkAllPositions(positions: List[(Int, Int)]): Boolean = {
            if(positions.isEmpty) true
            else {
                val pos = positions.head
                if (!canPlaceShipOnPosition(pos._1, pos._2)) false
                else checkAllPositions(positions.tail)
            }
        }
        checkAllPositions(ship.positions)
    }

    def getCellState(x: Int, y: Int): CellType = this.grid(x)(y)

    def updateCellState(x: Int, y: Int, gridToUpdate: List[List[CellType]], newState: CellType.Value): List[List[CellType]] = {
        val matrix = gridToUpdate
        val updatedLine = matrix(x).updated(y, newState)
        val newGrid = matrix.updated(x, updatedLine)
        return newGrid
    }

}