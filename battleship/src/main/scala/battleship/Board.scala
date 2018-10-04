package battleship

import scala.annotation.tailrec
import scala.collection.immutable.List
import CellType._

case class Board(grid: List[List[CellType]]) {

    // Getter
    def grid: List[List[CellType]] = grid

    def placeShip(ship: Ship, grid: List[List[CellType]]): Board = {
        @tailrec
        def updatePositionsState(positions: List[(Int, Int)], updatedGrid: List[List[CellType]]): Board = {
            if(positions.isEmpty) copy(grid = updatedGrid)
            else {
                val pos = positions.head
                val newBoard = updateCellState(pos._1, pos._2, SHIP)
                updatePositionsState(positions.tail, newBoard.grid)
            }
        }
        updatePositionsState(ship.positions, grid)
    }

    def isPositionValid(x: Int, y: Int): Boolean = {
        if(x > 10 || y > 10) false
        else true
    }

    def canPlaceShipOnPosition(x: Int, y: Int): Boolean = {
        grid(x)(y) match {
            case WATER => {
                if(isPositionValid(x,y)) true
                else false
            }
            case _ => false
        }
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

    def updateCellState(x: Int, y: Int, newState: CellType.Value): Board = {
        val matrix = grid
        val updatedLine = matrix(x).updated(y, newState)
        val newGrid = matrix.updated(x, updatedLine)
        val updatedBoard: Board = copy(grid = newGrid)
        return updatedBoard
    }

}