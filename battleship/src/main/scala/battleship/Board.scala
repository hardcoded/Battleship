package battleship

import scala.annotation.tailrec
import scala.collection.immutable.List
import CellType._


/**
  * The board of the game: 10x10 cells
  * @param grid the grid composing the Board: cells have a type to know its state
  */
case class Board(grid: List[List[CellType]]) {

    /**
      * Recursively udate the cells occupied by the ship
      * @param positions the cells left to update
      * @param updatedGrid the new grid with updated cells
      * @return the grid with updated cells for the ship
      */
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

    /**
      * Check if the cell is inside the boundaries of the board
      * @param x the x coordinate of the cell
      * @param y the y coordinate of the cell
      * @return true if the cell is inside the board, false otherwise
      */
    def isPositionValid(x: Int, y: Int): Boolean = {
        if((x < 0 || x > 9) || (y < 0 || y > 9)) false
        else true
    }

    /**
      * Check if the cell has already been shot
      * @param x the x coordinate of the cell
      * @param y the y coordinate of the cell
      * @return true if the cell's state is MISS or HIT, false otherwise
      */
    def positionIsShot(x: Int, y: Int): Boolean = this.getCellState(x, y) match {
        case HIT => true
        case MISS => true
        case _ => false
    }

    /**
      * Check if a ship can be placed on this position:
      * - the position is valid
      * - there is no other ship on this position
      * @param x the x coordinate of the cell
      * @param y the y coordinate of the cell
      * @return true if a ship can be placed, false otherwise
      */
    def canPlaceShipOnPosition(x: Int, y: Int): Boolean = {
        if(isPositionValid(x,y)) this.getCellState(x, y) match {
            case WATER => true
            case _ => false
        }
        else false
    }

    /**
      * Recursively check each potential positions of the ship to place
      * @param positions the positions the ship will occupy
      * @return false if one of the position is not OK (outside the board or a ship is there), true otherwise
      */
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

    /**
      * Get the state of the cell with the given coordinates
      * @param x the x coordinate of the cell
      * @param y the y coordinate of the cell
      * @return the state of the cell (WATER, SHIP, MISS, HIT)
      */
    def getCellState(x: Int, y: Int): CellType = this.grid(x)(y)

    /**
      * Update the state of the cell of given coordinates
      * @param x the x coordinate of the cell
      * @param y the y coordinate of the cell
      * @param gridToUpdate the grid the cell is on
      * @param newState the new state of the cell (WATER, SHIP, MISS, HIT)
      * @return the updated grid
      */
    def updateCellState(x: Int, y: Int, gridToUpdate: List[List[CellType]], newState: CellType.Value): List[List[CellType]] = {
        val matrix = gridToUpdate
        val updatedLine = matrix(x).updated(y, newState)
        val newGrid = matrix.updated(x, updatedLine)
        return newGrid
    }

}