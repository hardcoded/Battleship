package battleship

import Utils._
import CellType._

import scala.util.Random

case class Player(name: String, isHuman: Boolean, shipsBoard: Board, hitsBoard: Board, fleet: List[Ship] = List(), score: Int = 0) {

    /**
      * Check if the player still has unsunk ships
      * @return true if the player has unskunk ships, false if not
      */
    def isAlive: Boolean = {
        val sunkShips = this.fleet.filter(ship => ship.isSunk)
        if(sunkShips.length == this.fleet.length) false
        else true
    }

    /**
      * Get the ship place on this position
      * @param x x coordinate of the position
      * @param y y coordinate of the position
      * @param fleet the fleet in which to find the ship
      * @return the ship object on the cell
      */
    def getShipOnCell(x: Int, y: Int, fleet: List[Ship]): Ship = {
        val pos = (x,y)
        val shipOnCell = fleet.find(ship => ship.positions.contains(pos))
        return shipOnCell.get
    }

    /**
      * AI choose target position to shoot
      *  - easy AI : choose random target (even if already shot)
      *  - medium AI : choose random target (but never same target twice)
      *  - hard AI : choose random target (even if already shot)
      * @param aiLevel target chosen from random to accurate
      * @return tuple of int for the position
      */
    def chooseTarget(aiLevel: String): (Int, Int) = aiLevel match {
        case "AI-easy" => {
            val randX = Random.nextInt(10)
            val randY = Random.nextInt(10)
            (randX, randY)
        }
        case "AI-medium" => {
            val randX = Random.nextInt(10)
            val randY = Random.nextInt(10)
            if(!this.hitsBoard.positionIsShot(randX, randY)) (randX, randY)
            else chooseTarget("AI-medium")
        }
        case "AI-hard" => (0,0)
        case _ => (0,0)
    }

    /**
      * Shoot on cell with given coordinates
      * @param x x coordinate of the position to shoot
      * @param y y coordinate of the position to shoot
      * @param opponent the player on which to shoot
      * @return a tuple with the players updated
      */
    def fireAtCell(x: Int, y: Int, opponent: Player): (Player, Player) = {
        opponent.shipsBoard.grid(x)(y) match {
            case SHIP => {
                displayMessage(s"${Console.RED} HIT! ${Console.RESET} ")

                val shipToUpdate = getShipOnCell(x, y, opponent.fleet)
                val updatedShip = shipToUpdate.isShot(x, y)

                if(updatedShip.isSunk) displayMessage(s"${Console.RED}  Ship sunk! ${Console.RESET} ")

                val updatedFleet = opponent.fleet.updated(opponent.fleet.indexOf(shipToUpdate), updatedShip)

                val updatedOpponentGrid = opponent.shipsBoard.updateCellState(x, y, opponent.shipsBoard.grid, HIT)
                val updatedOpponentBoard = opponent.shipsBoard.copy(grid = updatedOpponentGrid)

                val updatedOpponent = opponent.copy(shipsBoard = updatedOpponentBoard, fleet = updatedFleet)
                
                val updatedSelfGrid = this.hitsBoard.updateCellState(x, y, this.hitsBoard.grid, HIT)
                val updatedSelfBoard = this.hitsBoard.copy(grid = updatedSelfGrid)
                val updatedSelf = copy(hitsBoard = updatedSelfBoard)
                
                return (updatedSelf, updatedOpponent)
            }
            case HIT => {
                displayMessage(s"${Console.RED} HIT!")
                return (this, opponent)
            }
            case _ => {
                displayMessage("MISS!")
                val updatedSelfGrid = this.hitsBoard.updateCellState(x, y, this.hitsBoard.grid, MISS)
                val updatedSelfBoard = this.hitsBoard.copy(grid = updatedSelfGrid)
                val updatedSelf = copy(hitsBoard = updatedSelfBoard)
                
                return (updatedSelf, opponent)
            }
        }
    }
}