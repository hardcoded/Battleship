package battleship

import scala.annotation.tailrec
import scala.util.Random

import CellType._
import Utils._

case class Player(name: String, isHuman: Boolean, shipsBoard: Board, hitsBoard: Board, fleet: List[Ship] = List(), positionsHit: List[(Int, Int, Boolean)] = List(), score: Int = 0) {

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
    def chooseTarget(aiLevel: String, randomX: Random, randomY: Random): (Int, Int) = aiLevel match {
        case "AI-easy" => {
            val randX = randomX.nextInt(10)
            val randY = randomY.nextInt(10)
            (randX, randY)
        }
        case "AI-medium" => {
            val randX = randomX.nextInt(10)
            val randY = randomY.nextInt(10)
            if(!this.hitsBoard.positionIsShot(randX, randY)) (randX, randY)
            else chooseTarget(aiLevel, randomX, randomY)
        }
        case "AI-hard" => {
            // TODO: try to follow ship by looking direction of hits
            if(this.positionsHit.isEmpty) chooseTarget("AI-medium", randomX, randomY)
            else {
                val lastPositionHit = this.positionsHit.head
                if(lastPositionHit._3) chooseTarget("AI-medium", randomX, randomY)
                else {
                    val adjascentPositions = this.getAdjascentPositions(lastPositionHit._1, lastPositionHit._2)
                    val secondLastHit = if(this.positionsHit.size > 1) this.positionsHit(1) else lastPositionHit
                    getNextHit(adjascentPositions, lastPositionHit, secondLastHit).getOrElse(chooseTarget("AI-medium", randomX, randomY))
                }
            }
        }
        case _ => (0,0)
    }

    def getNextHit(shootPossibilities: List[(Int, Int)], lastHit: (Int, Int, Boolean), secondLastHit: (Int, Int, Boolean)): Option[(Int, Int)] = {
        if(shootPossibilities.isEmpty) None // if all adjascent positions are shot get random position
        else {
            if(shootPossibilities.contains(secondLastHit)) {
                val diff = (lastHit._1 - secondLastHit._1, lastHit._1 - secondLastHit._1)
                val newHit = (lastHit._1 + diff._1, lastHit._2 + diff._2)
                if(!this.hitsBoard.positionIsShot(newHit._1, newHit._2)) Some(newHit)
                else {
                    val adjPos = checkAdjPos((lastHit._1, lastHit._2), diff)
                    if(adjPos.isEmpty) None
                    else Some(adjPos.get)
                }
            }
            else {
                val nextHit = shootPossibilities.head
                if(this.hitsBoard.isPositionValid(nextHit._1, nextHit._2) && !this.hitsBoard.positionIsShot(nextHit._1, nextHit._2)) Some(nextHit)
                else getNextHit(shootPossibilities.tail, lastHit, secondLastHit)
            }
        }
    }

    def checkAdjPos(lastPosChecked: (Int, Int), diff: (Int, Int)): Option[(Int, Int)] = {
        val newPos = (lastPosChecked._1 - diff._1, lastPosChecked._2 - diff._2)
        this.hitsBoard.getCellState(newPos._1, newPos._2) match {
            case HIT => checkAdjPos(newPos, diff)
            case MISS => None
            case _ => Some(newPos)
        }
    }

    def getAdjascentPositions(x: Int, y: Int): List[(Int, Int)] = {
        (x, y-1) :: (x-1, y) :: (x, y+1) :: (x+1, y) :: Nil
    }

    def addHitPosition(x: Int, y: Int, isSunk: Boolean = false): List[(Int, Int, Boolean)] = {
        (x, y, isSunk) :: this.positionsHit
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
                if(this.isHuman) displayMessage(s"${Console.RED} HIT! ${Console.RESET} ")

                val shipToUpdate = getShipOnCell(x, y, opponent.fleet)
                val updatedShip = shipToUpdate.addShoot(x, y)

                if(updatedShip.isSunk && this.isHuman) displayMessage(s"${Console.RED}  Ship sunk! ${Console.RESET} ")

                val updatedFleet = opponent.fleet.updated(opponent.fleet.indexOf(shipToUpdate), updatedShip)

                val updatedOpponentGrid = opponent.shipsBoard.updateCellState(x, y, opponent.shipsBoard.grid, HIT)
                val updatedOpponentBoard = opponent.shipsBoard.copy(grid = updatedOpponentGrid)
                val updatedOpponent = opponent.copy(shipsBoard = updatedOpponentBoard, fleet = updatedFleet)
                
                val updatedSelfGrid = this.hitsBoard.updateCellState(x, y, this.hitsBoard.grid, HIT)
                val updatedSelfBoard = this.hitsBoard.copy(grid = updatedSelfGrid)

                val newPosHit = if(updatedShip.isSunk) addHitPosition(x, y, true) else addHitPosition(x, y)
                val updatedSelf = copy(hitsBoard = updatedSelfBoard, positionsHit = newPosHit)
                
                return (updatedSelf, updatedOpponent)
            }
            case HIT => {
                if(this.isHuman) displayMessage(s"${Console.RED} HIT!")
                return (this, opponent)
            }
            case _ => {
                if(this.isHuman) displayMessage("MISS!")

                val updatedSelfGrid = this.hitsBoard.updateCellState(x, y, this.hitsBoard.grid, MISS)
                val updatedSelfBoard = this.hitsBoard.copy(grid = updatedSelfGrid)
                val updatedSelf = copy(hitsBoard = updatedSelfBoard)

                val updatedOpponentGrid = opponent.shipsBoard.updateCellState(x, y, opponent.shipsBoard.grid, MISS)
                val updatedOpponentBoard = opponent.shipsBoard.copy(grid = updatedOpponentGrid)
                val updatedOpponent = opponent.copy(shipsBoard = updatedOpponentBoard)
                
                return (updatedSelf, updatedOpponent)
            }
        }
    }
}