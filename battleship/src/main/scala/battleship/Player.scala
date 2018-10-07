package battleship

import Utils._
import CellType._

case class Player(name: String, isHuman: Boolean, shipsBoard: Board, hitsBoard: Board, fleet: List[Ship] = List(), score: Int = 0) {

    def isAlive: Boolean = {
        val sunkShips = this.fleet.filter(ship => ship.isSunk)
        if(sunkShips.length == this.fleet.length) false
        else true
    }

    def getShipOnCell(x: Int, y: Int, fleet: List[Ship]): Ship = {
        val pos = (x,y)
        val shipOnCell = fleet.find(ship => ship.positions.contains(pos))
        return shipOnCell.get
    }

//    def choseTarget(aiLevel): (Int, Int) = aiLevel match {
//        case "easy" =>
//        case "medium" =>
//        case "hard" =>
//    }

    def fireAtCell(x: Int, y: Int, opponent: Player): List[Player] = {
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
                
                return List(updatedSelf, updatedOpponent)
            }
            case HIT => {
                displayMessage(s"${Console.RED} HIT!")
                return List(this, opponent)
            }
            case _ => {
                displayMessage("MISS!")
                val updatedSelfGrid = this.hitsBoard.updateCellState(x, y, this.hitsBoard.grid, MISS)
                val updatedSelfBoard = this.hitsBoard.copy(grid = updatedSelfGrid)
                val updatedSelf = copy(hitsBoard = updatedSelfBoard)
                
                return List(updatedSelf, opponent)
            }
        }
    }
}