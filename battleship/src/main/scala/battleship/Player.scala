package battleship

import Utils._
import CellType._


case class HumanPlayer(name: String, shipsBoard: Board, hitsBoard: Board, lifePoints: Int) {

    def isAlive(): Boolean = {
        if(lifePoints > 0) true
        else false
    }

    def fireAtCell(x: Int, y: Int, opponent: Player): List[Player] = {
        opponent.shipsBoard(x)(y) match {
            case SHIP => {
                val updatedOpponentBoard = opponent.shipsBoard.updateCellState(x, y, HIT)
                val opponentNewLifePoints = opponent.lifePoints - 1
                val updatedOpponent = opponent.copy(shipsBoard = updatedOpponentBoard, lifePoints = opponentNewLifePoints)
                val updatedSelfBoard = this.hitsBoard.updateCellState(x, y, HIT)
                val updatedSelf = copy(hitsBoard = updatedSelfBoard)
                return List(updatedSelf, updatedOpponent)
            }
            case _ => {
                val updatedOpponentBoard = opponent.shipsBoard.updateCellState(x, y, MISS)
                val updatedOpponent = opponent.copy(shipsBoard = updatedOpponentBoard)
                val updatedSelfBoard = this.hitsBoard.updateCellState(x, y, MISS)
                val updatedSelf = copy(hitsBoard = updatedSelfBoard)
                return List(updatedSelf, updatedOpponent)
            }
        }
    }


}

// case class AIPlayer extends Player {

// }