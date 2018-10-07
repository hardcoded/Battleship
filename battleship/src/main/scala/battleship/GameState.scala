package battleship

import Player._

case class GameState(active: Player, opponent: Player, firstPlayer: Player) {

    def switchPlayers: GameState = {
        val switchedActive = opponent
        val switchedOpponent = active
        copy(active = switchedActive, opponent = switchedOpponent)
    }
    
}