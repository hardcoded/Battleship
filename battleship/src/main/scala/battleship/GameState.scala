package battleship

import Player._

<<<<<<< HEAD
case class GameState(active: Player, opponent: Player, firstPlayer: Player) {

=======
/**
  * The current state of the game:
  * - players grids
  * - players fleet
  * - players scores
  * @param active player whose turn it is to play
  * @param opponent opponent on which to fire
  * @param firstPlayer the first player of the game, used to switch first player on next game
  */
case class GameState(active: Player, opponent: Player, firstPlayer: Player) {

    /**
      * Interchange the active and opponent players for the next turn
      * @return the new game state
      */
>>>>>>> develop
    def switchPlayers: GameState = {
        val switchedActive = opponent
        val switchedOpponent = active
        copy(active = switchedActive, opponent = switchedOpponent)
    }
    
}