module Othello_Driver where
import Othello_Player as Black_Player
import Othello_Player as White_Player

playNMoves :: Integer -> State
playNMoves =
  playNext Pass Black_Player.nextState White_Player.nextState
           (Black_Player.startState Black) (White_Player.startState White)
  where
    playNext _ _ _ _ opponentState 0 = opponentState
    playNext lastMove playerNext opponentNext playerState opponentState n =
    	playNext nextMove opponentNext playerNext opponentState nextPlayerState (n - 1)
      where (nextMove, nextPlayerState) = playerNext playerState lastMove