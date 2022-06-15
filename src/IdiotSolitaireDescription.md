# IdiotSolitaire

Idiot solitaire has four piles ...

Required behavior:

* `newGame`: given a randomness seed, create a new game.
* `getRunState`: answer if the game state is `Playing`, `Won`, or `Lost`.
* `eliminatePile <i>`: remove the card in pile `i` if allowed.
* `drawCards`: draw a new card to each of the piles if there is cards left.
* `moveCard <i> <j>`: moves the top card from pile i to the empty pile j if allowed.
* `drawPileCount`: The number of remaining cards in the deck.
* `pileString <i>`: return a list of strings representing pile `i`.
