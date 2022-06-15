
namespace IdiotSolitaire {

    pub enum GameRunState {
        case Playing, Won, Lost
    }

    pub class IdiotSolitaire[gameState] {

        pub def setupGame(seed: Int32): gameState & ef

        pub def getRunState(g: gameState): GameRunState & ef

        pub def eliminatePile(pile: Int32, g: gameState): gameState & ef

        pub def drawCards(g: gameState): gameState & ef

        pub def moveCard(origin: {origin :: Int32}, target: Int32, g: gameState): gameState & ef

        pub def drawPileCount(g: gameState): Int32 & ef

        pub def pileString(pile: Int32, g: gameState): List[String] & ef

    }

}