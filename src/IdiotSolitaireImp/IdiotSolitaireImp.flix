namespace IdiotSolitaireImp {
    use IdiotSolitaire.GameRunState;
    use IdiotSolitaire.GameRunState.{Playing, Won, Lost};

    type alias Card = {suit :: Int32, den :: Int32}

    type alias Pile = MutDeque[Card, static]

    pub type alias Game = {deck :: Pile, piles :: Array[Pile]}

    pub def newGame(seed: Int32): Game & Impure = ??? as & Impure

    pub def getRunState(g: Game): GameRunState.GameRunState & Impure = ??? as & Impure

    pub def eliminatePile(i: Int32, g: game): Unit & Impure = ??? as & Impure

    pub def drawCards(g: Game): Unit & Impure = ??? as & Impure

    pub def moveCard(origin: {origin::Int32}, target: Int32, g: Game): Unit & Impure = ??? as & Impure

    pub def drawPileCount(g: Game): Int32 & Impure =
        g.deck |> MutDeque.size

    pub def pileString(i: Int32, g: Game): Array[String] & Impure = ??? as & Impure

}
