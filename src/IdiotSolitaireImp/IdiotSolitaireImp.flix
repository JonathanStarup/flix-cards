namespace IdiotSolitaireImp {
    use IdiotSolitaire.GameRunState;
    use IdiotSolitaire.GameRunState.{Playing, Won, Lost};

    type alias Card = {suit :: Int32, den :: Int32}

    type alias Pile = MutDeque[Card, static]

    type alias GameInternal = {deck :: Pile, piles :: Array[Pile]}

    pub opaque type Game = GameInternal

    /// def getRunState(g: GameInternal): GameRunState & Impure = {
    ///     if (not MutDeque.isEmpty(g.deck)) Playing else
    ///     let won = g.piles |> Array.forall(p -> {
    ///         if (MutDeque.size(p) == 1)
    ///             ???
    ///         else false
    ///     });
    ///     if (won) Won else Lost
    /// }

    def _drawPileCount(g: GameInternal): Int32 & Impure =
        g.deck |> MutDeque.size

    /// instance IdiotSolitaire.IdiotSolitaire[Game] {
    
    ///     pub def getRunState(g: Game): GameRunState = ???
    
    ///     pub def eliminatePile(pile: Int32, g: Game): Game = ???
    
    ///     pub def drawCards(g: Game): Game = ???
    
    ///     pub def setupGame(seed: Int32): Game = ???
    
    ///     pub def drawPileCount(g: Game): Int32 & Impure =
    ///         let Game(gg) = g;
    ///         drawPileCount(gg)
    
    ///     pub def pileString(pile: Int32, g: Game): List[String] = ???
    
    ///     pub def moveCard(origin: { origin :: Int32 }, target: Int32, g: Game): Game = ???
    
    /// }
    



}
