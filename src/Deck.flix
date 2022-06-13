namespace Deck {
    use Card.allDenominations;
    use Card.allSuits;
    use Card.Card;
    use Card.Card.Card;
    use Card.Denomination;
    use Card.Suit;

    pub type alias Deck = List[Card]

    pub def freshDeck(): Deck = {
        use Set.flatMap;
        allSuits() |>
            Set.flatMap(fullSuit) |>
            Set.toList
    }

    def fullSuit(s: Suit): Set[Card] = {
        allDenominations() |>
            Set.map(den -> Card(s, den))

    }
}
