namespace TestDeckMonad {
    use Card.Card;
    use Card.Card.Card;
    use Card.Denomination;
    use Card.Denomination.{Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King};
    use Card.Suit;
    use Card.Suit.{Diamond, Club, Heart, Spade};

    @test
    def test01(): Bool = {
        use EMonad.flatMap;
        use EApplicative.point;
        use DeckMonad.{addCard, drawCard};
        let deck = Card(Heart  , Ace  ) ::
                   Card(Diamond, Two  ) ::
                   Card(Spade  , King ) ::
                   Card(Club   , Three) :: Nil;
        let test = {
            let* c1 = drawCard();
            let* c2 = drawCard();
            let* _  = addCard(Card(Diamond, Seven));
            let* _  = drawCard();
            let* c3 = drawCard();
            let* c4 = drawCard();
            (c4 :: c3 :: c2 :: c1 :: Nil) |> point
        };
        DeckMonad.run(deck, test) == Some(deck |> List.reverse) and
        DeckMonad.run(List.take(3, deck), test) == None
    }

}
