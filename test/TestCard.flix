namespace TestCard {
    use Card.Card;
    use Card.Denomination;
    use Card.Denomination.{Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King};
    use Card.Suit;
    use Card.Suit.{Diamond, Club, Heart, Spade};

    namespace AllSuits {
        use Card.allSuits;

        @test
        def test01(): Bool = Set.size(allSuits()) == 4
    }

    namespace AllDenominations {
        use Card.allDenominations;

        @test
        def test01(): Bool = Set.size(allDenominations()) == 13
    }

    namespace AceHighCompare {
        use Card.aceHighCompare;

        @test
        def test01(): Bool = aceHighCompare(Ace, Two) == GreaterThan

        @test
        def test02(): Bool = aceHighCompare(Ace, Five) == GreaterThan

        @test
        def test03(): Bool = aceHighCompare(King, Ace) == LessThan

        @test
        def test04(): Bool = aceHighCompare(Ace, Ace) == EqualTo
    }

    namespace AceLowCompare {
        use Card.aceLowCompare;

        @test
        def test01(): Bool = aceLowCompare(Ace, Two) == LessThan

        @test
        def test02(): Bool = aceLowCompare(Ace, Five) == LessThan

        @test
        def test03(): Bool = aceLowCompare(King, Ace) == GreaterThan

        @test
        def test04(): Bool = aceLowCompare(Ace, Ace) == EqualTo

    }

}
