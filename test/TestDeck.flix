namespace TestDeck {
    
    namespace FreshDeck {
        use Deck.freshDeck;

        @test
        def test01(): Bool =
            List.length(freshDeck()) == 13*4
    }
}
