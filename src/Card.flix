namespace Card {

    pub enum Card(Denomination, Suit)

    pub enum Suit with ToString, Eq {
        case Diamond, Club, Heart, Spade 
    }

    pub enum Denomination with ToString, Eq {
        case Ace, Two, Three, Four, Five, Six, Seven,
             Eight, Nine, Ten, Jack, Queen, King
    }

    pub def aceHighComparison(d1: Denomination, d2: Denomination): Comparison = {
        def toInt(d) = match d {
            case Two   =>  2
            case Three =>  3
            case Four  =>  4
            case Five  =>  5
            case Six   =>  6
            case Seven =>  7
            case Eight =>  8
            case Nine  =>  9
            case Ten   => 10
            case Jack  => 11
            case Queen => 12
            case King  => 13
            case Ace   => 14
        };
        toInt(d1) <=> toInt(d2)
    }

    pub def aceLowComparison(d1: Denomination, d2: Denomination): Comparison = {
        def toInt(d) = match d {
            case Ace   =>  1
            case Two   =>  2
            case Three =>  3
            case Four  =>  4
            case Five  =>  5
            case Six   =>  6
            case Seven =>  7
            case Eight =>  8
            case Nine  =>  9
            case Ten   => 10
            case Jack  => 11
            case Queen => 12
            case King  => 13
        };
        toInt(d1) <=> toInt(d2)
    }
}
