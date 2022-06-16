namespace IdiotSolitaireImp {
    use IdiotSolitaire.GameRunState;
    use GameRunState.GameRunState.{Playing, Won, Lost};

    type alias Card = {suit :: Int32, den :: Int32}

    def cardString(c: Card): String = "${denString(c.den)}${suitString(c.suit)}"

    def suitString(s: Int32): String = match s {
        case 1 => "h" |> Console.red
        case 2 => "d" |> Console.yellow
        case 3 => "s" |> Console.blackBright
        case 4 => "c" |> Console.green
        case _ => ?wrongSuitInt
    }

    def denString(d: Int32): String = match d {
        case 1  => "A"
        case x if 2 <= x and x <= 10 => ToString.toString(x)
        case 11 => "J"
        case 12 => "Q"
        case 13 => "K"
        case _  => ?wrongDenInt
    } |> String.padLeft(2, ' ')

    type alias Pile = MutDeque[Card, static]

    pub type alias Game = {deck :: Pile, piles :: Array[Pile]}

    pub def newGame(seed: Int32): Game & Impure = {
        let rand = Random.newWithSeed(seed |> Int32.toInt64);
        let allCards = Iterator.range(1, 5) |> Iterator.flatMap(suit -> {
                Iterator.range(1, 14) |> Iterator.mapL(den -> {suit = suit, den = den})
            }) |> 
            Iterator.toArray;
        shuffle(rand, allCards);
        def newMd() = MutDeque.new(static);
        let md = newMd();
        allCards |> Array.foreach(c -> MutDeque.pushFront(c, md));
        {deck = md, piles = [newMd(), newMd(), newMd(), newMd()]}
    }

    def shuffle(r: Random.Random, a: Array[a]): Unit & Impure =
        let len = Array.length(a);
        def loop(i) = {
            if (i > 0) {
                let j = Random.nextNatWithMax(r, i + 1);
                let ai = a[i];
                let aj = a[j];
                a[i] = aj;
                a[j] = ai;
                loop(i - 1)
            } else ()
        };
        loop(len - 1)

    pub def getRunState(g: Game): GameRunState.GameRunState & Impure = {
        if (MutDeque.isEmpty(g.deck)) {
            let won = g.piles |> Array.map(p -> {
                match MutDeque.popFront(p) {
                    case None => ?emptyPileEnding
                    case Some(top) => {
                        MutDeque.pushFront(top, p);
                        MutDeque.size(p) == 1 and top.den == 13
                    }
                }
            }) |> Array.forall(identity);
            if (won) Won else Playing // TODO: Allow final moves
        } else Playing
    }

    pub def eliminatePile(i: Int32, g: Game): Unit & Impure = {
        let index = i-1;
        let targetPile = (g.piles)[index];
        if (MutDeque.isEmpty(targetPile)) () else
        let target = match MutDeque.peekFront(targetPile) {
            case None => ?unreachable
            case Some(t) => t
        };
        let valid = Iterator.range(0, 4) |> Iterator.map(p -> {
            if (p == index) false else
            match MutDeque.peekFront((g.piles)[p]) {
                case None => false
                case Some(front) =>
                    front.suit == target.suit and front.den > target.den
            }
        }) |> List.exists(identity);
        if (valid) {MutDeque.popFront(targetPile); ()} else ()
    }

    pub def drawCards(g: Game): Unit & Impure = {
        use Option.flatMap;
        {
            let* c1 = MutDeque.popFront(g.deck);
            let* c2 = MutDeque.popFront(g.deck);
            let* c3 = MutDeque.popFront(g.deck);
            let* c4 = MutDeque.popFront(g.deck);
            (g.piles)[0] |> MutDeque.pushFront(c1);
            (g.piles)[1] |> MutDeque.pushFront(c2);
            (g.piles)[2] |> MutDeque.pushFront(c3);
            (g.piles)[3] |> MutDeque.pushFront(c4);
            None
        };
        ()
    }

    pub def moveCard(origin: {origin::Int32}, target: Int32, g: Game): Unit & Impure = {
        if (not MutDeque.isEmpty((g.piles)[target-1])) () else
        match MutDeque.popFront((g.piles)[origin.origin-1]) {
            case None => ()
            case Some(c) => MutDeque.pushFront(c, (g.piles)[target-1])
        }
    }

    pub def drawPileCount(g: Game): Int32 & Impure =
        g.deck |> MutDeque.size

    pub def pileString(i: Int32, g: Game): Array[String] & Impure = {
        if (1 <= i and i <= 4)
            ((g.piles)[i-1]) |>
                MutDeque.toList |>
                List.reverse |>
                List.toArray |>
                Array.map(cardString)
        else []
    }

}
