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

    type alias Pile = MutDeque[Card, Static]

    pub type alias Game = {deck :: Pile, piles :: Array[Pile, Static]}

    pub def newGame(seed: Int32): Game & Impure = {
        let rand = Random.newWithSeed(seed |> Int32.toInt64);
        let allCards = Iterator.range(Static, 1, 5) |> Iterator.flatMap(suit -> {
                Iterator.range(Static, 1, 14) |> Iterator.mapL(den -> {suit = suit, den = den})
            }) |> 
            Iterator.toArray(Static);
        shuffle(rand, allCards);
        def newMd() = MutDeque.new(Static);
        let md = newMd();
        allCards |> Array.foreach(c -> MutDeque.pushFront(c, md));
        {deck = md, piles = [newMd(), newMd(), newMd(), newMd()]}
    }

    def shuffle(r: Random.Random, a: Array[a, Static]): Unit & Impure =
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
                    case None => false
                    case Some(top) => {
                        MutDeque.pushFront(top, p);
                        MutDeque.size(p) == 1 and top.den == 13
                    }
                }
            }) |> Array.forall(identity);
            if (won) Won else Playing // TODO: Allow final moves
        } else Playing
    }

    ///
    /// Returns `true` if successful.
    ///
    pub def eliminatePile(i: Int32, g: Game): Bool & Impure = {
        let index = i-1;
        let targetPile = (g.piles)[index];
        if (MutDeque.isEmpty(targetPile)) false else
        let target = match MutDeque.peekFront(targetPile) {
            case None => ?unreachable
            case Some(t) => t
        };
        let valid = Iterator.range(Static, 0, 4) |> Iterator.map(p -> {
            if (p == index) false else
            match MutDeque.peekFront((g.piles)[p]) {
                case None => false
                case Some(front) =>
                    front.suit == target.suit and front.den > target.den
            }
        }) |> List.exists(identity);
        if (valid) {MutDeque.popFront(targetPile); true} else false
    }

    pub def drawCards(g: Game): Bool & Impure = {
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
            Some(true)
        } |> Option.getWithDefault(false)
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

    pub def pileString(i: Int32, g: Game): Array[String, Static] & Impure = {
        if (1 <= i and i <= 4)
            ((g.piles)[i-1]) |>
                MutDeque.toList |>
                List.reverse |>
                (l -> List.toArray(l, Static)) |>
                Array.map(cardString)
        else []
    }

    def pause(millis: Int64): Unit & Impure = {
        import static java.lang.Thread.sleep(Int64): Unit & Impure;
        if (millis <= 0i64) () else sleep(millis)
    }

    pub def autoSolve(speed: Int64, render: Unit -> Unit & ef, g: Game): Unit & Impure = {
        def renderAndPause(b) = if (b) {render(); pause(speed)} else ();
        let change = ref false;
        def elimLoop() = {
            change := false;
            Iterator.range(Static, 1, 5) |> Iterator.foreach(p -> {
                let b = eliminatePile(p, g);
                renderAndPause(b);
                change := deref change or b
            });
            if (deref change) elimLoop() else ()
        };
        elimLoop();
        match findMoves(g) {
            case Err() => ()
            case Ok(moves) =>
                // make moves
                if (List.length(moves) > 0) {
                    moves |> List.foreach(match (origin, target) -> {
                        moveCard(origin = origin, target, g);
                        renderAndPause(true)
                    });
                    autoSolve(speed, render, g)
                } else {
                    // draw cards
                    let cardsDrawn = drawCards(g);
                    if (cardsDrawn) {
                        renderAndPause(true);
                        autoSolve(speed, render, g)
                    } else ()
                }
        }
    }

    /// Returns None if a non-trivial move is needed, otherwise a sequence of moves.
    def findMoves(g: Game): Result[List[(Int32, Int32)], Unit] & Impure = {
        use Array.{length, map, findIndices, sum, toList};
        use List.{zip, flatMap, repeat, range};

        let sizes = g.piles |> map(MutDeque.size);
        let emptyPiles = sizes |> findIndices(Eq.eq(0)) |> map(i -> i+1);
        if (length(emptyPiles) == 0) Ok(Nil) else

        let pilesToMoveFrom = sizes |> findIndices(size -> size >= 2) |> map(i -> i+1);
        let movableCards = sizes |> map(size -> Int32.max(0, size-1));
        let numberOfMovableCards =  movableCards |> sum;
        if (numberOfMovableCards == 0) Ok(Nil) else
        if (numberOfMovableCards <= length(emptyPiles)) {
            // find the piles with
            let origins = range(0, 4) |> flatMap(i -> repeat(movableCards[i], i+1));
            let targets = emptyPiles |> toList;
            // targets is the longest list so zip removing elements is fine
            zip(origins, targets) |> Ok
        } else if (length(pilesToMoveFrom) == 1) {
            let cardsToMove = Int32.min(numberOfMovableCards, length(emptyPiles));
            let origins = repeat(cardsToMove, pilesToMoveFrom[0]);
            let targets = emptyPiles |> toList;
            zip(origins, targets) |> Ok
        } else Err()
    }

}
