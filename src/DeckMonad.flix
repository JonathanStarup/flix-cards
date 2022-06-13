namespace DeckMonad {
    use Deck.Deck;
    use Card.Card;
    
    pub enum DeckMonad[a: Type, ef: Bool] {
        case DeckMonad(Deck -> Option[(a, Deck)] & ef)
    }

    instance EFunctor[DeckMonad] {
        pub def map(f: a -> b & ef1, x: DeckMonad[a, ef2]): DeckMonad[b, ef1 and ef2] = {
            use Option.flatMap;
            DeckMonad(d1 -> (
                let DeckMonad(xm) = x;
                let* (xres, d2) = xm(d1);
                (f(xres), d2) |> Some
            ))
        }
    }

    instance EApplicative[DeckMonad] {
        pub def point(x: a): DeckMonad[a, true] =
            DeckMonad(d -> (x, d) |> Some)
    
        pub def ap
                (f: DeckMonad[a -> b & ef1, ef2], x: DeckMonad[a, ef3]):
                DeckMonad[b, ef1 and ef2 and ef3] =
        {
            use Option.flatMap;
            DeckMonad(d1 -> (
                let DeckMonad(fm) = f;
                let DeckMonad(xm) = x;
                let* (ff, d2) = fm(d1);
                let* (xx, d3) = xm(d2);
                (ff(xx), d3) |> Some
            ))
        }
    }
    
    instance EMonad[DeckMonad] {
        pub def flatMap
                (f: a -> DeckMonad[b, ef1] & ef2, x: DeckMonad[a, ef3]):
                DeckMonad[b, ef1 and ef2 and ef3] =
        {
            use Option.flatMap;
            DeckMonad(d1 -> (
                let DeckMonad(xm) = x;
                let* (xx, d2) = xm(d1);
                let DeckMonad(fx) = f(xx);
                fx(d2)
            ))
        }
    }

    pub def addCard(c: Card): DeckMonad[Unit, Pure] =
        DeckMonad(deck -> ((), c :: deck) |> Some)

    pub def drawCard(): DeckMonad[Card, Pure] =
        DeckMonad(deck -> match deck {
            case Nil => None
            case c :: tl => (c, tl) |> Some
        })

    pub def drawCard2(): DeckMonad[(Card, Card), Pure] =
        DeckMonad(deck -> match deck {
            case c1 :: c2 :: tl => ((c1, c2), tl) |> Some
            case _ => None
        })

    pub def drawCard3(): DeckMonad[(Card, Card, Card), Pure] =
        DeckMonad(deck -> match deck {
            case c1 :: c2 :: c3 :: tl => ((c1, c2, c3), tl) |> Some
            case _ => None
        })

    pub def drawCard4(): DeckMonad[(Card, Card, Card, Card), Pure] =
        DeckMonad(deck -> match deck {
            case c1 :: c2 :: c3 :: c4 :: tl => ((c1, c2, c3, c4), tl) |> Some
            case _ => None
        })

    pub def run(d: Deck, dm: DeckMonad[a, ef]): Option[a] & ef = {
        let DeckMonad(op) = dm;
        op(d) |> Option.map(fst)
    }

}
