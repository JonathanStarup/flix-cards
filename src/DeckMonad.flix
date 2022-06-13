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

    pub def drawCard(): DeckMonad[Card, Pure] =
        DeckMonad(deck -> match deck {
            case Nil => None
            case hd :: tl => (hd, tl) |> Some
        })

}
