/// Command-line interface
namespace Cli {
    use Console.{print, printLine, flush};
    use ToString.toString;

    pub def main(): Unit & Impure = {
        "flix-cards ~ ~ ~ ~ ~" |> Console.bold |> printLine;
        startMenuLoop()
    }

    def getInput(): Option[List[String]] & Impure = {
        print("> "); flush();
        Console.readLine() |>
            Option.map(String.toLowerCase) |>
            Option.map(String.words)
    }

    def startMenuLoop(): Unit & Impure = {
        allStartMenuItems() |> showStartMenuItems |> printLine;
        let parsedInput = getInput() |> Option.flatMap(stringToStartMenuItem);
        match parsedInput {
            case None => startMenuLoop()
            case Some(Exit) => ()

            case Some(Play) => playMenuLoop(); startMenuLoop()
        }
    }

    enum StartMenuItem with ToString, Eq {
        case Play, Exit
    }

    def allStartMenuItems(): List[StartMenuItem] =
        Play :: Exit :: Nil

    def stringToStartMenuItem(input: List[String]): Option[StartMenuItem] = {
        match input {
            case "play" :: Nil => Play |> Some
            case "exit" :: Nil => Exit |> Some
            case _ => None
        }
    }
    
    def showStartMenuItems(options: List[StartMenuItem]): String =
        options |>
            List.map(o -> toString(o)) |>
            String.intercalate(" - ")

    enum PlayMenuItem with ToString, Eq {
        case IdiotSolitaire, Back
    }

    def allPlayMenuItems(): List[PlayMenuItem] =
        IdiotSolitaire :: Back :: Nil

    def stringToPlayMenuItem(input: List[String]): Option[PlayMenuItem] = {
        match input {
            case "idiot" :: Nil => IdiotSolitaire |> Some
            case "idiotsolitaire" :: Nil => IdiotSolitaire |> Some
            case "back" :: Nil => Back |> Some
            case _ => None
        }
    }

    def showPlayMenuItems(options: List[PlayMenuItem]): String =
        options |>
            List.map(o -> toString(o)) |>
            String.intercalate(" - ")

    def playMenuLoop(): Unit & Impure = {
        allPlayMenuItems() |> showPlayMenuItems |> printLine;
        let parsedInput = getInput() |> Option.flatMap(stringToPlayMenuItem);
        match parsedInput {
            case None => playMenuLoop()
            case Some(Back) => ()

            case Some(IdiotSolitaire) =>
                Cli/IdiotMenu.playGame();
                playMenuLoop()
        }
    }


    namespace IdiotMenu {
        use IdiotSolitaireImp.{newGame, getRunState, eliminatePile,
            drawCards, moveCard, drawPileCount, pileString};
        use IdiotSolitaireImp.Game;
        use GameRunState.GameRunState.{Playing, Won, Lost};

        def enterToReturn(): Unit & Impure = {
            printLine("Enter to return...");
            let _ = Cli.getInput();
            ()
        }

        pub def playGame(): Unit & Impure = {
            let game = newGame(getSeed());
            def gameLoop() = {
                def enterToContinue() = {
                    printLine("Enter to continue...");
                    let _ = Cli.getInput();
                    gameLoop()
                };
                def runIfPileOrLoop(f, iStr: String) = {
                    match Int32.fromString(iStr) {
                        case Ok(pile) =>
                            if (1 <= pile and pile <= 4) f(pile)
                            else gameLoop()
                        case Err(_) => gameLoop()
                    }
                };
                game |> stateString |> printLine;
                match getRunState(game) {
                    case Won =>
                        let msg = " x x x x x x x " ::
                                  "x x You Won x x" ::
                                  " x x x x x x x " :: Nil;
                        msg |> List.foreach(printLine);
                        enterToReturn()

                    case Lost =>
                        "You Lost :(" |> printLine;
                        enterToReturn()
                
                    case Playing =>
                        match Cli.getInput() {
                            case None => gameLoop()
                            case Some(i) => match i {
                                case "exit" :: Nil => ()

                                case "help" :: Nil  =>
                                    let commandList = "Exit" ::
                                        "Help" ::
                                        "Remove <pile>" ::
                                        "draw" ::
                                        "move <from> <to>" :: Nil;
                                    commandList |> List.foreach(printLine);
                                    enterToContinue()

                                case "remove" :: iStr :: Nil =>
                                    iStr |> runIfPileOrLoop(pile -> {
                                        eliminatePile(pile, game);
                                        gameLoop()
                                    })

                                case "draw" :: Nil =>
                                    drawCards(game);
                                    gameLoop()
                                
                                case "move" :: fromStr :: toStr :: Nil =>
                                    fromStr |> runIfPileOrLoop(fromPile ->
                                        (toStr |> runIfPileOrLoop(toPile -> {
                                            moveCard(origin = fromPile, toPile, game);
                                            gameLoop()
                                        }))
                                    )

                                case iStr :: Nil =>
                                    iStr |> runIfPileOrLoop(pile -> {
                                        eliminatePile(pile, game);
                                        gameLoop()
                                    })

                                case fromStr :: toStr :: Nil =>
                                    fromStr |> runIfPileOrLoop(fromPile ->
                                        (toStr |> runIfPileOrLoop(toPile -> {
                                            moveCard(origin = fromPile, toPile, game);
                                            gameLoop()
                                        }))
                                    )

                                case _ => gameLoop()
                            }
                        }
                }
            };
            gameLoop()
        }

        def getSeed(): Int32 & Impure = {
            let seedMod = Time/Epoch.seconds() mod Int32.toInt64(Int32.maxValue());
            let seedOpt = Int64.tryToInt32(seedMod);
            match seedOpt {
                case None => ?unreachable
                case Some(v) => v
            }
        }
        
        def stateString(g: Game): String & Impure = {
            use Int32.max;
            use Array.length;
            let drawCount = drawPileCount(g);
            let deckLine = "Deck: ${drawCount}";
            let pile1 = pileString(1, g);
            let pile2 = pileString(2, g);
            let pile3 = pileString(3, g);
            let pile4 = pileString(4, g);
            let maxHeight = length(pile1) `max` length(pile2) `max` length(pile3) `max` length(pile4);
            let cardPiles = List.range(0, maxHeight) |> List.map(i -> {
                def ps(p) = p |> arrayGetOpt(i) |> Option.getWithDefault("   ") |> toString;
                "${ps(pile1)} ${ps(pile2)} ${ps(pile3)} ${ps(pile4)}"
            });
            (deckLine :: cardPiles) |> String.intercalate(String.lineSeparator())
        }

        def arrayGetOpt(i: Int32, a: Array[a]): Option[a] & Impure =
            if (0 <= i and i < a.length) Some(a[i]) else None
    }   
}
