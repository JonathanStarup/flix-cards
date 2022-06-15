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
        use IdiotSolitaire.IdiotSolitaire;
        use IdiotSolitaire/IdiotSolitaire.{drawCards, drawPileCount, getRunState, eliminatePile, moveCard};
        use IdiotSolitaire.GameRunState.{Playing, Won, Lost};

        pub def playGame(): Unit & Impure = {
            let game = ???(getSeed());
            ??? // gameLoop(game)
        }

        def getSeed(): Int32 & Impure =
            let seedMod = Time/Epoch.seconds() mod Int32.toInt64(Int32.maxValue());
            let seedOpt = Int64.tryToInt32(seedMod);
            match seedOpt {
                case None => ?unreachable
                case Some(v) => v
            }
        
        def stateString(g: game): String with IdiotSolitaire[game] = {
            let drawcount = g |> drawPileCount;
            ???
        }

        pub def gameLoop(g: game): Unit & Impure with IdiotSolitaire[game] = {
            g |> stateString |> printLine;
            def enterToReturn() = {
                printLine("Enter to return...");
                let _ = Cli.getInput();
                ()
            };
            def enterToContinue() = {
                printLine("Enter to continue...");
                let _ = Cli.getInput();
                gameLoop(g)
            };
            def runIfPileOrLoop(f, iStr: String) =
                match Int32.fromString(iStr) {
                    case Ok(pile) =>
                        if (1 <= pile and pile <= 4) f(pile)
                        else gameLoop(g)
                    case Err(_) => gameLoop(g)
                };
            match getRunState(g) {
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
                        case None => gameLoop(g)
                        case Some(i) => match i {
                            case "exit" :: Nil => ()

                            case "help" :: Nil =>
                                let commandList = "Exit" ::
                                    "Help" ::
                                    "Remove <pile>" ::
                                    "draw" ::
                                    "move <from> <to>" :: Nil;
                                commandList |> List.foreach(printLine);
                                enterToContinue()

                            case "remove" :: iStr :: Nil =>
                                runIfPileOrLoop(pile -> eliminatePile(pile, g) |> gameLoop, iStr)

                            case "draw" :: Nil =>
                                g |> drawCards |> gameLoop
                            
                            case "move" :: fromStr :: toStr :: Nil =>
                                fromStr |> runIfPileOrLoop(fromPile ->
                                    toStr |> runIfPileOrLoop(toPile ->
                                        moveCard(origin = fromPile, toPile, g) |> gameLoop
                                    )
                                )

                            case _ => gameLoop(g)
                        }
                    }
            }
        }
    }   
}