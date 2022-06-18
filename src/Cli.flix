/// Command-line interface
namespace Cli {
    use ToString.toString;

    def print(x: s): Unit & Impure with ToString[s] = {
        Console.print(x);
        Console.flush()
    }

    def printLine(x: s): Unit & Impure with ToString[s] = Console.printLine(x)

    def readLine(): Option[String] & Impure = Console.readLine()

    def escape(s: String): String = "\u001B${s}"

    def csi(s: String): String = escape("[${s}")

    def clearScreen(): Unit & Impure =
        csi("2J") |> print;
        csi("${1};${1}H") |> print

    def enterToContinue(): Unit & Impure = {
        printLine("Enter to continue...");
        let _ = Cli.getInput();
       ()
    }

    pub def main(): Unit & Impure = {
        clearScreen();
        "flix-cards ~ ~ ~ ~ ~" |> Console.bold |> printLine;
        "You interact by ..." |> printLine;
        enterToContinue();
        startMenuLoop()
    }

    def getInput(): Option[List[String]] & Impure = {
        print("> ");
        readLine() |>
            Option.map(String.toLowerCase) |>
            Option.map(String.words)
    }

    def startMenuLoop(): Unit & Impure = {
        clearScreen();
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
        clearScreen();
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
        use IdiotSolitaireImp.{newGame, getRunState, eliminatePile, autoSolve,
            drawCards, moveCard, drawPileCount, pileString};
        use IdiotSolitaireImp.Game;
        use GameRunState.GameRunState.{Playing, Won, Lost};
        use Cli.{getInput, print, printLine, clearScreen, enterToContinue};

        def runIfPileOrLoop(f: Int32 -> Unit & ef, s: String): Unit & ef = {
            match Int32.fromString(s) {
                case Ok(pile) =>
                    if (1 <= pile and pile <= 4) f(pile)
                    else ()
                case Err(_) =>()
            }
        }

        pub def playGame(): Unit & Impure = {
            let game = newGame(getSeed());
            def gameLoop() = {
                clearScreen();
                game |> stateString |> printLine;
                if (not gameEnded(game)) {
                    let gameExit = receiveCommand(game);
                    if (not gameExit) gameLoop() else ()
                } else ()
            };
            gameLoop()
        }

        def gameEnded(game: Game): Bool & Impure = match getRunState(game) {
            case Won =>
                let msg = " x x x x x x x " ::
                            "x x You Won x x" ::
                            " x x x x x x x " :: Nil;
                msg |> List.foreach(printLine);
                enterToContinue();
                true

            case Lost =>
                "You Lost :(" |> printLine;
                enterToContinue();
                true
        
            case Playing =>
                false
        }

        /// Returns true if the game should end
        def receiveCommand(game: Game): Bool & Impure = match getInput() {
            case None => false
            case Some(i) => match i {
                case "exit" :: Nil => true

                case "help" :: Nil  =>
                    clearScreen();
                    let commandList = "Exit" ::
                        "Help" ::
                        "Remove <pile>" ::
                        "draw" ::
                        "auto" ::
                        "move <from> <to>" :: Nil;
                    commandList |> List.foreach(printLine);
                    enterToContinue();
                    false

                case "remove" :: iStr :: Nil =>
                    iStr |> runIfPileOrLoop(pile -> {
                        discard eliminatePile(pile, game)
                    });
                    false

                case "draw" :: Nil =>
                    drawCards(game);
                    false

                case "auto" :: Nil =>
                    autoSolve(100i64, _ -> {
                        clearScreen();
                        game |> stateString |> printLine
                    }, game);
                    false
                
                case "move" :: fromStr :: toStr :: Nil =>
                    fromStr |> runIfPileOrLoop(fromPile ->
                        (toStr |> runIfPileOrLoop(toPile -> {
                            moveCard(origin = fromPile, toPile, game)
                        }))
                    );
                    false

                case iStr :: Nil =>
                    iStr |> runIfPileOrLoop(pile -> {
                        discard eliminatePile(pile, game)
                    });
                    false

                case fromStr :: toStr :: Nil =>
                    fromStr |> runIfPileOrLoop(fromPile ->
                        (toStr |> runIfPileOrLoop(toPile -> {
                            moveCard(origin = fromPile, toPile, game)
                        }))
                    );
                    false

                case _ => false
            }
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
