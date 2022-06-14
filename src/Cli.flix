/// Command-line interface
namespace Cli {
    use Console.{print, printLine};
    use ToString.toString;

    pub def main(): Unit & Impure = {
        "flix-cards ~ ~ ~ ~ ~" |> Console.bold |> printLine;
        startMenuLoop()
    }

    def startMenuLoop(): Unit & Impure = {
        allStartMenuItems() |> showStartMenuItems |> printLine;
        let parsedInput = Console.readLine() |> Option.flatMap(stringToStartMenuItem);
        match parsedInput {
            case None => startMenuLoop()
            case Some(Exit) => ()

            case Some(Play) => playMenuLoop(); startMenuLoop()
            case Some(Settings) => ???
        }
    }

    enum StartMenuItem with ToString, Eq {
        case Play, Settings, Exit
    }

    def allStartMenuItems(): List[StartMenuItem] =
        Play :: Settings :: Exit :: Nil

    def stringToStartMenuItem(s: String): Option[StartMenuItem] = {
        let ss =  s |> String.trim |> String.toLowerCase;
        match ss {
            case "play" => Play |> Some
            case "settings" => Settings |> Some
            case "exit" => Exit |> Some
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

    def stringToPlayMenuItem(s: String): Option[PlayMenuItem] = {
        let ss =  s |> String.trim |> String.toLowerCase;
        match ss {
            case "idiot" => IdiotSolitaire |> Some
            case "idiotsolitaire" => IdiotSolitaire |> Some
            case "back" => Back |> Some
            case _ => None
        }
    }

    def showPlayMenuItems(options: List[PlayMenuItem]): String =
        options |>
            List.map(o -> toString(o)) |>
            String.intercalate(" - ")

    def playMenuLoop(): Unit & Impure = {
        allPlayMenuItems() |> showPlayMenuItems |> printLine;
        let parsedInput = Console.readLine() |> Option.flatMap(stringToPlayMenuItem);
        match parsedInput {
            case None => playMenuLoop()
            case Some(Back) => ()

            case Some(IdiotSolitaire) => ???
        }
    }
}
