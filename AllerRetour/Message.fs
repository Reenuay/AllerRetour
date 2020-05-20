namespace AllerRetour

open System
open Fabulous
open Xamarin.Forms

[<RequireQualifiedAccess>]
module Message =
  let show msg =
    Cmd.ofAsyncMsgOption <|
      async {
        Application
          .Current
          .MainPage
          .DisplayAlert( String.Empty, msg, "Ok" )
        |> ignore

        return None
      }

  let foldErrors =
    List.fold (fun s v -> s + "\n" + v) ""

  let errors errorList =
    show <| foldErrors errorList
