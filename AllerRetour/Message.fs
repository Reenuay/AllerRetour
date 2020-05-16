namespace AllerRetour

open System
open Fabulous
open Xamarin.Forms

[<RequireQualifiedAccess>]
module Message =
  let show msg =
    Application
      .Current
      .MainPage
      .DisplayAlert( String.Empty, msg, "Ok" )
    |> ignore

    Cmd.none

  let foldErrors =
    List.fold (fun s v -> s + "\n" + v) ""

  let showErrors errors =
    show <| foldErrors errors
