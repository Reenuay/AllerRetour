namespace AllerRetour

open Fabulous

[<RequireQualifiedAccess>]
module Security =
  let private event = new Event<unit>()
  
  let DropToken = event.Publish

  let dropToken : Cmd<'msg> =
    [ fun _ ->
      event.Trigger() ]
