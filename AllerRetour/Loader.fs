namespace AllerRetour

open Fabulous

[<RequireQualifiedAccess>]
type LoaderState = Started | Stopped

[<RequireQualifiedAccess>]
module Loader =
  let private event = new Event<LoaderState>()
  
  let StateChanged = event.Publish

  let start () =
    event.Trigger(LoaderState.Started)
    Cmd.none

  let stop () =
    event.Trigger(LoaderState.Stopped)
    Cmd.none
