namespace AllerRetour

open Fabulous

[<RequireQualifiedAccess>]
type LoaderState = Started | Stopped

[<RequireQualifiedAccess>]
module Loader =
  let private event = new Event<LoaderState>()
  
  let StateChanged = event.Publish

  let start : Cmd<'msg> =
    [ fun _ ->
      event.Trigger(LoaderState.Started) ]

  let stop : Cmd<'msg> =
    [ fun _ ->
      event.Trigger(LoaderState.Stopped) ]
