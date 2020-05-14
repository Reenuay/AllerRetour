namespace AllerRetour

open System
open Fabulous
open Xamarin.Forms
open ResponseTypes

[<RequireQualifiedAccess>]
type Route =
  | SignIn
  | SignUp
  | ForgotPassword
  | ResendEmail of string
  | Main of SignInResponse * ProfileResponse

[<RequireQualifiedAccess>]
module Route =
  let private event = new Event<Route>()

  let Changed = event.Publish

  let push route =
    event.Trigger(route)
    Cmd.none

[<RequireQualifiedAccess>]
module AppMessage =
  let show msg =
    Application
      .Current
      .MainPage
      .DisplayAlert( String.Empty, msg, "Ok" )
    |> ignore

    Cmd.none

[<RequireQualifiedAccess>]
module Authentication =
  let private event = new Event<SignInResponse option>()
  
  let StateChanged = event.Publish
  
  let changeState token =
    event.Trigger(token)
    Cmd.none
