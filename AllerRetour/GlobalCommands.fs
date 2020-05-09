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
  | Main of ProfileResponse

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
      .DisplayAlert(String.Empty, msg, "Ok")
    |> ignore

    Cmd.none

[<RequireQualifiedAccess>]
module Auth =
  let private event = new Event<SignInResponse>()
  
  let Authenticated = event.Publish
  
  let authenticate token =
    event.Trigger(token)
    Cmd.none
