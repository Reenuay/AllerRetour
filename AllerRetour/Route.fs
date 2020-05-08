namespace AllerRetour

open Fabulous
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
