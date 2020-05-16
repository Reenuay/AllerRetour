namespace AllerRetour

open Fabulous
open ResponseTypes

[<RequireQualifiedAccess>]
type Route =
  | SignIn
  | SignUp
  | ForgotPassword
  | SignUpSuccess of string
  | ResendEmail of string
  | Main of SignInResponse * ProfileResponse

[<RequireQualifiedAccess>]
module Route =
  let private event = new Event<Route>()

  let Changed = event.Publish

  let push route =
    event.Trigger(route)
    Cmd.none
