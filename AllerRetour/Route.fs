namespace AllerRetour

open Fabulous
open PrimitiveTypes
open ResponseTypes

[<RequireQualifiedAccess>]
type Route =
  | SignIn
  | SignUp
  | ForgotPassword
  | ResetPassword of EmailAddress
  | SignUpSuccess of EmailAddress
  | ResendEmail of string
  | Main of SignInResponse * ProfileResponse

[<RequireQualifiedAccess>]
module Route =
  let private event = new Event<Route>()

  let Changed = event.Publish

  let push route : Cmd<'msg> =
    [ fun _ ->
      event.Trigger(route) ]
