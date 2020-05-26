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
  | ResendEmail of SignInResponse * EmailAddress
  | ChangeEmail of EmailAddress
  | Main of SignInResponse * ProfileResponse

[<RequireQualifiedAccess>]
module Route =
  let private event = new Event<Route>()

  let Changed = event.Publish

  let push route =
    [ fun _ ->
        event.Trigger(route) ]
