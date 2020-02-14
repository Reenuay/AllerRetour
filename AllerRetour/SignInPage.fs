module AllerRetour.SignInPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open TwoTrackResult
open PrimitiveTypes
open RequestTypes
open Resources
open Views

type Model = {
  Email: Validatable<EmailAddress, string>
  Password: Validatable<Password, string>
  PasswordHidden: bool
}
with
  member this.ToDto() : SignInRequest option =
    match this.Email, this.Password with
    | Success e, Success p ->
      Some { Email = EmailAddress.value e; Password = Password.value p }
    | _ ->
      None

  member this.IsValid() =
    match this.Email, this.Password with
    | Success _, Success _ -> true
    | _ -> false

  member this.Revalidate() = {
    this with
      Email = adaptV EmailAddress.create (underV EmailAddress.value this.Email)
      Password = adaptV Password.create (underV Password.value this.Password)
  }

type Msg =
  | SetEmail of string
  | SetPassword of string
  | SwapPasswordHidden
  | ClickSignIn
  | ClickGoToSignUp
  | ClickToForgotPassword

type ExternalMsg =
  | NoOp
  | SignIn of SignInRequest
  | GoToSignUp
  | GoToForgotPassword

let initModel = {
  Email = emptyString
  Password = emptyString
  PasswordHidden = true
}

let update msg (model: Model) =
  match msg with
  | SetEmail e ->
    { model with Email = adaptV EmailAddress.create e }, NoOp
  | SetPassword p ->
    { model with Password = adaptV Password.create p }, NoOp
  | SwapPasswordHidden ->
    { model with PasswordHidden = not model.PasswordHidden }, NoOp
  | ClickSignIn ->
    match model.ToDto() with
    | Some d -> model, SignIn d
    | None -> model.Revalidate(), NoOp
  | ClickGoToSignUp ->
    model, GoToSignUp
  | ClickToForgotPassword ->
    model, GoToForgotPassword

let view (model: Model) dispatch =    
  makePage [
    makeLogo ()

    makeLabel "justCash"

    makeThinText "save on shopping\nsimply and tastefully"
    |> padding Thicknesses.bigLowerSpace
    
    makeEntry
      None
      "Email"
      (Some Images.envelopeIcon)
      EmailAddress.value
      (bindNewText dispatch SetEmail)
      model.Email
    
    makeEntry
      (Some (model.PasswordHidden, bindPress dispatch SwapPasswordHidden))
      "Password"
      (Some Images.lockIcon)
      Password.value
      (bindNewText dispatch SetPassword)
      model.Password
    |> margin (Thicknesses.mediumLowerSpace)

    View.Button(
      text = "Sign In",
      isEnabled = model.IsValid(),
      command = bindPress dispatch ClickSignIn
    )

    View.Button(
      text = "Not registered?",
      command = bindPress dispatch ClickGoToSignUp
    )

    View.Button(
      text = "Forgot password?",
      command = bindPress dispatch ClickToForgotPassword
    )
  ]
