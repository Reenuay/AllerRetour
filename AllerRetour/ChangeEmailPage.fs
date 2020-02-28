module AllerRetour.ChangeEmailPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open Resources
open Views

type Model = {
  Email: Validatable<EmailAddress, string>
  PreviousEmail: string
  Password: Validatable<Password, string>
  PasswordHidden: bool
}
with
  member this.ToDto() : ChangeEmailRequest option =
    match this.Email, this.Password with
    | Success e, Success p ->
      Some { NewEmail = EmailAddress.value e; Password = Password.value p }
    | _ ->
      None

  member this.IsValid() =
    match this.Email, this.Password with
    | Success _, Success _ -> true
    | _ -> false

  member this.CreateEmail(email) =
    match EmailAddress.create email with
    | Failure x -> Failure x
    | Success e ->
      if EmailAddress.value e = this.PreviousEmail then
        Failure ["This is the old value"]
      else
        Success e

  member this.Revalidate() = {
    this with
      Email = adaptV this.CreateEmail (underV EmailAddress.value this.Email)
      Password = adaptV Password.create (underV Password.value this.Password)
  }

type Msg =
  | SetEmail of string
  | SetPassword of string
  | SwapPasswordHidden
  | ClickChange
  | ClickGoToSignIn

type ExternalMsg =
  | NoOp
  | ChangeEmail of ChangeEmailRequest
  | GoToSignIn

let create email = {
  Email = emptyString
  PreviousEmail = email
  Password = emptyString
  PasswordHidden = true
}

let update msg (model: Model) =
  match msg with
  | SetEmail e ->
    { model with Email = adaptV model.CreateEmail e }, NoOp

  | SetPassword p ->
    { model with Password = adaptV Password.create p }, NoOp

  | SwapPasswordHidden ->
    { model with PasswordHidden = not model.PasswordHidden }, NoOp

  | ClickChange ->
    match model.ToDto() with
    | Some d -> model, ChangeEmail d
    | None -> model.Revalidate(), NoOp

  | ClickGoToSignIn ->
    model, GoToSignIn

let view (model: Model) dispatch =
  makeScrollStackPage [
    Images.passwordChange
    |> makeCircle
    |> margin Thicknesses.mediumUpperBigLowerSpace

    makeInfoText "Please enter new email"
    |> margin Thicknesses.mediumLowerSpace

    makeEntry
      None
      (Some Keyboard.Email)
      "New email"
      (Some Images.envelopeIcon)
      EmailAddress.value
      (bindNewText dispatch SetEmail)
      model.Email
        
    makeEntry
      (Some (model.PasswordHidden, bindPress dispatch SwapPasswordHidden))
      None
      "Password"
      (Some Images.lockIcon)
      Password.value
      (bindNewText dispatch SetPassword)
      model.Password
    |> margin Thicknesses.mediumLowerSpace

    makeButton
      (model.IsValid())
      (bindPress dispatch ClickChange)
      "change"
    |> margin Thicknesses.mediumLowerSpace

    makeNavButton
      (bindPress dispatch ClickGoToSignIn)
      "log in"
    |> margin Thicknesses.mediumLowerSpace
  ]
