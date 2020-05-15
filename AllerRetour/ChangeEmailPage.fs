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
    | Ok e, Ok p ->
      Some { NewEmail = EmailAddress.value e; Password = Password.value p }
    | _ ->
      None

  member this.IsValid() =
    match this.Email, this.Password with
    | Ok _, Ok _ -> true
    | _ -> false

  member this.CreateEmail(email) =
    match EmailAddress.create email with
    | Error x -> Error x
    | Ok e ->
      if EmailAddress.value e = this.PreviousEmail then
        Error ["This is the old value"]
      else
        Ok e

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
  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    children = [
      
      View.MakeAvatar(
        source = Images.passwordChange,
        margin = Thicknesses.bigLowerSpace
      )

      View.MakeText(
        text = "Please enter new email",
        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeEntry(
        model.Email,
        "New email",
        EmailAddress.value,
        (bindNewText dispatch SetEmail),
        keyboard = Keyboard.Email,
        image = Images.envelopeIcon
      )

      View.MakeEntry(
        model.Password,
        "Password",
        Password.value,
        (bindNewText dispatch SetPassword),
        image = Images.lockIcon,
        passwordOptions = (model.PasswordHidden, bindClick dispatch SwapPasswordHidden),
        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeButton(
        text = "change",
        command = bindClick dispatch ClickChange,
        isEnabled = model.IsValid(),
        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeTextButton(
        text = "log in",
        command = bindClick dispatch ClickGoToSignIn,
        margin = Thicknesses.mediumLowerSpace,
        fontFamily = Fonts.renogare
      )
    ]
  )
