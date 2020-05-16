module AllerRetour.ChangeEmailSubPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open Views
open Resources

type Model = {
  Email: Validatable<EmailAddress, string>
  PreviousEmail: EmailAddress
  Password: Validatable<Password, string>
  PasswordHidden: bool
}
with
  member this.ToDto() : EmailAndPassword option =
    match this.Email, this.Password with
    | Ok e, Ok p ->
      Some { Email = e; Password = p }
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
      if e = this.PreviousEmail then
        Error ["This is the old value"]
      else
        Ok e

  member this.Revalidate() = {
    this with
      Email = Validatable.bindR this.CreateEmail (Validatable.value EmailAddress.value this.Email)
      Password = Validatable.bindR Password.create (Validatable.value Password.value this.Password)
  }

type Msg =
  | SetEmail of string
  | SetPassword of string
  | SwapPasswordHidden
  | ClickChange
  | ClickGoBack

type ExternalMsg =
  | NoOp
  | ChangeEmail of EmailAndPassword
  | GoBack

let create email = {
  Email = Validatable.emptyString
  PreviousEmail = email
  Password = Validatable.emptyString
  PasswordHidden = true
}

let update msg (model: Model) =
  match msg with
  | SetEmail e ->
    { model with Email = Validatable.bindR model.CreateEmail e }, NoOp

  | SetPassword p ->
    { model with Password = Validatable.bindR Password.create p }, NoOp

  | ClickChange ->
    match model.ToDto() with
    | Some d -> model, ChangeEmail d
    | None -> model.Revalidate(), NoOp

  | SwapPasswordHidden ->
    { model with PasswordHidden = not model.PasswordHidden }, NoOp

  | ClickGoBack ->
    model, GoBack

let view (model: Model) dispatch =
  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    dispatchBack = bindClick dispatch ClickGoBack,
    verticalOptions = LayoutOptions.StartAndExpand,
    children = [
      View.MakeAvatar(
        source = Images.passwordChange,
        margin = Thicknesses.mediumUpperBigLowerSpace
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
    ]
  )
