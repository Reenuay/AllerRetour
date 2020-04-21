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
    | Success e, Success p ->
      Some { Email = e; Password = p }
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
      if e = this.PreviousEmail then
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
  | ClickGoBack

type ExternalMsg =
  | NoOp
  | ChangeEmail of EmailAndPassword
  | GoBack

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
    dispatchBack = bindPress dispatch ClickGoBack,
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
        passwordOptions = (model.PasswordHidden, bindPress dispatch SwapPasswordHidden),
        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeButton(
        text = "change",
        command = bindPress dispatch ClickChange,
        isEnabled = model.IsValid(),
        margin = Thicknesses.mediumLowerSpace
      )
    ]
  )
