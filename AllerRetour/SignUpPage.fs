module AllerRetour.SignUpPage

open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open Views
open Resources

type Model = {
  FirstName: Validatable<NameString, string>
  LastName: Validatable<NameString, string>
  Email: Validatable<EmailAddress, string>
  Password: Validatable<Password, string>
  RepeatPassword: Validatable<string, string>
  PasswordHidden: bool
  PasswordRepeatHidden: bool
}
with
  member this.CheckRepeatPassword(r) =
    match underV Password.value this.Password with
    | x when x <> "" && x = r -> Ok r
    | _ -> Error ["Passwords must be the same"]

  member this.ToDto() : SignUpRequest option =
    match this.FirstName, this.LastName, this.Email, this.Password, this.RepeatPassword with
    | Ok f, Ok l, Ok e, Ok p, Ok _ ->
      Some {
        FirstName = NameString.value f
        LastName = NameString.value l
        Email = EmailAddress.value e
        Password = Password.value p
      }
    | _ ->
      None

  member this.IsValid() =
    match this.FirstName, this.LastName, this.Email, this.Password, this.RepeatPassword with
    | Ok _, Ok _, Ok _, Ok _, Ok _ -> true
    | _ -> false

  member this.Revalidate() = {
    this with
      FirstName = adaptV NameString.create (underV NameString.value this.FirstName)
      LastName = adaptV (NameString.create) (underV NameString.value this.LastName)
      Email = adaptV EmailAddress.create (underV EmailAddress.value this.Email)
      Password = adaptV Password.create (underV Password.value this.Password)
      RepeatPassword = adaptV this.CheckRepeatPassword (underV id this.RepeatPassword)
  }

type Msg =
  | SetFirstName of string
  | SetLastName of string
  | SetEmail of string
  | SetPassword of string
  | SetRepeatPassword of string
  | SwapPasswordHidden
  | SwapPasswordRepeatHidden
  | ClickSignUp
  | ClickGoToSignIn

type ExternalMsg =
  | NoOp
  | SignUp of SignUpRequest
  | GoToSignIn

let initModel = {
  FirstName = emptyString
  LastName = emptyString
  Email = emptyString
  Password = emptyString
  RepeatPassword = emptyString
  PasswordHidden = true
  PasswordRepeatHidden = true
}

let update msg (model: Model) =
  match msg with
  | SetFirstName f ->
    { model with FirstName = adaptV NameString.create f }, NoOp

  | SetLastName l ->
    { model with LastName = adaptV NameString.create l }, NoOp

  | SetEmail e ->
    { model with Email = adaptV EmailAddress.create e }, NoOp

  | SetPassword p ->
    { model with Password = adaptV Password.create p }, NoOp

  | SetRepeatPassword r ->
    { model with RepeatPassword = adaptV model.CheckRepeatPassword r }, NoOp

  | SwapPasswordHidden ->
    { model with PasswordHidden = not model.PasswordHidden }, NoOp

  | SwapPasswordRepeatHidden ->
    { model with PasswordRepeatHidden = not model.PasswordRepeatHidden }, NoOp

  | ClickSignUp ->
    match model.ToDto() with
    | Some d -> model, SignUp d
    | None -> model.Revalidate(), NoOp

  | ClickGoToSignIn ->
    model, GoToSignIn

let view model dispatch =
  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    children = [
      View.Image(
        source = Images.logo,
        width = screenWidthP 0.5
      )

      View.MakeThinText(
        text = "sign up with email",
        margin = Thicknesses.bigUpperSpace
      )

      View.MakeEntry(
        model.FirstName,
        "First name",
        NameString.value,
        (bindNewText dispatch SetFirstName),
        image = Images.userIcon
      )

      View.MakeEntry(
        model.LastName,
        "Last name",
        NameString.value,
        (bindNewText dispatch SetLastName),
        image = Images.userIcon
      )

      View.MakeEntry(
        model.Email,
        "Email",
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
        passwordOptions = (model.PasswordHidden, bindClick dispatch SwapPasswordHidden)
      )

      View.MakeEntry(
        model.RepeatPassword,
        "Re-enter password",
        id,
        (bindNewText dispatch SetRepeatPassword),
        image = Images.lockIcon,
        passwordOptions = (model.PasswordRepeatHidden, bindClick dispatch SwapPasswordRepeatHidden),
        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeButton(
        text = "sign up",
        command = bindClick dispatch ClickSignUp,
        isEnabled = model.IsValid(),
        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeTextButton(
        text = "already registered?",
        command = bindClick dispatch ClickGoToSignIn,
        margin = Thicknesses.mediumLowerSpace
      )
    ]
  )
