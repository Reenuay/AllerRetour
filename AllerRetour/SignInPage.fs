module AllerRetour.SignInPage

open Fabulous.XamarinForms
open Xamarin.Forms
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
  View.MakeScrollStackPage(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    children = [
      View.Image(
        source = Images.logo,
        width = screenWidthP 0.5
      )

      View.Label(
        text = "justCash",
        margin = Thicknesses.bigUpperSpace,
        padding = Thickness 5.,
        fontSize = FontSizes.big,
        textColor = Colors.accent,
        fontFamily = Fonts.renogare,
        horizontalTextAlignment = TextAlignment.Center
      )

      makeThinText "save on shopping\nsimply and tastefully"
      |> margin Thicknesses.bigLowerSpace

      makeThinText "login with email"

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
        passwordOptions = (model.PasswordHidden, bindPress dispatch SwapPasswordHidden),
        margin = Thicknesses.mediumLowerSpace
      )

      makeButton
        (model.IsValid())
        (bindPress dispatch ClickSignIn)
        "log in"
      |> margin Thicknesses.mediumLowerSpace

      makeDuoGrid
        (makeLink (bindPress dispatch ClickToForgotPassword) "forgot password?"
        |> margin Thicknesses.duoGridCentering)
        (makeNavButton (bindPress dispatch ClickGoToSignUp) "sign up")
      |> margin Thicknesses.mediumLowerSpace
    ]
  )
