module AllerRetour.ForgotPasswordPage

open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open Views
open Resources

type Model = Validatable<EmailAddress, string>

type Msg =
  | SetEmail of string
  | ClickSend
  | ClickGoToSignIn

type ExternalMsg =
  | NoOp
  | Send of PasswordResetEmailRequest
  | GoToSignIn

let initModel = emptyString

let update msg (model: Model) =
  match msg with
  | SetEmail e -> adaptV EmailAddress.create e, NoOp

  | ClickSend ->
    match model with
    | Success e -> model, Send { Email = EmailAddress.value e }
    | Failure (x, _) -> adaptV EmailAddress.create x, NoOp

  | ClickGoToSignIn -> model, GoToSignIn

let view (model: Model) dispatch =
  View.MakeScrollStackPage(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    children = [
      Images.forgotPassword
      |> makeCircle
      |> margin Thicknesses.bigLowerSpace

      makeInfoText "Please enter your registered email ID"

      makeThinText "We will send a verification code\n to your registered email ID"

      View.MakeEntry(
        model,
        "Email",
        EmailAddress.value,
        (bindNewText dispatch SetEmail),
        keyboard = Keyboard.Email,
        image = Images.envelopeIcon,
        margin = Thicknesses.mediumLowerSpace
      )

      makeButton
        (TwoTrackResult.isSuccess model)
        (bindPress dispatch ClickSend)
        "send"
      |> margin Thicknesses.mediumLowerSpace

      makeNavButton
        (bindPress dispatch ClickGoToSignIn)
        "log in"
      |> margin Thicknesses.mediumLowerSpace
    ]
  )
