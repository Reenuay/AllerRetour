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
  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    children = [
      View.MakeAvatar(
        source = Images.forgotPassword,
        margin = Thicknesses.bigLowerSpace
      )

      View.MakeText("Please enter your registered email ID")

      View.MakeThinText("We will send a verification code\n to your registered email ID")

      View.MakeEntry(
        model,
        "Email",
        EmailAddress.value,
        (bindNewText dispatch SetEmail),
        keyboard = Keyboard.Email,
        image = Images.envelopeIcon,
        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeButton(
        text = "send",
        command = bindPress dispatch ClickSend,
        isEnabled = TwoTrackResult.isSuccess model,
        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeTextButton(
        text = "log in",
        command = bindPress dispatch ClickGoToSignIn,
        margin = Thicknesses.mediumLowerSpace,
        fontFamily = Fonts.renogare
      )
    ]
  )
