module AllerRetour.ForgotPasswordPage

open Fabulous
open PrimitiveTypes
open RequestTypes
open Resources
open Views
open Xamarin.Forms

type Model =
  private
    {
      Email: Validatable<EmailAddress, string>
    }

type Msg =
  private
  | SetEmail of string
  | Send
  | SignIn
  | PinSent of Http.Result<string>

let initModel =
  {
    Email = Validatable.emptyString
  }

let update msg (model: Model) =
  match msg with
  | SetEmail emailString ->
    let
      email =
        Validatable.bindR
          EmailAddress.create
          emailString
    in
    ( { model with Email = email }, Cmd.none )

  | Send ->
    match model.Email with
    | Ok email ->
      let
        req : PasswordResetEmailRequest =
          {
            Email = EmailAddress.value email
          }
      let
        cmd =
          Cmd.batch [
            Loader.start

            Cmd.ofAsyncMsg <|
              async {
                let! res = Http.sendPin req
                return PinSent res
              }
            ]
      in
      ( model, cmd )

    | Error ( emailString, _ ) ->
      let
        email =
          Validatable.bindR
            EmailAddress.create
            emailString
      in
      ( { model with Email = email }, Cmd.none )

  | SignIn ->
    ( model, Route.push Route.SignIn )

  | PinSent (Ok _) ->
    match model.Email with
    | Ok email ->
      let
        cmd =
          Cmd.batch
            [
              Route.push (Route.ResetPassword email)

              Loader.stop
            ]
      in
      ( model, cmd )

    | Error ( emailString, _ ) ->
      let
        newEmail =
          Validatable.bindR EmailAddress.create emailString
      in
      ( { model with Email = newEmail }, Cmd.none )

  | PinSent (Error errors) ->
    let
      cmd =
        Cmd.batch
          [
            Loader.stop

            Message.errors errors
          ]
    in
    ( model, cmd )

let view (model: Model) dispatch =
  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    children = [
      View.MakeAvatar(
        source = Images.forgotPassword,
        margin = Thicknesses.bigLowerSpace
      )

      View.MakeText(
        text = "Please enter your registered email ID"
      )

      View.MakeThinText(
        text = "We will send a verification code\n to your registered email ID"
      )

      View.MakeEntry(
        map = EmailAddress.value,
        value = model.Email,
        image = Images.envelopeIcon,
        margin = Thicknesses.mediumLowerSpace,
        keyboard = Keyboard.Email,
        placeholder = "Email",
        textChanged = bindNewText dispatch SetEmail
      )

      View.MakeButton(
        text = "send",
        margin = Thicknesses.mediumLowerSpace,
        command = bindClick dispatch Send,
        isEnabled = Validatable.isValid model.Email
      )

      View.MakeTextButton(
        text = "log in",
        margin = Thicknesses.mediumLowerSpace,
        command = bindClick dispatch SignIn,
        fontFamily = Fonts.renogare
      )
    ]
  )
