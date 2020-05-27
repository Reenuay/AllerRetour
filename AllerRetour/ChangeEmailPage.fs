module AllerRetour.ChangeEmailPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open Resources
open Views

type Model =
  private
    {
      Email: Validatable<EmailAddress>
      OldEmail: EmailAddress
      Password: Validatable<Password>
      PasswordHidden: bool
    }

type Msg =
  private
  | SetEmail of string
  | SetPassword of string
  | TogglePasswordHidden
  | ChangeEmail
  | SignIn
  | EmailChanged of Http.Result<string>

let initModel oldEmail =
  {
    Email = Validatable.emptyString
    OldEmail = oldEmail
    Password = Validatable.emptyString
    PasswordHidden = true
  }

let update tokenMaybe msg model =
  match msg with
  | SetEmail emailString ->
    let
      email =
        EmailAddress.validateWithOld model.OldEmail emailString
    in
    ( { model with Email = email }, Cmd.none )

  | SetPassword passwordString ->
    let
      password =
        Password.validate passwordString
    in
    ( { model with Password = password }, Cmd.none )

  | TogglePasswordHidden ->
    let
      passwordHidden =
        not model.PasswordHidden
    in
    ( { model with PasswordHidden = passwordHidden }, Cmd.none )

  | ChangeEmail ->
    match tokenMaybe with
    | Some token ->
      let
        fields =
          (
            Validatable.tryValue model.Email,
            Validatable.tryValue model.Password
          )
      in
      match fields with
      | ( Some email, Some password ) ->
        let
          req =
            {
              NewEmail = EmailAddress.value email
              Password = Password.value password
            }
        let
          cmd =
            Cmd.batch
              [
                Loader.start

                Cmd.ofAsyncMsg <|
                  async {
                    let! res = Http.changeEmail token req
                    return EmailChanged res
                  }
              ]
        ( model, cmd )

      | _ ->
        let
          email =
            EmailAddress.revalidateWithOld model.OldEmail model.Email
        let
          password =
            Password.revalidate model.Password
        let
          newModel =
            {
              model with
                Email = email
                Password = password
            }
        in
        ( newModel, Cmd.none )

    | None ->
      let cmd =
        Cmd.batch
          [
            Route.push Route.SignIn

            Message.show "You are not signed in."
          ]
      in
      ( model, cmd )

  | SignIn ->
    ( model, Route.push Route.SignIn )

  | EmailChanged (Ok _) ->
    let
      message =
        "Your email has been successfully changed!"
        + " Check your inbox to confirm your new email ID."
    let
      cmd =
        Cmd.batch
          [
            Loader.stop

            Security.dropToken

            Message.show message
          ]
    in
    ( model, cmd )

  | EmailChanged (Error errors) ->
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
        source = Images.passwordChange,
        margin = Thicknesses.bigLowerSpace
      )

      View.MakeText(
        text = "Please enter new email",
        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeEntry(
        value = model.Email,
        image = Images.envelopeIcon,
        keyboard = Keyboard.Email,
        placeholder = "New email",
        textChanged = bindNewText dispatch SetEmail
      )

      let
        passwordOptions =
          (
            model.PasswordHidden,
            bindClick dispatch TogglePasswordHidden
          )
      in
      View.MakeEntry(
        value = model.Password,
        image = Images.lockIcon,
        margin = Thicknesses.mediumLowerSpace,
        placeholder = "Password",
        textChanged = bindNewText dispatch SetPassword,
        passwordOptions = passwordOptions
      )

      let
        isEnabled =
          Validatable.isValid model.Email
          && Validatable.isValid model.Password
      in
      View.MakeButton(
        text = "change",
        command = bindClick dispatch ChangeEmail,
        isEnabled = isEnabled,
        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeTextButton(
        text = "log in",
        command = bindClick dispatch SignIn,
        margin = Thicknesses.mediumLowerSpace,
        fontFamily = Fonts.renogare
      )
    ]
  )
