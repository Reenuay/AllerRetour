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
      Email: Validatable<EmailAddress, string>
      PreviousEmail: EmailAddress
      Password: Validatable<Password, string>
      PasswordHidden: bool
    }
    with
      member this.CreateEmail(emailString) =
        match EmailAddress.create emailString with
        | Error x ->
          Error x

        | Ok e ->
          if e = this.PreviousEmail then
            Error ["This is the old value"]

          else
            Ok e

[<RequireQualifiedAccess>]
module Model =
  let checkRepeatPassword model newPasswordString =
    let
      passwordString =
        Validatable.value
          Password.value
          model.Password
    in
    match passwordString with
    | x when x <> "" && x = newPasswordString ->
      Ok newPasswordString

    | _ ->
      Error ["Passwords must be the same"]

type Msg =
  private
  | SetEmail of string
  | SetPassword of string
  | TogglePasswordHidden
  | ChangeEmail
  | SignIn
  | EmailChanged of Http.Result<string>

let initModel email =
  {
    Email = Validatable.emptyString
    PreviousEmail = email
    Password = Validatable.emptyString
    PasswordHidden = true
  }

let update tokenMaybe msg (model: Model) =
  match msg with
  | SetEmail emailString ->
    let
      email =
        Validatable.bindR
          model.CreateEmail
          emailString
    in
    ( { model with Email = email }, Cmd.none )

  | SetPassword passwordString ->
    let
      password =
        Validatable.bindR
          Password.create
          passwordString
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
      match ( model.Email, model.Password ) with
      | ( Ok email, Ok password ) ->
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
            Validatable.bindR
              EmailAddress.create
              (Validatable.value EmailAddress.value model.Email)
        let
          password =
            Validatable.bindR
              Password.create
              (Validatable.value Password.value model.Password)
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
        +  " Check your inbox to confirm your new email ID."
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
        passwordOptions = (model.PasswordHidden, bindClick dispatch TogglePasswordHidden),
        margin = Thicknesses.mediumLowerSpace
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
