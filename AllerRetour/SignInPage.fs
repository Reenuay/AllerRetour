module AllerRetour.SignInPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open ResponseTypes
open Resources
open Views

type Model =
  private
    {
      Email: Validatable<EmailAddress>
      Password: Validatable<Password>
      PasswordHidden: bool
    }

type Msg =
  private
  | SetEmail of string
  | SetPassword of string
  | TogglePasswordHidden
  | SignIn
  | SignUp
  | ForgotPassword
  | SignedIn of EmailAddress * Http.Result<SignInResponse>
  | ProfileReceived of SignInResponse * Http.Result<ProfileResponse>

let initModel =
  {
    Email = Validatable.emptyString
    Password = Validatable.emptyString
    PasswordHidden = true
  }

let update msg model : Model * Cmd<Msg> =
  match msg with
  | SetEmail emailString ->
    let
      email =
        EmailAddress.validate emailString
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

  | SignIn ->
    let
      fields =
        (
          Validatable.tryValue model.Email,
          Validatable.tryValue model.Password
        )
    in
    match fields with
    | ( Some email, Some password )  ->
      let
        req =
          {
            Email = EmailAddress.value email
            Password = Password.value password
          }
      let
        cmd =
          Cmd.batch [
            Loader.start

            Cmd.ofAsyncMsg <|
              async {
                let! res = Http.signIn req
                return SignedIn ( email, res )
              }
          ]
      in
      ( model, cmd )

    | _ ->
      let
        email =
          EmailAddress.revalidate model.Email
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

  | SignUp ->
    ( model, Route.push Route.SignUp )

  | ForgotPassword ->
    ( model, Route.push Route.ForgotPassword )

  | SignedIn ( email, Ok token ) ->
    let
      confirmedCmd =
        Cmd.ofAsyncMsg <|
          async {
            let! res = Http.getProfile token.Token
            return ProfileReceived ( token, res )
          }
    let
      unconfirmedCmd =
        Cmd.batch
          [
            Route.push (Route.ResendEmail ( token, email ))

            Loader.stop
          ]
    let
      cmd =
        if token.EmailConfirmed
        then confirmedCmd
        else unconfirmedCmd
    in
    ( model, cmd )

  | ProfileReceived ( token, Ok profile ) ->
    let
      cmd =
        Cmd.batch
          [
            Route.push (Route.Main ( token, profile ))

            Loader.stop
          ]
    in
    ( model, cmd )

  | SignedIn ( _, Error errors )
  | ProfileReceived ( _, Error errors ) ->
    let
      cmd =
        Cmd.batch
          [
            Loader.stop

            Message.errors errors
          ]
    in
    ( model, cmd )

let view model dispatch =
  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    children = [
      View.Image(
        width = screenWidthP 0.5,
        source = Images.logo
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

      View.MakeThinText(
        text = "save on shopping\nsimply and tastefully",
        margin = Thicknesses.bigLowerSpace
      )

      View.MakeThinText(
        text = "login with email"
      )

      View.MakeEntry(
        value = model.Email,
        image = Images.envelopeIcon,
        keyboard = Keyboard.Email,
        placeholder = "Email",
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
        text = "log in",
        margin = Thicknesses.mediumLowerSpace,
        command = bindClick dispatch SignIn,
        isEnabled = isEnabled
      )

      View.Grid(
        width = screenWidthP 0.8,
        margin = Thicknesses.mediumLowerSpace,
        coldefs = [ Star; Star ],
        rowSpacing = 0.,
        columnSpacing = 0.,
        horizontalOptions = LayoutOptions.CenterAndExpand,
        children = [
          View.MakeTextButton(
            text = "forgot password?",
            margin = Thickness ( 0., -8., 0., 0. ),
            command = bindClick dispatch ForgotPassword,
            horizontalOptions = LayoutOptions.Start
          )
            .Column(0)

          View.MakeTextButton(
            text = "sign up",
            command = bindClick dispatch SignUp,
            fontFamily = Fonts.renogare,
            horizontalOptions = LayoutOptions.End
          )
            .Column(1)
        ]
      )
    ]
  )
