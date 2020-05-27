module AllerRetour.SignUpPage

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
      FirstName: Validatable<Name>
      LastName: Validatable<Name>
      Email: Validatable<EmailAddress>
      Password: Validatable<Password>
      PasswordHidden: bool
      RepeatPassword: Validatable<string>
      PasswordRepeatHidden: bool
    }

type Msg =
  private
  | SetFirstName of string
  | SetLastName of string
  | SetEmail of string
  | SetPassword of string
  | TogglePasswordHidden
  | SetRepeatPassword of string
  | TogglePasswordRepeatHidden
  | SignUp
  | SignIn
  | SignedUp of EmailAddress * Http.Result<string>

let initModel =
  {
    FirstName = Validatable.emptyString
    LastName = Validatable.emptyString
    Email = Validatable.emptyString
    Password = Validatable.emptyString
    PasswordHidden = true
    RepeatPassword = Validatable.emptyString
    PasswordRepeatHidden = true
  }

let update msg (model: Model) =
  match msg with
  | SetFirstName firstNameString ->
    let
      firstName =
        Name.validate firstNameString
    in
    ( { model with FirstName = firstName }, Cmd.none )

  | SetLastName lastNameString ->
    let
      lastName =
        Name.validate lastNameString
    in
    ( { model with LastName = lastName }, Cmd.none )

  | SetEmail emailString ->
    let
      email =
        EmailAddress.validate emailString
    in
    { model with Email = email }, Cmd.none

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

  | SetRepeatPassword repeatPasswordString ->
    let
      repeatPassword =
        Password.validateRepeat model.Password repeatPasswordString
    in
    ( { model with RepeatPassword = repeatPassword }, Cmd.none )

  | TogglePasswordRepeatHidden ->
    let
      repeatPasswordHidden =
        not model.PasswordHidden
    in
    ( { model with PasswordRepeatHidden = repeatPasswordHidden }, Cmd.none )

  | SignUp ->
    let
      fields =
        (
          Validatable.tryValue model.FirstName,
          Validatable.tryValue model.LastName,
          Validatable.tryValue model.Email,
          Validatable.tryValue model.Password,
          Validatable.tryValue model.RepeatPassword
        )
    in
    match fields with
    | (
        Some firstName,
        Some lastName,
        Some email,
        Some password,
        Some _
      ) ->
      let
        req =
          {
            FirstName = Name.value firstName
            LastName = Name.value lastName
            Email = EmailAddress.value email
            Password = Password.value password
          }
      let
        cmd =
          Cmd.batch
            [
              Loader.start

              Cmd.ofAsyncMsg <|
                async {
                  let! res = Http.signUp req
                  return SignedUp ( email, res )
                }
            ]
      in
      ( model, cmd )

    | _ ->
      let
        firstName =
          Name.revalidate model.FirstName
      let
        lastName =
          Name.revalidate model.LastName
      let
        email =
          EmailAddress.revalidate model.Email
      let
        password =
          Password.revalidate model.Password
      let
        repeatPassword =
          Password.revalidateRepeat model.Password model.RepeatPassword
      let
        newModel =
          {
            model with
              Email = email
              LastName = lastName
              Password = password
              FirstName = firstName
              RepeatPassword = repeatPassword
          }
      in
      ( newModel, Cmd.none )

  | SignIn ->
    ( model, Route.push Route.SignIn )

  | SignedUp ( email, Ok _ ) ->
    let
      cmd =
        Cmd.batch [
          Route.push (Route.SignUpSuccess email)

          Loader.stop
        ]
    in
    ( model, cmd )

  | SignedUp ( _, Error errors ) ->
    let
      cmd =
        Cmd.batch [
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

      View.MakeThinText(
        text = "sign up with email",
        margin = Thicknesses.bigUpperSpace
      )

      View.MakeEntry(
        value = model.FirstName,
        image = Images.userIcon,
        placeholder = "First name",
        textChanged = bindNewText dispatch SetFirstName
      )

      View.MakeEntry(
        value = model.LastName,
        image = Images.userIcon,
        placeholder = "Last name",
        textChanged = bindNewText dispatch SetLastName
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
        placeholder = "Password",
        textChanged = bindNewText dispatch SetPassword,
        passwordOptions = passwordOptions
      )

      let
        passwordOptions =
          (
            model.PasswordRepeatHidden,
            bindClick dispatch TogglePasswordRepeatHidden
          )
      in
      View.MakeEntry(
        value = model.RepeatPassword,
        image = Images.lockIcon,
        margin = Thicknesses.mediumLowerSpace,
        placeholder = "Re-enter password",
        textChanged = bindNewText dispatch SetRepeatPassword,
        passwordOptions = passwordOptions
      )

      let
        isEnabled =
          Validatable.isValid model.FirstName
          && Validatable.isValid model.LastName
          && Validatable.isValid model.Email
          && Validatable.isValid model.Password
          && Validatable.isValid model.RepeatPassword
      in
      View.MakeButton(
        text = "sign up",
        margin = Thicknesses.mediumLowerSpace,
        command = bindClick dispatch SignUp,
        isEnabled = isEnabled
      )

      View.MakeTextButton(
        text = "already registered?",
        margin = Thicknesses.mediumLowerSpace,
        command = bindClick dispatch SignIn
      )
    ]
  )
