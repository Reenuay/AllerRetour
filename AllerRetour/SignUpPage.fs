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
      FirstName: Validatable<NameString, string>
      LastName: Validatable<NameString, string>
      Email: Validatable<EmailAddress, string>
      Password: Validatable<Password, string>
      RepeatPassword: Validatable<string, string>
      PasswordHidden: bool
      PasswordRepeatHidden: bool
    }

type Msg =
  private
  | SetFirstName of string
  | SetLastName of string
  | SetEmail of string
  | SetPassword of string
  | SetRepeatPassword of string
  | SwapPasswordHidden
  | SwapPasswordRepeatHidden
  | SignUp
  | SignIn
  | SignedUp of Http.Result<string>

[<RequireQualifiedAccess>]
module Model =
  let checkRepeatPassword model p =
    match Validatable.value Password.value model.Password with
    | x when x <> "" && x = p ->
      Ok p

    | _ ->
      Error ["Passwords must be the same"]

  let toDto model =
    let
      fields =
        (
          model.FirstName,
          model.LastName,
          model.Email,
          model.Password,
          model.RepeatPassword
        )
    in
    match fields with
    | ( Ok f, Ok l, Ok e, Ok p, Ok _ ) ->
      Some
        {
          FirstName = NameString.value f
          LastName = NameString.value l
          Email = EmailAddress.value e
          Password = Password.value p
        }

    | _ ->
      None

  let isValid model
    =  Validatable.isValid model.FirstName
    && Validatable.isValid model.LastName
    && Validatable.isValid model.Email
    && Validatable.isValid model.Password
    && Validatable.isValid model.RepeatPassword

  let revalidate model =
    let
      firstName =
        Validatable.bindR
          NameString.create
          (Validatable.value NameString.value model.FirstName)
    let
      lastName =
        Validatable.bindR
          NameString.create
          (Validatable.value NameString.value model.LastName)
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
      repeatPassword =
        Validatable.bindR
          (checkRepeatPassword model)
          (Validatable.value id model.RepeatPassword)
    in
    {
      model with
        Email = email
        LastName = lastName
        Password = password
        FirstName = firstName
        RepeatPassword = repeatPassword
    }

let initModel =
  {
    FirstName = Validatable.emptyString
    LastName = Validatable.emptyString
    Email = Validatable.emptyString
    Password = Validatable.emptyString
    RepeatPassword = Validatable.emptyString
    PasswordHidden = true
    PasswordRepeatHidden = true
  }

let update msg (model: Model) =
  match msg with
  | SetFirstName firstNameString ->
    let
      firstName =
        Validatable.bindR NameString.create firstNameString
    in
    ( { model with FirstName = firstName }, Cmd.none )

  | SetLastName lastNameString ->
    let
      lastName =
        Validatable.bindR NameString.create lastNameString
    in
    ( { model with LastName = lastName }, Cmd.none )

  | SetEmail emailString ->
    let
      email =
        Validatable.bindR EmailAddress.create emailString
    in
    { model with Email = email }, Cmd.none

  | SetPassword passwordString ->
    let
      password =
        Validatable.bindR Password.create passwordString
    in
    ( { model with Password = password }, Cmd.none)

  | SetRepeatPassword repeatPasswordString ->
    let
      repeatPassword =
        Validatable.bindR (Model.checkRepeatPassword model) repeatPasswordString
    in
    ( { model with RepeatPassword = repeatPassword }, Cmd.none )

  | SwapPasswordHidden ->
    let
      passwordHidden =
        not model.PasswordHidden
    in
    ( { model with PasswordHidden = passwordHidden }, Cmd.none )

  | SwapPasswordRepeatHidden ->
    let
      repeatPasswordHidden =
        not model.PasswordHidden
    in
    ( { model with PasswordRepeatHidden = repeatPasswordHidden }, Cmd.none )

  | SignUp ->
    match Model.toDto model with
    | Some request ->
      let
        cmd =
          Cmd.batch [
            Loader.start ()

            Cmd.ofAsyncMsg <|
              async {
                let! response = Http.signUp request
                return SignedUp response
              }
          ]
      in
      ( model, cmd )

    | None ->
      ( Model.revalidate model, Cmd.none )

  | SignIn ->
    ( model, Route.push Route.SignIn )

  | SignedUp (Ok _) ->
    let
      emailString =
        Validatable.value EmailAddress.value model.Email
    let
      cmd =
        Cmd.batch [
          Route.SignUpSuccess emailString
          |> Route.push

          Loader.stop ()
        ]
    in
    ( model, cmd )

  | SignedUp (Error errors) ->
    let
      cmd =
        Cmd.batch [
          Loader.stop ()

          Message.showErrors errors
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
        map = NameString.value,
        value = model.FirstName,
        image = Images.userIcon,
        placeholder = "First name",
        textChanged = bindNewText dispatch SetFirstName
      )

      View.MakeEntry(
        map = NameString.value,
        value = model.LastName,
        image = Images.userIcon,
        placeholder = "Last name",
        textChanged = bindNewText dispatch SetLastName
      )

      View.MakeEntry(
        map = EmailAddress.value,
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
            bindClick dispatch SwapPasswordHidden
          )
      in
      View.MakeEntry(
        map = Password.value,
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
            bindClick dispatch SwapPasswordRepeatHidden
          )
      in
      View.MakeEntry(
        map = id,
        value = model.RepeatPassword,
        image = Images.lockIcon,
        margin = Thicknesses.mediumLowerSpace,
        placeholder = "Re-enter password",
        textChanged = bindNewText dispatch SetRepeatPassword,
        passwordOptions = passwordOptions
      )

      View.MakeButton(
        text = "sign up",
        margin = Thicknesses.mediumLowerSpace,
        command = bindClick dispatch SignUp,
        isEnabled = Model.isValid model
      )

      View.MakeTextButton(
        text = "already registered?",
        margin = Thicknesses.mediumLowerSpace,
        command = bindClick dispatch SignIn
      )
    ]
  )
