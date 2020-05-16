module AllerRetour.SignInPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open ResponseTypes
open Resources
open Views

type Model = private {
  Email: Validatable<EmailAddress, string>
  Password: Validatable<Password, string>
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
  | SignedIn of Http.Result<SignInResponse>
  | ProfileReceived of SignInResponse * Http.Result<ProfileResponse>

[<RequireQualifiedAccess>]
module Model =
  let toDto model =
    match ( model.Email, model.Password ) with
    | ( Ok e, Ok p ) ->
      Some
        {
          Email = EmailAddress.value e
          Password = Password.value p
        }

    | _ ->
      None

  let isValid model =
    match ( model.Email, model.Password ) with
    | ( Ok _, Ok _ ) ->
      true

    | _ ->
      false

  let revalidate (model: Model) =
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
    in
    {
      model with
        Email = email
        Password = password
    }

let initModel = {
  Email = Validatable.emptyString
  Password = Validatable.emptyString
  PasswordHidden = true
}

let update msg model : Model * Cmd<Msg> =
  match msg with
  | SetEmail e ->
    let
      email =
        Validatable.bindR EmailAddress.create e
    in
    ( { model with Email = email }, Cmd.none )

  | SetPassword p ->
    ( { model with Password = Validatable.bindR Password.create p }, Cmd.none )

  | TogglePasswordHidden ->
    ( { model with PasswordHidden = not model.PasswordHidden }, Cmd.none )

  | SignIn ->
    match Model.toDto model with
    | Some d ->
      let
        cmd =
          Cmd.batch [
            Loader.start ()

            Cmd.ofAsyncMsg <|
              async {
                let! tokenR = Http.signIn d
                return SignedIn tokenR
              }
          ]
      in
      ( model, cmd )

    | None ->
      ( Model.revalidate model, Cmd.none )

  | SignUp ->
    ( model, Route.push Route.SignUp )

  | ForgotPassword ->
    ( model, Route.push Route.ForgotPassword )

  | SignedIn (Ok token) ->
    let
      cmd =
        if
          token.EmailConfirmed
        then
          Cmd.ofAsyncMsg <|
            async {
              let! profileR = Http.getProfile token.Token
              return ProfileReceived ( token, profileR )
            }
        else
          let
            emailString =
              Validatable.value
                EmailAddress.value
                model.Email
          in
          Route.push <| Route.ResendEmail emailString
    in
    ( model, cmd )

  | ProfileReceived ( token, Ok profile ) ->
    let
      cmd =
        Cmd.batch [
          Route.Main ( token, profile )
          |> Route.push

          Loader.stop ()
        ]
    in
    ( model, cmd )

  | SignedIn (Error errors)
  | ProfileReceived ( _, Error errors ) ->
    ( model, AppMessage.show <| foldErrors errors )

let view model dispatch =
  View.MakeScrollStack(
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

      View.MakeThinText(
        text = "save on shopping\nsimply and tastefully",
        margin = Thicknesses.bigLowerSpace
      )

      View.MakeThinText("login with email")

      View.MakeEntry(
        map = EmailAddress.value,
        value = model.Email,
        image = Images.envelopeIcon,
        keyboard = Keyboard.Email,
        placeholder = "Email",
        textChanged = bindNewText dispatch SetEmail
      )

      View.MakeEntry(
        map = Password.value,
        value = model.Password,
        image = Images.lockIcon,
        margin = Thicknesses.mediumLowerSpace,
        placeholder = "Password",
        textChanged = bindNewText dispatch SetPassword,
        passwordOptions = ( model.PasswordHidden, bindClick dispatch TogglePasswordHidden )
      )

      View.MakeButton(
        text = "log in",
        command = bindClick dispatch SignIn,
        isEnabled = Model.isValid model,
        margin = Thicknesses.mediumLowerSpace
      )

      View.Grid(
        coldefs = [ Star; Star ],
        rowSpacing = 0.,
        columnSpacing = 0.,
        width = screenWidthP 0.8,
        margin = Thicknesses.mediumLowerSpace,
        horizontalOptions = LayoutOptions.CenterAndExpand,
        children = [
          View.MakeTextButton(
            text = "forgot password?",
            command = bindClick dispatch ForgotPassword,
            margin = Thickness ( 0., -8., 0., 0. ),
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
