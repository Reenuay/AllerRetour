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
  private
  | SetEmail of string
  | SetPassword of string
  | TogglePasswordHidden
  | SignIn
  | SignUp
  | ForgotPassword
  | SignedIn of Http.Result<SignInResponse>
  | ProfileReceived of SignInResponse * Http.Result<ProfileResponse>

let initModel = {
  Email = emptyString
  Password = emptyString
  PasswordHidden = true
}

let update msg model : Model * Cmd<Msg> =
  match msg with
  | SetEmail e ->
    ( { model with Email = adaptV EmailAddress.create e }, Cmd.none )

  | SetPassword p ->
    ( { model with Password = adaptV Password.create p }, Cmd.none )

  | TogglePasswordHidden ->
    ( { model with PasswordHidden = not model.PasswordHidden }, Cmd.none )

  | SignIn ->
    match model.ToDto() with
    | Some d ->
      let
        cmd =
          async {
            let! tokenR = Http.signIn d
            return SignedIn tokenR
          }
          |> Cmd.ofAsyncMsg
      in
      ( model, cmd )

    | None ->
      ( model.Revalidate (), Cmd.none )

  | SignUp ->
    ( model, Route.push Route.SignUp )

  | ForgotPassword ->
    ( model, Route.push Route.ForgotPassword )

  | SignedIn (Success token) ->
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
          model.Email
          |> underV EmailAddress.value
          |> Route.ResendEmail
          |> Route.push
    in
    ( model, cmd )

  | ProfileReceived ( token, Success profile ) ->
    let
      cmd =
        Route.Main ( token, profile )
        |> Route.push
    in
    ( model, cmd )

  | SignedIn (Failure errors)
  | ProfileReceived ( _, Failure errors ) ->
    ( model, AppMessage.show <| foldErrors errors )

let view model dispatch =
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

      View.MakeThinText(
        text = "save on shopping\nsimply and tastefully",
        margin = Thicknesses.bigLowerSpace
      )

      View.MakeThinText("login with email")

      View.MakeEntry(
        model.Email,
        "Email",
        EmailAddress.value,
        bindNewText dispatch SetEmail,
        keyboard = Keyboard.Email,
        image = Images.envelopeIcon
      )

      View.MakeEntry(
        model.Password,
        "Password",
        Password.value,
        bindNewText dispatch SetPassword,
        image = Images.lockIcon,
        passwordOptions = ( model.PasswordHidden, bindPress dispatch TogglePasswordHidden ),
        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeButton(
        text = "log in",
        command = bindPress dispatch SignIn,
        isEnabled = model.IsValid (),
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
            command = bindPress dispatch ForgotPassword,
            margin = Thickness ( 0., -8., 0., 0. ),
            horizontalOptions = LayoutOptions.Start
          )
            .Column(0)

          View.MakeTextButton(
            text = "sign up",
            command = bindPress dispatch SignUp,
            fontFamily = Fonts.renogare,
            horizontalOptions = LayoutOptions.End
          )
            .Column(1)
        ]
      )
    ]
  )
