module AllerRetour.ChangeEmailSubPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open Views
open Resources
      
type Model =
  private
    {
      Email: Validatable<EmailAddress>
      OldEmail: EmailAddress
      Password: Validatable<Password>
      PasswordHidden: bool
    }

type Msg =
  | SetEmail of string
  | SetPassword of string
  | SwapPasswordHidden
  | ClickChange
  | ClickGoBack

type ExternalMsg =
  | NoOp
  | ChangeEmail of EmailAndPassword
  | GoBack

let create email = {
  Email = Validatable.emptyString
  OldEmail = email
  Password = Validatable.emptyString
  PasswordHidden = true
}

let update msg (model: Model) =
  match msg with
  | SetEmail emailString ->
    let
      email =
        EmailAddress.validateWithOld model.OldEmail emailString
    in
    ( { model with Email = email }, NoOp )

  | SetPassword passwordString ->
    let
      password =
        Password.validate passwordString
    in
    ( { model with Password = password }, NoOp )

  | ClickChange ->
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
        data : EmailAndPassword =
          {
            Email = email
            Password = password
          }
      in
      ( model, ChangeEmail data )
        
    | _ ->
      let
        email =
          EmailAddress.revalidateWithOld model.OldEmail model.Email
      let
        password =
          Password.revalidate model.Password
      let
        newModel =
          { model with
              Email = email
              Password = password
          }
      in
      ( newModel, NoOp )

  | SwapPasswordHidden ->
    { model with PasswordHidden = not model.PasswordHidden }, NoOp

  | ClickGoBack ->
    model, GoBack

let view (model: Model) dispatch =
  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    dispatchBack = bindClick dispatch ClickGoBack,
    verticalOptions = LayoutOptions.StartAndExpand,
    children = [
      View.MakeAvatar(
        source = Images.passwordChange,
        margin = Thicknesses.mediumUpperBigLowerSpace
      )

      View.MakeEntry(
        model.Email,
        "New email",
        (bindNewText dispatch SetEmail),
        keyboard = Keyboard.Email,
        image = Images.envelopeIcon
      )

      View.MakeEntry(
        model.Password,
        "Password",
        (bindNewText dispatch SetPassword),
        image = Images.lockIcon,
        passwordOptions = (model.PasswordHidden, bindClick dispatch SwapPasswordHidden),
        margin = Thicknesses.mediumLowerSpace
      )

      let
        isEnabled =
          Validatable.isValid model.Email
          && Validatable.isValid model.Password
      in
      View.MakeButton(
        text = "change",
        margin = Thicknesses.mediumLowerSpace,
        command = bindClick dispatch ClickChange,
        isEnabled = isEnabled
      )
    ]
  )
