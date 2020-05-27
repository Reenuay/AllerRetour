module AllerRetour.ChangePasswordSubPage

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
      NewPassword: Validatable<Password>
      RepeatNewPassword: Validatable<string>
      OldPassword: Validatable<Password>
      NewPasswordHidden: bool
      RepeatNewPasswordHidden: bool
      OldPasswordHidden: bool
    }

type Msg =
  private
  | SetNewPassword of string
  | SetRepeatNewPassword of string
  | SetOldPassword of string
  | SwapNewPasswordHidden
  | SwapRepeatNewPasswordHidden
  | SwapOldPasswordHidden
  | ClickChange
  | ClickGoBack

type ExternalMsg =
  | NoOp
  | ChangePassword of ChangePasswordRequest
  | GoBack

let initModel =
  {
    NewPassword = Validatable.emptyString
    RepeatNewPassword = Validatable.emptyString
    OldPassword = Validatable.emptyString
    NewPasswordHidden = true
    RepeatNewPasswordHidden = true
    OldPasswordHidden = true
  }

let update msg (model: Model) =
  match msg with
  | SetNewPassword passwordString ->
    let
      password =
        Password.validate passwordString
    in
    ( { model with NewPassword = password }, NoOp )

  | SetRepeatNewPassword passwordString ->
    let
      repeatPassword =
        Password.validateRepeat model.NewPassword passwordString
    in
    ( { model with RepeatNewPassword = repeatPassword }, NoOp )

  | SetOldPassword passwordString ->
    let
      password =
        Password.validate passwordString
    in
    ( { model with OldPassword = password }, NoOp )

  | SwapNewPasswordHidden ->
    { model with NewPasswordHidden = not model.NewPasswordHidden }, NoOp

  | SwapRepeatNewPasswordHidden ->
    { model with RepeatNewPasswordHidden = not model.RepeatNewPasswordHidden }, NoOp

  | SwapOldPasswordHidden ->
    { model with OldPasswordHidden = not model.OldPasswordHidden }, NoOp

  | ClickChange ->
    let
      fields =
        (
          Validatable.tryValue model.NewPassword,
          Validatable.tryValue model.RepeatNewPassword,
          Validatable.tryValue model.OldPassword
        )
    in
    match fields with
    | ( Some newPassword, Some _, Some oldPassword ) ->
      let
        data =
          {
            NewPassword = Password.value newPassword
            OldPassword = Password.value oldPassword
          }
      in
      ( model, ChangePassword data )

    | _ ->
      let
        newPassword =
          Password.revalidate model.NewPassword
      let
        repeatNewPassword =
          Password.revalidateRepeat model.NewPassword model.RepeatNewPassword
      let
        oldPassword =
          Password.revalidate model.OldPassword
      let
        newModel =
          { model with
              NewPassword = newPassword
              RepeatNewPassword = repeatNewPassword
              OldPassword = oldPassword
          }
      in
      ( newModel, NoOp )

  | ClickGoBack ->
    model, GoBack

let view model dispatch =
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
        model.NewPassword,
        "New password",
        (bindNewText dispatch SetNewPassword),
        image = Images.lockIcon,
        passwordOptions = (
          model.NewPasswordHidden,
          bindClick dispatch SwapNewPasswordHidden
        )
      )

      View.MakeEntry(
        model.RepeatNewPassword,
        "Re-enter new password",
        (bindNewText dispatch SetRepeatNewPassword),
        image = Images.lockIcon,
        passwordOptions = (
          model.RepeatNewPasswordHidden,
          bindClick dispatch SwapRepeatNewPasswordHidden
        )
      )

      View.MakeEntry(
        model.OldPassword,
        "Old password",
        (bindNewText dispatch SetOldPassword),
        image = Images.lockIcon,
        passwordOptions = (
          model.OldPasswordHidden,
          bindClick dispatch SwapOldPasswordHidden
        ),
        margin = Thicknesses.mediumLowerSpace
      )

      let
        isEnabled =
          Validatable.isValid model.OldPassword
          && Validatable.isValid model.NewPassword
          && Validatable.isValid model.RepeatNewPassword
      in
      View.MakeButton(
        text = "change",
        margin = Thicknesses.mediumLowerSpace,
        command = bindClick dispatch ClickChange,
        isEnabled = isEnabled
      )
    ]
  )
