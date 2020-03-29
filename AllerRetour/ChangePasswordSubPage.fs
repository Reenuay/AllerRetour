module AllerRetour.ChangePasswordSubPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open Resources
open Views

type Model = {
  NewPassword: Validatable<Password, string>
  RepeatNewPassword: Validatable<string, string>
  OldPassword: Validatable<Password, string>
  NewPasswordHidden: bool
  RepeatNewPasswordHidden: bool
  OldPasswordHidden: bool
}
with
  member this.CheckRepeatPassword(r) =
    match this.NewPassword with
    | Success p when Password.value p <> r ->
      Failure ["Passwords must be the same"]
    | _ ->
      Success r

  member this.ToDto() : ChangePasswordRequest option =
    match this.NewPassword, this.RepeatNewPassword, this.OldPassword with
    | Success n, Success _, Success o ->
      Some {
        NewPassword = Password.value n
        OldPassword = Password.value o
      }
    | _ ->
      None

  member this.IsValid() =
    match this.NewPassword, this.RepeatNewPassword, this.OldPassword with
    | Success _, Success _, Success _ -> true
    | _ -> false

  member this.Revalidate() = {
    this with
      NewPassword = adaptV Password.create (underV Password.value this.NewPassword)
      RepeatNewPassword = adaptV this.CheckRepeatPassword (underV id this.RepeatNewPassword)
      OldPassword = adaptV Password.create (underV Password.value this.OldPassword)
  }

type Msg =
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

let initModel = {
  NewPassword = emptyString
  RepeatNewPassword = emptyString
  OldPassword = emptyString
  NewPasswordHidden = true
  RepeatNewPasswordHidden = true
  OldPasswordHidden = true
}

let update msg (model: Model) =
  match msg with
  | SetNewPassword p ->
    { model with NewPassword = adaptV Password.create p }, NoOp

  | SetRepeatNewPassword p ->
    { model with RepeatNewPassword = adaptV model.CheckRepeatPassword p }, NoOp

  | SetOldPassword p ->
    { model with OldPassword = adaptV Password.create p }, NoOp

  | SwapNewPasswordHidden ->
    { model with NewPasswordHidden = not model.NewPasswordHidden }, NoOp

  | SwapRepeatNewPasswordHidden ->
    { model with RepeatNewPasswordHidden = not model.RepeatNewPasswordHidden }, NoOp

  | SwapOldPasswordHidden ->
    { model with OldPasswordHidden = not model.OldPasswordHidden }, NoOp

  | ClickChange ->
    match model.ToDto() with
    | Some d -> model, ChangePassword d
    | None -> model.Revalidate(), NoOp

  | ClickGoBack ->
    model, GoBack

let view model dispatch =
  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    verticalOptions = LayoutOptions.StartAndExpand,
    children = [
      makeBackButton (bindPress dispatch ClickGoBack)

      Images.passwordChange
      |> makeCircle
      |> margin Thicknesses.mediumUpperBigLowerSpace

      makeEntry
        (Some (model.NewPasswordHidden, bindPress dispatch SwapNewPasswordHidden))
        None
        "New password"
        None
        Password.value
        (bindNewText dispatch SetNewPassword)
        model.NewPassword
        
      makeEntry
        (Some (model.RepeatNewPasswordHidden, bindPress dispatch SwapRepeatNewPasswordHidden))
        None
        "Repeat new password"
        None
        id
        (bindNewText dispatch SetRepeatNewPassword)
        model.RepeatNewPassword
        
      makeEntry
        (Some (model.OldPasswordHidden, bindPress dispatch SwapOldPasswordHidden))
        None
        "Old password"
        None
        Password.value
        (bindNewText dispatch SetOldPassword)
        model.OldPassword
      |> margin Thicknesses.mediumLowerSpace

      makeButton
        (model.IsValid())
        (bindPress dispatch ClickChange)
        "change"
      |> margin Thicknesses.mediumLowerSpace
    ]
  )
