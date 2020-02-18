module AllerRetour.ResetPasswordPage

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open Views
open Resources

type Model = {
  NewPassword: Validatable<Password, string>
  RepeatNewPassword: Validatable<string, string>
  Token: Validatable<Pin, string>
  Email: string
  Timer: int
  TokenEntered: bool
  NewPasswordHidden: bool
  RepeatNewPasswordHidden: bool
}
with
  member this.CheckRepeatPassword(r) =
    match underV Password.value this.NewPassword with
    | x when x <> "" && x = r -> Success r
    | _ -> Failure ["Passwords must be the same"]

  member this.ToDto() : PasswordResetRequest option =
    match this.NewPassword, this.RepeatNewPassword, this.Token with
    | Success n, Success _, Success p ->
      Some {
        Email = this.Email
        NewPassword = Password.value n
        Token = Pin.value p
      }
    | _ ->
      None

  member this.IsValid() =
    match this.NewPassword, this.RepeatNewPassword, this.Token with
    | Success _, Success _, Success _ -> true
    | _ -> false

  member this.Revalidate() = {
    this with
      NewPassword = adaptV Password.create (underV Password.value this.NewPassword)
      RepeatNewPassword = adaptV this.CheckRepeatPassword (underV id this.RepeatNewPassword)
      Token = adaptV Pin.create (underV Pin.value this.Token)
  }

type Msg =
  | SetNewPassword of string
  | SetRepeatNewPassword of string
  | SetToken of string
  | TimerTick
  | ClickReset
  | SetTokenEntered of bool
  | SwapNewPasswordHidden
  | SwapRepeatNewPasswordHidden
  | ClickConfirm
  | ClickGoToSignIn

type ExternalMsg =
  | NoOp
  | Timer
  | ResetPassword of PasswordResetRequest
  | GoToForgotPassword
  | GoToSignIn

let initModel email =
  let fifteenMinutes = 15 * 60

  {
    NewPassword = emptyString
    RepeatNewPassword = emptyString
    Token = emptyString
    Email = email
    Timer = fifteenMinutes
    TokenEntered = false
    NewPasswordHidden = true
    RepeatNewPasswordHidden = true
  }

let update msg (model: Model) =
  match msg with
  | SetNewPassword p ->
    { model with NewPassword = adaptV Password.create p }, NoOp

  | SetRepeatNewPassword p ->
    { model with RepeatNewPassword = adaptV model.CheckRepeatPassword p }, NoOp

  | SetToken p ->
    { model with Token = adaptV Pin.create p }, NoOp

  | TimerTick ->
    if model.TokenEntered then
      model, NoOp
    else
      let time = model.Timer - 1
      { model with Timer = time },
      if time > 0 then Timer else GoToForgotPassword

  | ClickReset ->
    match model.ToDto() with
    | Some d -> model, ResetPassword d
    | None -> model.Revalidate(), NoOp

  | SetTokenEntered e ->
    { model with TokenEntered = e }, NoOp

  | SwapNewPasswordHidden ->
    { model with NewPasswordHidden = not model.NewPasswordHidden }, NoOp

  | SwapRepeatNewPasswordHidden ->
    { model with RepeatNewPasswordHidden = not model.RepeatNewPasswordHidden }, NoOp

  | ClickConfirm ->
    (
      if TwoTrackResult.isSuccess model.Token then
        { model with TokenEntered = true }
      else
        { model with Token = adaptV Pin.create (underV Pin.value model.Token) }
    ), NoOp

  | ClickGoToSignIn ->
    model, GoToSignIn
    

let view model dispatch =
  let entered v1 v2 =
    if model.TokenEntered then v1 else v2

  makePage [
    yield
      makeCircle
        (View.Image(
          source = entered Images.passwordChange Images.verificationCode
        ))
      |> margin Thicknesses.mediumUpperBigLowerSpace

    if not model.TokenEntered then
      yield! [
        makeInfoText
          "Please enter your verification code"

        makeThinText "We sent a verification code\nto your registered email ID"

        makeThinText (TimeSpan.FromSeconds(float model.Timer).ToString("mm\:ss"))

        makeEntry
          None
          (Some Keyboard.Numeric)
          "Code"
          (Some Images.lockIcon)
          Pin.value
          (bindNewText dispatch SetToken)
          model.Token
        |> margin (Thicknesses.mediumLowerSpace)

        makeButton
          (TwoTrackResult.isSuccess model.Token)
          (bindPress dispatch ClickConfirm)
          "confirm"
        |> margin (Thicknesses.mediumLowerSpace)
      ]
    else
      yield! [
        makeInfoText
          "Please enter a new password"
        |> margin Thicknesses.mediumLowerSpace

        makeEntry
          (Some (model.NewPasswordHidden, bindPress dispatch SwapNewPasswordHidden))
          None
          "New password"
          (Some Images.lockIcon)
          Password.value
          (bindNewText dispatch SetNewPassword)
          model.NewPassword
        
        makeEntry
          (Some (model.RepeatNewPasswordHidden, bindPress dispatch SwapRepeatNewPasswordHidden))
          None
          "Re-enter password"
          (Some Images.lockIcon)
          id
          (bindNewText dispatch SetRepeatNewPassword)
          model.RepeatNewPassword
        |> margin Thicknesses.mediumLowerSpace

        makeButton
          (model.IsValid())
          (bindPress dispatch ClickReset)
          "change password"
        |> margin Thicknesses.mediumLowerSpace
      ]

    yield
      makeNavButton
        (bindPress dispatch ClickGoToSignIn)
        "log in"
      |> margin Thicknesses.mediumLowerSpace
  ]
