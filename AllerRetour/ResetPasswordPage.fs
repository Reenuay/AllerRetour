module AllerRetour.ResetPasswordPage

open System
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
    match Validatable.value Password.value this.NewPassword with
    | x when x <> "" && x = r -> Ok r
    | _ -> Error ["Passwords must be the same"]

  member this.ToDto() : PasswordResetRequest option =
    match this.NewPassword, this.RepeatNewPassword, this.Token with
    | Ok n, Ok _, Ok p ->
      Some {
        Email = this.Email
        NewPassword = Password.value n
        Token = Pin.value p
      }
    | _ ->
      None

  member this.IsValid() =
    match this.NewPassword, this.RepeatNewPassword, this.Token with
    | Ok _, Ok _, Ok _ -> true
    | _ -> false

  member this.Revalidate() = {
    this with
      NewPassword = Validatable.bindR Password.create (Validatable.value Password.value this.NewPassword)
      RepeatNewPassword = Validatable.bindR this.CheckRepeatPassword (Validatable.value id this.RepeatNewPassword)
      Token = Validatable.bindR Pin.create (Validatable.value Pin.value this.Token)
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
    NewPassword = Validatable.emptyString
    RepeatNewPassword = Validatable.emptyString
    Token = Validatable.emptyString
    Email = email
    Timer = fifteenMinutes
    TokenEntered = false
    NewPasswordHidden = true
    RepeatNewPasswordHidden = true
  }

let update msg (model: Model) =
  match msg with
  | SetNewPassword p ->
    { model with NewPassword = Validatable.bindR Password.create p }, NoOp

  | SetRepeatNewPassword p ->
    { model with RepeatNewPassword = Validatable.bindR model.CheckRepeatPassword p }, NoOp

  | SetToken p ->
    { model with Token = Validatable.bindR Pin.create p }, NoOp

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
      if Result.isOk model.Token then
        { model with TokenEntered = true }
      else
        { model with Token = Validatable.bindR Pin.create (Validatable.value Pin.value model.Token) }
    ), NoOp

  | ClickGoToSignIn ->
    model, GoToSignIn
    

let view model dispatch =
  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    children = [
      if not model.TokenEntered then
        yield! [
          View.MakeAvatar(
            source = Images.verificationCode,
            margin = Thicknesses.bigLowerSpace
          )

          View.MakeText("Please enter your verification code")

          View.MakeThinText("We sent a verification code\nto your registered email ID")

          View.MakeThinText(TimeSpan.FromSeconds(float model.Timer).ToString("mm\:ss"))

          View.MakeEntry(
            model.Token,
            "Code",
            Pin.value,
            (bindNewText dispatch SetToken),
            keyboard = Keyboard.Numeric,
            image = Images.lockIcon,
            margin = Thicknesses.mediumLowerSpace
          )

          View.MakeButton(
            text = "confirm",
            command = bindClick dispatch ClickConfirm,
            isEnabled = Result.isOk model.Token,
            margin = Thicknesses.mediumLowerSpace
          )
        ]
      else
        yield! [
          View.MakeAvatar(
            source = Images.passwordChange,
            margin = Thicknesses.mediumUpperBigLowerSpace
          )
        
          View.MakeText(
            text = "Please enter a new password",
            margin = Thicknesses.mediumLowerSpace
          )

          View.MakeEntry(
            model.NewPassword,
            "New password",
            Password.value,
            (bindNewText dispatch SetNewPassword),
            image = Images.lockIcon,
            passwordOptions = (model.NewPasswordHidden, bindClick dispatch SwapNewPasswordHidden)
          )

          View.MakeEntry(
            model.RepeatNewPassword,
            "Re-enter password",
            id,
            (bindNewText dispatch SetRepeatNewPassword),
            image = Images.lockIcon,
            passwordOptions = (model.RepeatNewPasswordHidden, bindClick dispatch SwapRepeatNewPasswordHidden),
            margin = Thicknesses.mediumLowerSpace
          )

          View.MakeButton(
            text = "change password",
            command = bindClick dispatch ClickReset,
            isEnabled = model.IsValid(),
            margin = Thicknesses.mediumLowerSpace
          )
        ]

      yield
        View.MakeTextButton(
          text = "log in",
          command = bindClick dispatch ClickGoToSignIn,
          margin = Thicknesses.mediumLowerSpace,
          fontFamily = Fonts.renogare
        )
    ]
  )
