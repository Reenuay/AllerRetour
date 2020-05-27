module AllerRetour.ResetPasswordPage

open Fabulous
open PrimitiveTypes
open RequestTypes
open Resources
open System
open Views
open Xamarin.Forms

type Model =
  private
    {
      Email: EmailAddress
      Timer: int
      Pin: Validatable<Pin>
      PinEntered: bool
      NewPassword: Validatable<Password>
      NewPasswordHidden: bool
      NewPasswordRepeat: Validatable<string>
      NewPasswordRepeatHidden: bool
    }

type Msg =
  private
  | SetPin of string
  | TimerTick
  | ConfirmPin
  | SignIn
  | SetNewPassword of string
  | ToggleNewPasswordHidden
  | SetNewPasswordRepeat of string
  | ToggleNewPasswordRepeatHidden
  | ResetPassword
  | PasswordReset of Http.Result<string>

let init email =
  let
    fifteenMinutes =
      15 * 60
  let
    model =
      {
        Email = email
        Timer = fifteenMinutes
        Pin = Validatable.emptyString
        PinEntered = false
        NewPassword = Validatable.emptyString
        NewPasswordHidden = true
        NewPasswordRepeat = Validatable.emptyString
        NewPasswordRepeatHidden = true
      }
  in
  ( model, Cmd.ofMsg TimerTick )

let update msg (model: Model) =
  match msg with
  | SetPin pinString ->
    let
      pin =
        Validatable.bindR Pin.create pinString
    in
    ( { model with Pin = pin }, Cmd.none )

  | TimerTick ->
    if model.PinEntered then
      ( model, Cmd.none )

    else
      let
        time =
          model.Timer - 1
      let
        cmd =
          if time > 0 then
            Cmd.ofAsyncMsg <|
              async {
                do! Async.SwitchToThreadPool()
                do! Async.Sleep 1000

                return TimerTick
              }

          else
            Cmd.batch
              [
                Route.push Route.ForgotPassword

                Message.show "Verification timed out!"
              ]
      in
      ( { model with Timer = time }, cmd )

  | ConfirmPin ->
    let
      newModel =
        if Validatable.isValid model.Pin then
          { model with PinEntered = true }

        else
          let
            pin =
              Pin.revalidate model.Pin
          in
          { model with Pin = pin }
    in
    ( newModel, Cmd.none )

  | SignIn ->
    ( model, Route.push Route.SignIn )

  | SetNewPassword newPasswordString ->
    let
      newPassword =
        Password.validate newPasswordString
    let
      newModel =
        {
          model with
            NewPassword = newPassword
        }
    in
    ( newModel, Cmd.none )

  | ToggleNewPasswordHidden ->
    let
      newPasswordHidden =
        not model.NewPasswordHidden
    let
      newModel =
        {
          model with
            NewPasswordHidden = newPasswordHidden
        }
    in
    ( newModel, Cmd.none )

  | SetNewPasswordRepeat newPasswordRepeatString ->
    let
      newPasswordRepeat =
        Password.validateRepeat model.NewPassword newPasswordRepeatString
    let
      newModel =
        {
          model with
            NewPasswordRepeat = newPasswordRepeat
        }
    in
    ( newModel, Cmd.none )

  | ToggleNewPasswordRepeatHidden ->
    let
      newPasswordRepeatHidden =
        not model.NewPasswordRepeatHidden
    let
      newModel =
        {
          model with
            NewPasswordRepeatHidden = newPasswordRepeatHidden
        }
    in
    ( newModel, Cmd.none )

  | ResetPassword ->
    let
      fields =
        (
          Validatable.tryValue model.NewPassword,
          Validatable.tryValue model.Pin,
          Validatable.tryValue model.NewPasswordRepeat
        )
    in
    match fields with
    | ( Some n, Some p, Some _ ) ->
      let
        req =
          {
            Email = EmailAddress.value model.Email
            NewPassword = Password.value n
            Token = Pin.value p
          }
      let
        cmd =
          Cmd.batch
            [
              Cmd.ofAsyncMsg <|
                async {
                  let! res = Http.resetPassword req
                  return PasswordReset res
                }

              Loader.start
            ]
      in
      ( model, cmd )

    | _ ->
      let
        newPassword =
          Password.revalidate model.NewPassword
      let
        repeatNewPassword =
          Password.revalidateRepeat model.NewPassword model.NewPasswordRepeat
      let
        pin =
          Pin.revalidate model.Pin
      let
        newModel =
          {
            model with
              NewPassword = newPassword
              NewPasswordRepeat = repeatNewPassword
              Pin = pin
          }
      in
      ( newModel, Cmd.none )

  | PasswordReset (Ok _) ->
    let
      cmd =
        Cmd.batch
          [
            Route.push Route.SignIn

            Loader.stop

            Message.show "Your password has been successfully reset!"
          ]
    in
    ( model, cmd )

  | PasswordReset (Error errors) ->
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
      if not model.PinEntered then
        yield! [
          View.MakeAvatar(
            source = Images.verificationCode,
            margin = Thicknesses.bigLowerSpace
          )

          View.MakeText(
            text = "Please enter your verification code"
          )

          View.MakeThinText(
            text = "We sent a verification code\nto your registered email ID"
          )

          View.MakeThinText(
            text =
              TimeSpan
                .FromSeconds(float model.Timer)
                .ToString("mm\:ss")
          )

          View.MakeEntry(
            value = model.Pin,
            image = Images.lockIcon,
            margin = Thicknesses.mediumLowerSpace,
            keyboard = Keyboard.Numeric,
            placeholder = "Code",
            textChanged = bindNewText dispatch SetPin
          )

          let
            isEnabled =
              Validatable.isValid model.Pin
          in
          View.MakeButton(
            text = "confirm",
            margin = Thicknesses.mediumLowerSpace,
            command = bindClick dispatch ConfirmPin,
            isEnabled = isEnabled
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

          let
            passwordOptions =
              (
                model.NewPasswordHidden,
                bindClick dispatch ToggleNewPasswordHidden
              )
          in
          View.MakeEntry(
            value = model.NewPassword,
            image = Images.lockIcon,
            placeholder = "New password",
            textChanged = bindNewText dispatch SetNewPassword,
            passwordOptions = passwordOptions
          )

          View.MakeEntry(
            model.NewPasswordRepeat,
            "Re-enter password",
            (bindNewText dispatch SetNewPasswordRepeat),
            image = Images.lockIcon,
            margin = Thicknesses.mediumLowerSpace,
            passwordOptions = (model.NewPasswordRepeatHidden, bindClick dispatch ToggleNewPasswordRepeatHidden)
          )

          let
            isEnabled =
              Validatable.isValid model.NewPassword
              && Validatable.isValid model.NewPasswordRepeat
              && Validatable.isValid model.Pin
          in
          View.MakeButton(
            text = "change password",
            margin = Thicknesses.mediumLowerSpace,
            command = bindClick dispatch ResetPassword,
            isEnabled = isEnabled
          )
        ]

      yield
        View.MakeTextButton(
          text = "log in",
          margin = Thicknesses.mediumLowerSpace,
          command = bindClick dispatch SignIn,
          fontFamily = Fonts.renogare
        )
    ]
  )
