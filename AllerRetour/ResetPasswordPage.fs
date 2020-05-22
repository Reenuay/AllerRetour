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
      Pin: Validatable<Pin, string>
      PinEntered: bool
      NewPassword: Validatable<Password, string>
      NewPasswordHidden: bool
      NewPasswordRepeat: Validatable<string, string>
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

[<RequireQualifiedAccess>]
module Model =
  let checkRepeatPassword model p =
    match Validatable.value Password.value model.NewPassword with
    | x when x <> "" && x = p ->
      Ok p

    | _ ->
      Error ["Passwords must be the same"]

  let toDto model =
    let
      fields =
        (
          model.NewPassword,
          model.NewPasswordRepeat,
          model.Pin
        )
    in
    match fields with
    | ( Ok n, Ok _, Ok p ) ->
      Some
        {
          Email = EmailAddress.value model.Email
          NewPassword = Password.value n
          Token = Pin.value p
        }

    | _ ->
      None

  let isValid model
    =  Validatable.isValid model.NewPassword
    && Validatable.isValid model.NewPasswordRepeat
    && Validatable.isValid model.Pin

  let revalidate model =
    let
      newPassword =
        Validatable.bindR
          Password.create
          (Validatable.value Password.value model.NewPassword)
    let
      repeatNewPassword =
        Validatable.bindR
          (checkRepeatPassword model)
          (Validatable.value id model.NewPasswordRepeat)
    let
      token =
        Validatable.bindR
          Pin.create
          (Validatable.value Pin.value model.Pin)
    in
    {
      model with
        NewPassword = newPassword
        NewPasswordRepeat = repeatNewPassword
        Pin = token
    }

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
              Validatable.bindR
                Pin.create
                (Validatable.value Pin.value model.Pin)
          in
          { model with Pin = pin }
    in
    ( newModel, Cmd.none )

  | SignIn ->
    ( model, Route.push Route.SignIn )

  | SetNewPassword newPasswordString ->
    let
      newPassword =
        Validatable.bindR Password.create newPasswordString
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
        Validatable.bindR
          (Model.checkRepeatPassword model)
          newPasswordRepeatString
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
    match Model.toDto model with
    | Some request ->
      let
        cmd =
          Cmd.batch
            [
              Cmd.ofAsyncMsg <|
                async {
                  let! response = Http.resetPassword request
                  return PasswordReset response
                }

              Loader.start
            ]
      in
      ( model, cmd )

    | None ->
      ( Model.revalidate model, Cmd.none )

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
            map = Pin.value,
            value = model.Pin,
            image = Images.lockIcon,
            margin = Thicknesses.mediumLowerSpace,
            keyboard = Keyboard.Numeric,
            placeholder = "Code",
            textChanged = bindNewText dispatch SetPin
          )

          View.MakeButton(
            text = "confirm",
            margin = Thicknesses.mediumLowerSpace,
            command = bindClick dispatch ConfirmPin,
            isEnabled = Result.isOk model.Pin
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
            map = Password.value,
            value = model.NewPassword,
            image = Images.lockIcon,
            placeholder = "New password",
            textChanged = bindNewText dispatch SetNewPassword,
            passwordOptions = passwordOptions
          )

          View.MakeEntry(
            model.NewPasswordRepeat,
            "Re-enter password",
            id,
            (bindNewText dispatch SetNewPasswordRepeat),
            image = Images.lockIcon,
            margin = Thicknesses.mediumLowerSpace,
            passwordOptions = (model.NewPasswordRepeatHidden, bindClick dispatch ToggleNewPasswordRepeatHidden)
          )

          View.MakeButton(
            text = "change password",
            margin = Thicknesses.mediumLowerSpace,
            command = bindClick dispatch ResetPassword,
            isEnabled = Model.isValid model
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
