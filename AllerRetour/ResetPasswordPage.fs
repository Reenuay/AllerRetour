module AllerRetour.ResetPasswordPage

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open Views

type Model = {
  NewPassword: Validatable<Password, string>
  RepeatNewPassword: Validatable<string, string>
  Token: Validatable<Pin, string>
  Email: string
  Timer: int
}
with
  member this.CheckRepeatPassword(r) =
    match this.NewPassword with
    | Success p when Password.value p <> r ->
      Failure ["Passwords must be the same"]
    | _ ->
      Success r

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

type ExternalMsg =
  | NoOp
  | Timer
  | ResetPassword of PasswordResetRequest
  | GoToForgotPassword

let initModel email =
  let fifteenMinutes = 15 * 60

  {
    NewPassword = emptyString
    RepeatNewPassword = emptyString
    Token = emptyString
    Email = email
    Timer = fifteenMinutes
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
    let time = model.Timer - 1
    { model with Timer = time },
    if time > 0 then Timer else GoToForgotPassword

  | ClickReset ->
    match model.ToDto() with
    | Some d -> model, ResetPassword d
    | None -> model.Revalidate(), NoOp

let view model dispatch =
  View.ContentPage(
    content = View.StackLayout(
      padding = Thickness 20.0,
      children = [
        View.Label(
          text = """
            We sent you a verification code on your email.
            It will be valid until timer stops.
            Use it to reset your password."""
        )

        View.Label(
          text = TimeSpan.FromSeconds(float model.Timer).ToString("mm\:ss")
        )
        
        makeEntry
          None
          None
          "New password"
          None
          Password.value
          (fun args -> dispatch (SetNewPassword args.NewTextValue))
          model.NewPassword
        
        makeEntry
          None
          None
          "Repeat new password"
          None
          id
          (fun args -> dispatch (SetRepeatNewPassword args.NewTextValue))
          model.RepeatNewPassword
        
        makeEntry
          None
          None
          "Code"
          None
          Pin.value
          (fun args -> dispatch (SetToken args.NewTextValue))
          model.Token

        View.Button(
          text = "Reset password",
          command = (fun () -> dispatch ClickReset)
        )
      ]
    )
  )
