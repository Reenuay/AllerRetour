module AllerRetour.ChangePasswordSubPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model = {
  NewPassword: Validatable<Password, string>
  RepeatNewPassword: Validatable<string, string>
  OldPassword: Validatable<Password, string>
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
  | ClickChange

type ExternalMsg =
  | NoOp
  | ChangePassword of ChangePasswordRequest

let initModel = {
  NewPassword = emptyString
  RepeatNewPassword = emptyString
  OldPassword = emptyString
}

let update msg (model: Model) =
  match msg with
  | SetNewPassword p ->
    { model with NewPassword = adaptV Password.create p }, NoOp

  | SetRepeatNewPassword p ->
    { model with RepeatNewPassword = adaptV model.CheckRepeatPassword p }, NoOp

  | SetOldPassword p ->
    { model with OldPassword = adaptV Password.create p }, NoOp

  | ClickChange ->
    match model.ToDto() with
    | Some d -> model, ChangePassword d
    | None -> model.Revalidate(), NoOp

let view model dispatch =
  View.ContentPage(
    content = View.StackLayout(
      children = [
        yield!
          makeEntry
            true
            "New password"
            Password.value
            (fun args -> dispatch (SetNewPassword args.NewTextValue))
            model.NewPassword
        yield!
          makeEntry
            true
            "Repeat new password"
            id
            (fun args -> dispatch (SetRepeatNewPassword args.NewTextValue))
            model.RepeatNewPassword
        yield!
          makeEntry
            true
            "Old password"
            Password.value
            (fun args -> dispatch (SetOldPassword args.NewTextValue))
            model.OldPassword
        View.Button(
          text = "Change password",
          command = (fun () -> dispatch ClickChange))
      ]
    )
  )
