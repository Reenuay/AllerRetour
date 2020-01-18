module AllerRetour.ChangePasswordPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model = {
  NewPassword: string
  RepeatNewPassword: string
  OldPassword: string
}

type Msg =
  | SetNewPassword of string
  | SetRepeatNewPassword of string
  | SetOldPassword of string
  | ClickChange

type ExternalMsg =
  | NoOp
  | ChangePassword

let initModel = {
  NewPassword = ""
  RepeatNewPassword = ""
  OldPassword = ""
}

let update msg (model: Model) =
  match msg with
  | SetNewPassword p -> { model with NewPassword = p }, NoOp
  | SetRepeatNewPassword p -> { model with RepeatNewPassword = p }, NoOp
  | SetOldPassword p -> { model with OldPassword = p }, NoOp
  | ClickChange -> model, ChangePassword

let view model dispatch =
  View.ContentPage(
    content = View.StackLayout(
      children = [
        View.Entry(
          text = model.NewPassword,
          placeholder = "New password",
          isPassword = true,
          textChanged = (fun args -> dispatch (SetNewPassword args.NewTextValue)))
        View.Entry(
          text = model.RepeatNewPassword,
          placeholder = "Repeat new password",
          isPassword = true,
          textChanged = (fun args -> dispatch (SetNewPassword args.NewTextValue)))
        View.Entry(
          text = model.OldPassword,
          placeholder = "Old password",
          isPassword = true,
          textChanged = (fun args -> dispatch (SetOldPassword args.NewTextValue)))
        View.Button(
          text = "Change password",
          command = (fun () -> dispatch ClickChange))
      ]
    )
  )
