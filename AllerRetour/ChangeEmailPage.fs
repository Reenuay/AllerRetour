module AllerRetour.ChangeEmailPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model = EmailAndPassword

type Msg =
  | SetEmail of string
  | SetPassword of string
  | ClickChange
  | ClickGoToSignIn

type ExternalMsg =
  | NoOp
  | ChangeEmail of EmailAndPassword
  | GoToSignIn

let update msg (model: Model) =
  match msg with
  | SetEmail e -> { model with Email = e }, NoOp
  | SetPassword e -> { model with Password = e }, NoOp
  | ClickChange -> model, ChangeEmail model
  | ClickGoToSignIn -> model, GoToSignIn

let view (model: Model) dispatch =
  View.ContentPage(
    content = View.StackLayout(
      children = [
        View.Entry(
          text = model.Email,
          placeholder = "New email",
          textChanged = (fun args -> dispatch (SetEmail args.NewTextValue)))
        View.Entry(
          text = model.Password,
          placeholder = "Password",
          isPassword = true,
          textChanged = (fun args -> dispatch (SetPassword args.NewTextValue)))
        View.Button(
          text = "Change email",
          command = (fun () -> dispatch ClickChange))
        View.Button(
          text = "Return to sign in page",
          command = (fun () -> dispatch ClickGoToSignIn))
      ]
    )
  )
