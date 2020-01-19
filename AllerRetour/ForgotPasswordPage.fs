module AllerRetour.ForgotPasswordPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model = string

type Msg =
  | SetEmail of string
  | ClickSend
  | ClickGoToSignIn

type ExternalMsg =
  | NoOp
  | Send
  | GoToSignIn

let update msg (model: Model) =
  match msg with
  | SetEmail e -> e, NoOp
  | ClickSend -> model, Send
  | ClickGoToSignIn -> model, GoToSignIn

let view (model: Model) dispatch =
  View.ContentPage(
    content = View.StackLayout(
      padding = Thickness 20.0,
      verticalOptions = LayoutOptions.Center,
      children = [
        View.Entry(
          text = model,
          placeholder = "Email",
          textChanged = (fun args -> dispatch (SetEmail args.NewTextValue)))
        View.Button(
          text = "Send",
          command = (fun () -> dispatch ClickSend))
        View.Button(
          text = "Return to sign in",
          command = (fun () -> dispatch ClickGoToSignIn))
      ]
    )
  )
