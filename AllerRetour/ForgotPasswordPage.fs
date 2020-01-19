module AllerRetour.ForgotPasswordPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model = {
  Email: string
}

type Msg =
  | SetEmail of string
  | ClickSend
  | ClickGoToSignIn

type ExternalMsg =
  | NoOp
  | Send
  | GoToSignIn

let initModel = {
  Email = ""
}

let update msg (model: Model) =
  match msg with
  | SetEmail e -> { model with Email = e }, NoOp
  | ClickSend -> model, Send
  | ClickGoToSignIn -> model, GoToSignIn

let view model dispatch =
  View.ContentPage(
    content = View.StackLayout(
      padding = Thickness 20.0,
      verticalOptions = LayoutOptions.Center,
      children = [
        View.Entry(
          text = model.Email,
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
