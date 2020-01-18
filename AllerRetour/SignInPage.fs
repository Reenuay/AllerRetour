module AllerRetour.SignInPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model = {
  Email: string
  Password: string
}

type Msg =
  | SetEmail of string
  | SetPassword of string
  | ClickSignIn
  | ClickGoToSignUp

type ExternalMsg =
  | NoOp
  | SignIn
  | GoToSignUp

let initModel = {
  Email = ""
  Password = ""
}

let update msg (model: Model) =
  match msg with
  | SetEmail e -> { model with Email = e }, NoOp
  | SetPassword e -> { model with Password = e }, NoOp
  | ClickSignIn -> model, SignIn
  | ClickGoToSignUp -> model, GoToSignUp

let view model dispatch =
  View.ContentPage(
    content = View.StackLayout(
      padding = Thickness 20.0,
      verticalOptions = LayoutOptions.Center,
      children = [
        View.Label(text = "Aller Retour")
        View.Entry(
          text = model.Email,
          placeholder = "Email",
          textChanged = (fun args -> dispatch (SetEmail args.NewTextValue)))
        View.Entry(
          text = model.Password,
          placeholder = "Password",
          isPassword = true,
          textChanged = (fun args -> dispatch (SetPassword args.NewTextValue)))
        View.Button(
          text = "Sign In",
          command = (fun () -> dispatch ClickSignIn))
        View.Button(
          text = "Not registered?",
          command = (fun () -> dispatch ClickGoToSignUp))
      ]
    )
  )
