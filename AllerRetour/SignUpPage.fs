module AllerRetour.SignUpPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model = {
  FirstName: string
  LastName: string
  Email: string
  Password: string
  RepeatPassword: string
}

type Msg =
  | SetFirstName of string
  | SetLastName of string
  | SetEmail of string
  | SetPassword of string
  | SetRepeatPassword of string
  | ClickSignUp
  | ClickGoToSignIn

type ExternalMsg =
  | NoOp
  | SignUp
  | GoToSignIn

let initModel () = {
  FirstName = ""
  LastName = ""
  Email = ""
  Password = ""
  RepeatPassword = ""
}

let update msg model =
  match msg with
  | SetFirstName f -> { model with FirstName = f }, NoOp
  | SetLastName l -> { model with LastName = l }, NoOp
  | SetEmail e -> { model with Email = e }, NoOp
  | SetPassword p -> { model with Password = p }, NoOp
  | SetRepeatPassword p -> { model with RepeatPassword = p }, NoOp
  | ClickSignUp -> model, SignUp
  | ClickGoToSignIn -> model, GoToSignIn

let view model dispatch =
  View.ContentPage(
    content = View.StackLayout(
      padding = Thickness 20.0,
      verticalOptions = LayoutOptions.Center,
      children = [
        View.Label(text = "Aller Retour")
        View.Entry(
          text = model.FirstName,
          placeholder = model.FirstName,
          textChanged = (fun args -> dispatch (SetFirstName args.NewTextValue)))
        View.Entry(
          text = model.LastName,
          placeholder = model.LastName,
          textChanged = (fun args -> dispatch (SetLastName args.NewTextValue)))
        View.Entry(
          text = model.Email,
          textChanged = (fun args -> dispatch (SetEmail args.NewTextValue)))
        View.Entry(
          text = model.Password,
          isPassword = true,
          textChanged = (fun args -> dispatch (SetPassword args.NewTextValue)))
        View.Entry(
          text = model.RepeatPassword,
          isPassword = true,
          textChanged = (fun args -> dispatch (SetRepeatPassword args.NewTextValue)))
        View.Button(
          text = "Sign Up",
          command = (fun () -> dispatch ClickSignUp))
        View.Button(
          text = "Already registered?",
          command = (fun () -> dispatch ClickGoToSignIn))
      ]
    )
  )
