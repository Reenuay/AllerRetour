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
  | SignIn
  | GoToSignUp

let initModel () = {
  Email = ""
  Password = ""
}

let update msg model =
  match msg with
  | SetEmail e -> { model with Email = e }
  | SetPassword e -> { model with Password = e }
  | _ -> model

let view model dispatch =
  View.StackLayout(
    padding = Thickness 20.0,
    verticalOptions = LayoutOptions.Center,
    children = [
      View.Label(text = "Aller Retour")
      View.Entry(text = model.Email, textChanged = (fun args -> dispatch (SetEmail args.NewTextValue)))
      View.Entry(text = model.Password, isPassword = true, textChanged = (fun args -> dispatch (SetPassword args.NewTextValue)))
      View.Button(text = "Sign In", command = (fun () -> dispatch SignIn))
      View.Button(text = "Not registered?", command = (fun () -> dispatch GoToSignUp))
    ]
  )
