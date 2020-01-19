module AllerRetour.SignUpSuccessPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model = string

type Msg = ClickGoToSignIn

type ExternalMsg = GoToSignIn

let update msg (model: Model) =
  match msg with
  | ClickGoToSignIn -> model, GoToSignIn

let view model dispatch =
  View.ContentPage(
    content = View.StackLayout(
      verticalOptions = LayoutOptions.Center,
      children = [
        View.Label(
          text = "Success! We sent you an email.\n"
            + sprintf "Please check you email address %s for confirmation.\n" model
            + "If there is no email check spam folder.")
        View.Button(
          text = "Return to sign in page",
          command = (fun () -> dispatch ClickGoToSignIn))
      ]
    )
  )
