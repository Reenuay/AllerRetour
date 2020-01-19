module AllerRetour.ResendEmailPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model = string

type Msg =
  | ClickResendEmail
  | ClickGoToSignIn

type ExternalMsg =
  | ResendEmail of string
  | GoToSignIn

let update msg (model: Model) =
  match msg with
  | ClickResendEmail -> model, ResendEmail model
  | ClickGoToSignIn -> model, GoToSignIn

let view model dispatch =
  View.ContentPage(
    content = View.StackLayout(
      verticalOptions = LayoutOptions.Center,
      children = [
        View.Label(
          text = sprintf "Please confirm your email address %s to be able to sign in." model
            + "Check your inbox for email we sent."
            + "If it's not there please check spam folder."
            + "If it's still not there press resend email below.")
        View.Button(
          text = "Resend email",
          command = (fun () -> dispatch ClickResendEmail))
        View.Button(
          text = "Return to sign in page",
          command = (fun () -> dispatch ClickGoToSignIn))
      ]
    )
  )
