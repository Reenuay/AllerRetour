module AllerRetour.ForgotPasswordPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes

type Model = Validatable<EmailAddress, string>

type Msg =
  | SetEmail of string
  | ClickSend
  | ClickGoToSignIn

type ExternalMsg =
  | NoOp
  | Send of string
  | GoToSignIn

let initModel = emptyString

let update msg (model: Model) =
  match msg with
  | SetEmail e -> adaptV EmailAddress.create e, NoOp
  | ClickSend ->
    match model with
    | Success e -> model, Send (EmailAddress.value e)
    | Failure x -> Failure x, NoOp
  | ClickGoToSignIn -> model, GoToSignIn

let view (model: Model) dispatch =
  View.ContentPage(
    content = View.StackLayout(
      padding = Thickness 20.0,
      verticalOptions = LayoutOptions.Center,
      children = [
        yield!
          makeEntry
            false
            "Email"
            EmailAddress.value
            (fun args -> dispatch (SetEmail args.NewTextValue))
            model
        yield View.Button(
          text = "Send",
          isEnabled = (match model with Success _ -> true | _ -> false),
          command = (fun () -> dispatch ClickSend))
        yield View.Button(
          text = "Return to sign in",
          command = (fun () -> dispatch ClickGoToSignIn))
      ]
    )
  )
