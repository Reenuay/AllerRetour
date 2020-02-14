module AllerRetour.ForgotPasswordPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open Views
open Resources

type Model = Validatable<EmailAddress, string>

type Msg =
  | SetEmail of string
  | ClickSend
  | ClickGoToSignIn

type ExternalMsg =
  | NoOp
  | Send of PasswordResetEmailRequest
  | GoToSignIn

let initModel = emptyString

let update msg (model: Model) =
  match msg with
  | SetEmail e -> adaptV EmailAddress.create e, NoOp
  | ClickSend ->
    match model with
    | Success e -> model, Send { Email = EmailAddress.value e }
    | Failure x -> Failure x, NoOp
  | ClickGoToSignIn -> model, GoToSignIn

let view (model: Model) dispatch =
  View.ContentPage(
    content = View.StackLayout(
      padding = Thickness 20.0,
      verticalOptions = LayoutOptions.Center,
      children = [
        makeEntry
          None
          "Email"
          (Some Images.envelopeIcon)
          EmailAddress.value
          (fun args -> dispatch (SetEmail args.NewTextValue))
          model

        View.Button(
          text = "Send",
          isEnabled = (match model with Success _ -> true | _ -> false),
          command = (fun () -> dispatch ClickSend)
        )

        View.Button(
          text = "Return to sign in",
          command = (fun () -> dispatch ClickGoToSignIn)
        )
      ]
    )
  )
