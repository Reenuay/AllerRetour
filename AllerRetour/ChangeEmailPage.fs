module AllerRetour.ChangeEmailPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open Resources
open Views

type Model = {
  Email: Validatable<EmailAddress, string>
  PreviousEmail: string
  Password: Validatable<Password, string>
}
with
  member this.ToDto() : ChangeEmailRequest option =
    match this.Email, this.Password with
    | Success e, Success p ->
      Some { NewEmail = EmailAddress.value e; Password = Password.value p }
    | _ ->
      None

  member this.IsValid() =
    match this.Email, this.Password with
    | Success _, Success _ -> true
    | _ -> false

  member this.CreateEmail(email) =
    match EmailAddress.create email with
    | Failure x -> Failure x
    | Success e ->
      if EmailAddress.value e = this.PreviousEmail then
        Failure ["This is the old value"]
      else
        Success e

  member this.Revalidate() = {
    this with
      Email = adaptV this.CreateEmail (underV EmailAddress.value this.Email)
      Password = adaptV Password.create (underV Password.value this.Password)
  }

type Msg =
  | SetEmail of string
  | SetPassword of string
  | ClickChange
  | ClickGoToSignIn

type ExternalMsg =
  | NoOp
  | ChangeEmail of ChangeEmailRequest
  | GoToSignIn

let create email = {
  Email = emptyString
  PreviousEmail = email
  Password = emptyString
}

let update msg (model: Model) =
  match msg with
  | SetEmail e ->
    { model with Email = adaptV model.CreateEmail e }, NoOp

  | SetPassword p ->
    { model with Password = adaptV Password.create p }, NoOp

  | ClickChange ->
    match model.ToDto() with
    | Some d -> model, ChangeEmail d
    | None -> model.Revalidate(), NoOp

  | ClickGoToSignIn ->
    model, GoToSignIn

let view (model: Model) dispatch =
  View.ContentPage(
    content = View.StackLayout(
      children = [
        makeEntry
          None
          None
          "New email"
          (Some Images.envelopeIcon)
          EmailAddress.value
          (fun args -> dispatch (SetEmail args.NewTextValue))
          model.Email
        
        makeEntry
          None
          None
          "Password"
          None
          Password.value
          (fun args -> dispatch (SetPassword args.NewTextValue))
          model.Password

        View.Button(
          text = "Change email",
          command = (fun () -> dispatch ClickChange)
        )

        View.Button(
          text = "Return to sign in page",
          command = (fun () -> dispatch ClickGoToSignIn)
        )
      ]
    )
  )
