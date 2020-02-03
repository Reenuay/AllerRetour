module AllerRetour.ChangeEmailPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes

type Model = {
  Email: Validatable<EmailAddress, string>
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

  member this.Revalidate() = {
    this with
      Email = adaptV EmailAddress.create (underV EmailAddress.value this.Email)
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

let initModel = {
  Email = emptyString
  Password = emptyString
}

let update msg (model: Model) =
  match msg with
  | SetEmail e ->
    { model with Email = adaptV EmailAddress.create e }, NoOp

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
        yield!
          makeEntry
            false
            "New email"
            EmailAddress.value
            (fun args -> dispatch (SetEmail args.NewTextValue))
            model.Email
        yield!
          makeEntry
            true
            "Password"
            Password.value
            (fun args -> dispatch (SetPassword args.NewTextValue))
            model.Password
        yield View.Button(
          text = "Change email",
          command = (fun () -> dispatch ClickChange))
        yield View.Button(
          text = "Return to sign in page",
          command = (fun () -> dispatch ClickGoToSignIn))
      ]
    )
  )
