module AllerRetour.ChangeEmailSubPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes

type Model = {
  Email: Validatable<EmailAddress, string>
  PreviousEmail: EmailAddress
  Password: Validatable<Password, string>
}
with
  member this.ToDto() : EmailAndPassword option =
    match this.Email, this.Password with
    | Success e, Success p ->
      Some { Email = e; Password = p }
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
      if e = this.PreviousEmail then
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

type ExternalMsg =
  | NoOp
  | ChangeEmail of EmailAndPassword

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

let view (model: Model) dispatch =
  View.ContentPage(
    content = View.StackLayout(
      children = [
        yield!
          makeEntry
            false
            "Email"
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
      ]
    )
  )
