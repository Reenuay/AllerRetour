module AllerRetour.SignInPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open TwoTrackResult

type Model = {
  Email: Validatable<EmailAddress, string>
  Password: Validatable<Password, string>
}
with
  member this.ToDto() : EmailAndPassword option =
    match this.Email, this.Password with
    | Success e, Success p ->
      Some { Email = EmailAddress.value e; Password = Password.value p }
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
  | ClickSignIn
  | ClickGoToSignUp
  | ClickToForgotPassword

type ExternalMsg =
  | NoOp
  | SignIn of EmailAndPassword
  | GoToSignUp
  | GoToForgotPassword

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
  | ClickSignIn ->
    match model.ToDto() with
    | Some d -> model, SignIn d
    | None -> model.Revalidate(), NoOp
  | ClickGoToSignUp ->
    model, GoToSignUp
  | ClickToForgotPassword ->
    model, GoToForgotPassword

let view (model: Model) dispatch =    
  View.ContentPage(
    content = View.StackLayout(
      padding = Thickness 20.0,
      verticalOptions = LayoutOptions.Center,
      children = [
        yield View.Label(text = "Aller Retour")
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
          text = "Sign In",
          isEnabled = model.IsValid(),
          command = (fun () -> dispatch ClickSignIn))
        yield View.Button(
          text = "Not registered?",
          command = (fun () -> dispatch ClickGoToSignUp))
        yield View.Button(
          text = "Forgot password?",
          command = (fun () -> dispatch ClickToForgotPassword))
      ]
    )
  )
