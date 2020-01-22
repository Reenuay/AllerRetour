module AllerRetour.SignInPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model = {
  Email: TwoTrackResult<EmailAddress, string * string list>
  Password: TwoTrackResult<Password, string * string list>
}
with
  member this.ToDto() : EmailAndPassword option =
    match this.Email, this.Password with
    | Success e, Success p -> Some { Email = EmailAddress.value e; Password = Password.value p }
    | _ ->
      None
  member this.IsValid =
    match this.Email, this.Password with
    | Success _, Success _ -> true
    | _ -> false

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
    model, match model.ToDto() with Some r -> SignIn r | None -> NoOp
  | ClickGoToSignUp ->
    model, GoToSignUp
  | ClickToForgotPassword ->
    model, GoToForgotPassword

let makeEntry fSuccess placeholder dispatch = function
| Success x ->
  [
    View.Entry(
      text = fSuccess x,
      placeholder = placeholder,
      textChanged = (fun args -> dispatch args))
  ]
| Failure (v, l) ->
  [
    View.Entry(
      text = v,
      placeholder = placeholder,
      textChanged = (fun args -> dispatch args))
    View.Label(
      text = foldErrors l,
      fontSize = FontSize 10.0)
  ]

let view (model: Model) dispatch =    
  View.ContentPage(
    content = View.StackLayout(
      padding = Thickness 20.0,
      verticalOptions = LayoutOptions.Center,
      children = [
        yield View.Label(text = "Aller Retour")
        yield!
          makeEntry
            EmailAddress.value
            "Email"
            (fun args -> dispatch (SetEmail args.NewTextValue))
            model.Email
        yield!
          makeEntry
            Password.value
            "Password"
            (fun args -> dispatch (SetPassword args.NewTextValue))
            model.Password
        yield View.Button(
          text = "Sign In",
          isEnabled = model.IsValid,
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
