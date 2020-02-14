module AllerRetour.SignUpPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open Views
open Resources

type Model = {
  FirstName: Validatable<NameString, string>
  LastName: Validatable<NameString, string>
  Email: Validatable<EmailAddress, string>
  Password: Validatable<Password, string>
  RepeatPassword: Validatable<string, string>
}
with
  member this.CheckRepeatPassword(r) =
    match this.Password with
    | Success p when Password.value p <> r ->
      Failure ["Passwords must be the same"]
    | _ ->
      Success r

  member this.ToDto() : SignUpRequest option =
    match this.FirstName, this.LastName, this.Email, this.Password, this.RepeatPassword with
    | Success f, Success l, Success e, Success p, Success _ ->
      Some {
        FirstName = NameString.value f
        LastName = NameString.value l
        Email = EmailAddress.value e
        Password = Password.value p
      }
    | _ ->
      None

  member this.IsValid() =
    match this.FirstName, this.LastName, this.Email, this.Password, this.RepeatPassword with
    | Success _, Success _, Success _, Success _, Success _ -> true
    | _ -> false

  member this.Revalidate() = {
    this with
      FirstName = adaptV NameString.create (underV NameString.value this.FirstName)
      LastName = adaptV (NameString.create) (underV NameString.value this.LastName)
      Email = adaptV EmailAddress.create (underV EmailAddress.value this.Email)
      Password = adaptV Password.create (underV Password.value this.Password)
      RepeatPassword = adaptV this.CheckRepeatPassword (underV id this.RepeatPassword)
  }

type Msg =
  | SetFirstName of string
  | SetLastName of string
  | SetEmail of string
  | SetPassword of string
  | SetRepeatPassword of string
  | ClickSignUp
  | ClickGoToSignIn

type ExternalMsg =
  | NoOp
  | SignUp of SignUpRequest
  | GoToSignIn

let initModel = {
  FirstName = emptyString
  LastName = emptyString
  Email = emptyString
  Password = emptyString
  RepeatPassword = emptyString
}

let update msg (model: Model) =
  match msg with
  | SetFirstName f ->
    { model with FirstName = adaptV NameString.create f }, NoOp

  | SetLastName l ->
    { model with LastName = adaptV NameString.create l }, NoOp

  | SetEmail e ->
    { model with Email = adaptV EmailAddress.create e }, NoOp

  | SetPassword p ->
    { model with Password = adaptV Password.create p }, NoOp

  | SetRepeatPassword r ->
    { model with RepeatPassword = adaptV model.CheckRepeatPassword r }, NoOp

  | ClickSignUp ->
    match model.ToDto() with
    | Some d -> model, SignUp d
    | None -> model.Revalidate(), NoOp

  | ClickGoToSignIn ->
    model, GoToSignIn

let view model dispatch =
  View.ContentPage(
    content = View.StackLayout(
      padding = Thickness 20.0,
      verticalOptions = LayoutOptions.Center,
      children = [
        makeEntry
          None
          "First name"
          None
          NameString.value
          (fun args -> dispatch (SetFirstName args.NewTextValue))
          model.FirstName
        
        makeEntry
          None
          "Last name"
          None
          NameString.value
          (fun args -> dispatch (SetLastName args.NewTextValue))
          model.LastName
        
        makeEntry
          None
          "Email"
          (Some Images.envelopeIcon)
          EmailAddress.value
          (fun args -> dispatch (SetEmail args.NewTextValue))
          model.Email
        
        makeEntry
          None
          "Password"
          None
          Password.value
          (fun args -> dispatch (SetPassword args.NewTextValue))
          model.Password
        
        makeEntry
          None
          "Repeat password"
          None
          id
          (fun args -> dispatch (SetRepeatPassword args.NewTextValue))
          model.RepeatPassword

        View.Button(
          text = "Sign Up",
          isEnabled = model.IsValid(),
          command = (fun () -> dispatch ClickSignUp)
        )

        View.Button(
          text = "Already registered?",
          command = (fun () -> dispatch ClickGoToSignIn)
        )
      ]
    )
  )
