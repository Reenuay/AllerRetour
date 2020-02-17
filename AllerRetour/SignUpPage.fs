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
  PasswordHidden: bool
  PasswordRepeatHidden: bool
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
  | SwapPasswordHidden
  | SwapPasswordRepeatHidden
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
  PasswordHidden = true
  PasswordRepeatHidden = true
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

  | SwapPasswordHidden ->
    { model with PasswordHidden = not model.PasswordHidden }, NoOp

  | SwapPasswordRepeatHidden ->
    { model with PasswordRepeatHidden = not model.PasswordRepeatHidden }, NoOp

  | ClickSignUp ->
    match model.ToDto() with
    | Some d -> model, SignUp d
    | None -> model.Revalidate(), NoOp

  | ClickGoToSignIn ->
    model, GoToSignIn

let view model dispatch =
  makePage [
    makeLogo ()

    makeThinText "sign up with email"
    |> margin Thicknesses.bigUpperSpace

    makeEntry
      None
      None
      "First name"
      (Some Images.userIcon)
      NameString.value
      (bindNewText dispatch SetFirstName)
      model.FirstName
        
    makeEntry
      None
      None
      "Last name"
      (Some Images.userIcon)
      NameString.value
      (bindNewText dispatch SetLastName)
      model.LastName
        
    makeEntry
      None
      (Some Keyboard.Email)
      "Email"
      (Some Images.envelopeIcon)
      EmailAddress.value
      (bindNewText dispatch SetEmail)
      model.Email
        
    makeEntry
      (Some (model.PasswordHidden, bindPress dispatch SwapPasswordHidden))
      None
      "Password"
      (Some Images.lockIcon)
      Password.value
      (bindNewText dispatch SetPassword)
      model.Password
        
    makeEntry
      (Some (model.PasswordRepeatHidden, bindPress dispatch SwapPasswordRepeatHidden))
      None
      "Repeat password"
      (Some Images.lockIcon)
      id
      (bindNewText dispatch SetRepeatPassword)
      model.RepeatPassword
    |> margin (Thicknesses.mediumLowerSpace)

    makeButton
      (model.IsValid())
      (bindPress dispatch ClickSignUp)
      "sign up"
    |> margin (Thicknesses.mediumLowerSpace)

    makeLink
      (bindPress dispatch ClickGoToSignIn)
      "already registered?"
    |> horizontalOptions LayoutOptions.Center
  ]
