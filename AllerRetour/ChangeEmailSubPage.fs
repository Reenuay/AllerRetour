module AllerRetour.ChangeEmailSubPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open RequestTypes
open Views
open Resources

type Model = {
  Email: Validatable<EmailAddress, string>
  PreviousEmail: EmailAddress
  Password: Validatable<Password, string>
  PasswordHidden: bool
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
  | SwapPasswordHidden
  | ClickChange
  | ClickGoBack

type ExternalMsg =
  | NoOp
  | ChangeEmail of EmailAndPassword
  | GoBack

let create email = {
  Email = emptyString
  PreviousEmail = email
  Password = emptyString
  PasswordHidden = true
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

  | SwapPasswordHidden ->
    { model with PasswordHidden = not model.PasswordHidden }, NoOp

  | ClickGoBack ->
    model, GoBack

let view (model: Model) dispatch =
  makeScrollStack LayoutOptions.StartAndExpand [
    makeBackButton (bindPress dispatch ClickGoBack)

    Images.passwordChange
    |> makeCircle
    |> margin Thicknesses.mediumUpperBigLowerSpace

    makeEntry
      None
      (Some Keyboard.Email)
      "New email"
      None
      EmailAddress.value
      (bindNewText dispatch SetEmail)
      model.Email
        
    makeEntry
      (Some (model.PasswordHidden, bindPress dispatch SwapPasswordHidden))
      None
      "Password"
      None
      Password.value
      (bindNewText dispatch SetPassword)
      model.Password
    |> margin Thicknesses.mediumLowerSpace

    makeButton
      (model.IsValid())
      (bindPress dispatch ClickChange)
      "change"
    |> margin Thicknesses.mediumLowerSpace
  ]
