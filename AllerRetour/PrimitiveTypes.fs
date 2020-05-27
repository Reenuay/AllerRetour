module AllerRetour.PrimitiveTypes

open System
open System.Net.Mail
open System.Text.RegularExpressions

// -------------- GENDER
type Gender = Male | Female

[<RequireQualifiedAccess>]
module Gender =
  let fromString = function
  | "Male" -> Some Male
  | "Female" -> Some Female
  | _ -> None

  let fromStringOption = Option.bind fromString

  let toString = function
  | Male -> "Male"
  | Female -> "Female"

  let optionToString = function
  | Some g -> toString g
  | None -> ""

// -------------- EMAIL ADDRESS
type EmailAddress = private EmailAddress of string

[<RequireQualifiedAccess>]
module EmailAddress =
  let maxLength = 100

  let create e =
    if String.length e > maxLength then
      Error [sprintf "Maximum length is %i" maxLength]
    else if MailAddress(e).Address <> e then
      Error ["Bad format"]
    else
      Ok <| EmailAddress e

  let value (EmailAddress s) = s

  let validate =
    Validatable.bindR create

  let revalidate email =
    Validatable.getInput email
    |> validate

  let validateWithOld email =
    let
      checkEquals emailString =
        match create emailString with
        | Ok newEmail when newEmail = email ->
          Error ["This is the old value"]

        | x ->
          x
    in
    Validatable.bindR checkEquals

  let revalidateWithOld email (newEmail: Validatable<EmailAddress>) =
    Validatable.getInput newEmail
    |> validateWithOld email

// -------------- PASSWORD
type Password = private Password of string

[<RequireQualifiedAccess>]
module Password =
  let minLength = 8
  let maxLength = 100

  let private hasRestrictedWords p =
    Regex.IsMatch(
      p,
      "(aller|retour)",
      RegexOptions.IgnoreCase
    )

  let create p =
    if String.length p < minLength then
      Error [sprintf "Minimum length is %i" minLength]
    else if String.length p > maxLength then
      Error [sprintf "Maximum length is %i" maxLength]
    else if hasRestrictedWords p then
      Error ["App name is not allowed in password"]
    else
      Ok <| Password p

  let value (Password s) = s

  let validate =
    Validatable.bindR create

  let revalidate password =
    Validatable.getInput password
    |> validate

  let validateRepeat (password: Validatable<Password>) =
    let
      error =
        Error ["Passwords must be the same"]
    let
      checkEquals passwordString =
        if Validatable.getInput password = passwordString
        then Ok passwordString
        else error
    in
    match Validatable.getState password with
    | Initial ->
      Validatable.bindR (always error)

    | _ ->
      Validatable.bindR checkEquals

  let revalidateRepeat password (repeatPassword: Validatable<string>) =
    Validatable.getInput repeatPassword
    |> validateRepeat password

// -------------- NAME
type Name = private Name of string

[<RequireQualifiedAccess>]
module Name =
  let minLength = 1
  let maxLength = 100
  
  let create n =
    if String.length n < minLength then
      Error [sprintf "Minimum length is %i" minLength]
    else if String.length n > maxLength then
      Error [sprintf "Maximum length is %i" maxLength]
    else
      Ok <| Name n
  
  let value (Name s) = s

  let validate =
    Validatable.bindR create

  let revalidate name =
    Validatable.getInput name
    |> validate

// -------------- PIN
type Pin = private Pin of string

[<RequireQualifiedAccess>]
module Pin =
  let private isValid p =
    Regex.IsMatch(p, "\d{6}")

  let create p =
    if isValid p then
      Ok <| Pin p
    else
      Error ["Bad format"] 

  let value (Pin s) = s

  let validate =
    Validatable.bindR create

  let revalidate pin =
    Validatable.getInput pin
    |> validate

type EmailAndPassword = {
  Email: EmailAddress
  Password: Password
}

type Profile = {
  FirstName: Name
  LastName: Name
  Birthday: DateTime option
  Gender: Gender option
}
