module AllerRetour.PrimitiveTypes

open System
open TwoTrackResult
open ValidationPredicates

type Gender = Male | Female

type EmailAddress = private EmailAddress of string

type Password = private Password of string

type NameString = private NameString of string

type Profile = {
  FirstName: NameString
  LastName: NameString
  Birthday: DateTime option
  Gender: Gender option
}

type EmailAndPassword = {
  Email: EmailAddress
  Password: Password
}

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

module EmailAddress =
  let maxLength = 100

  let create
    =  chain (isValidEmail) ["Bad format"]
    ++ chain (hasMaxLengthOf maxLength) [sprintf "Maximum length is %i" maxLength]
    >> map EmailAddress

  let value (EmailAddress s) = s

module Password =
  let minLength = 8
  let maxLength = 100
  let restrictedWords = ["aller"; "retour"]

  let create
    =  chain (hasMinLengthOf minLength) [sprintf "Minimum length is %i" minLength]
    ++ chain (hasMaxLengthOf maxLength) [sprintf "Maximum length is %i" maxLength]
    ++ chain (containsWords restrictedWords >> not) ["App name is not allowed in password"]
    >> map Password

  let value (Password s) = s

module NameString =
  let minLength = 1
  let maxLength = 100
  
  let create
    =  chain (hasMinLengthOf minLength) [sprintf "Minimum length is %i" minLength]
    ++ chain (hasMaxLengthOf maxLength) [sprintf "Maximum length is %i" maxLength]
    >> map NameString
  
  let value (NameString s) = s
