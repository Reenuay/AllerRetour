[<AutoOpen>]
module AllerRetour.PrimitiveTypes

open TwoTrackResult
open ValidationPredicates

type Gender = Male | Female

type EmailAddress = private EmailAddress of string

type Password = private Password of string

type NameString = private NameString of string

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
