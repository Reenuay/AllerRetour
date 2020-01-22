[<AutoOpen>]
module AllerRetour.ViewHelpers

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

let makeEntry isPassword placeholder fSuccess dispatch v =
  let text, error =
    match v with
    | Success x -> fSuccess x, ""
    | Failure (v, l) -> v, foldErrors l

  [
    yield
      View.Entry(
        text = text,
        placeholder = placeholder,
        isPassword = isPassword,
        textChanged = (fun args -> dispatch args))
    yield!
      if error <> String.Empty then
        [
          View.Label(
            text = error,
            fontSize = FontSize 10.0)
        ]
      else
        []
  ]
