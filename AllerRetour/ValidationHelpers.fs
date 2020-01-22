[<AutoOpen>]
module AllerRetour.ValidationHelpers

let emptyString = Failure ("", [])

let foldErrors = List.fold (fun s v -> s + "\n" + v) ""

let adaptV f x =
  match f x with
  | Success s -> Success s
  | Failure l -> Failure (x, l)
