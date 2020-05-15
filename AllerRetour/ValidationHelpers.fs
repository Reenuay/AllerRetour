[<AutoOpen>]
module AllerRetour.ValidationHelpers

type Validatable<'a, 'b> = Result<'a, 'b * string list>

let emptyString = Error ("", [])

let foldErrors = List.fold (fun s v -> s + "\n" + v) ""

let adaptV f x =
  match f x with
  | Ok s -> Ok s
  | Error l -> Error (x, l)

let underV fSuccess = function
| Ok x -> fSuccess x
| Error (v, _) -> v
