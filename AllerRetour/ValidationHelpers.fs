[<AutoOpen>]
module AllerRetour.ValidationHelpers

type Validatable<'a, 'b> = TwoTrackResult<'a, 'b * string list>

let emptyString = Failure ("", [])

let foldErrors = List.fold (fun s v -> s + "\n" + v) ""

let adaptV f x =
  match f x with
  | Success s -> Success s
  | Failure l -> Failure (x, l)

let underV fSuccess = function
| Success x -> fSuccess x
| Failure (v, _) -> v
