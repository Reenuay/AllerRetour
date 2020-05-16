namespace AllerRetour

type Validatable<'a, 'b> = Result<'a, 'b * string list>

[<RequireQualifiedAccess>]
module Validatable =
  let emptyString =
    Error ("", [])

  let bindR f x =
    match f x with
    | Ok s ->
      Ok s

    | Error l ->
      Error (x, l)

  let value f = function
  | Ok x ->
    f x

  | Error (v, _) ->
    v
