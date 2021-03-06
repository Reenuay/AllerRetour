namespace AllerRetour

type ResultBuilder () =
  member _.Bind(x, f) =
    match x with
    | Ok s -> f s
    | Error f -> Error f
  member _.Return(x) = Ok x
  member _.ReturnFrom(x) = x

[<AutoOpen>]
module ResultBuilder =
  let result = ResultBuilder()

module Result =
  let either fOk fError = function
  | Ok r ->
    fOk r

  | Error r ->
    fError r
