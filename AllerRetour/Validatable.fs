[<AutoOpen>]
module AllerRetour.Validatable

type ValidationState<'a> =
  | Initial
  | Valid of 'a
  | Invalid of string list

type Validatable<'src, 'trg> =
  private
    {
      Input: 'src
      State: ValidationState<'trg>
    }

type Validatable<'trg> = Validatable<string, 'trg>

[<RequireQualifiedAccess>]
module Validatable =
  let empty input =
    {
      Input = input
      State = Initial
    }

  let emptyString =
    {
      Input = ""
      State = Initial
    }

  let bindR f input =
    let state =
      match f input with
      | Ok target ->
        Valid target

      | Error errors ->
        Invalid errors
    in
    {
      Input = input
      State = state
    }

  let getInput validatable =
    validatable.Input

  let getState validatable =
    validatable.State

  let tryValue validatable =
    match getState validatable with
    | Valid v ->
      Some v

    | _ ->
      None

  let isValid validatable =
    match validatable.State with
    | Valid _ ->
      true

    | _ ->
      false
