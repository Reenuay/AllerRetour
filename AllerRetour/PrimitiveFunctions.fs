[<AutoOpen>]
module AllerRetour.PrimitiveFunctions

let always x _ = x

let foldErrors =
  List.fold (fun s v -> s + "\n" + v) ""
