module AllerRetour.Json

open Microsoft.FSharpLu.Json

let serialize x = Compact.Strict.serialize x

let inline deserialize x = Compact.Strict.tryDeserialize x
