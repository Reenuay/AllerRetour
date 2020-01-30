module AllerRetour.Http

open System
open FSharp.Data

open RequestTypes
open ResponseTypes

type AsyncT<'a> = Async<TwoTrackResult<'a, string list>>

let baseUrl = "http://46.101.209.128/api/customer"
let routeTo route = baseUrl + route

let inline get route query headers =
  async {
    try
      let! res
        = Http.AsyncRequestString
            (
              routeTo route,
              httpMethod="GET",
              headers = [ HttpRequestHeaders.Accept HttpContentTypes.Json ] @ headers,
              query = query
            )
      return
        match Json.deserialize res with
        | Choice1Of2 t -> Success t
        | Choice2Of2 e -> Failure [e]
    with
    | exn -> return Failure [exn.Message]
  }

let inline post route body headers =
  async {
    try
      let! res
        = Http.AsyncRequestString
            (
              routeTo route,
              httpMethod="POST",
              headers = [ HttpRequestHeaders.Accept HttpContentTypes.Json ] @ headers,
              body = TextRequest (Json.serialize body)
            )
      return
        match Json.deserialize res with
        | Choice1Of2 t -> Success t
        | Choice2Of2 e -> Failure [e]
    with
    | exn -> return Failure [exn.Message]
  }

let signIn (r: SignInRequest) : AsyncT<SignInResponse> =
  post "/signin" r []

let getProfile token : AsyncT<ProfileResponse> =
  get "/profile" [] [ HttpRequestHeaders.Authorization (sprintf "Bearer %s" token) ]