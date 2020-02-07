module AllerRetour.Http

open System
open FSharp.Data

open RequestTypes
open ResponseTypes

type AsyncT<'a> = Async<TwoTrackResult<'a, string list>>

let baseUrl = "http://46.101.209.128/api/customer"
let routeTo route = baseUrl + route

let inline makeRequest f =
  async {
    try
      let! res = f ()
      return
        match Json.deserialize res with
        | Choice1Of2 t -> Success t
        | Choice2Of2 e -> Failure [e]
    with
    | exn -> return Failure [exn.Message]
  }

let inline get route query headers =
  let f () =
    Http.AsyncRequestString (
      routeTo route,
      httpMethod = "GET",
      headers = [ HttpRequestHeaders.Accept HttpContentTypes.Json ] @ headers,
      query = query
    )

  makeRequest f

let inline post route body headers =
  let f () =
    Http.AsyncRequestString (
      routeTo route,
      httpMethod = "POST",
      headers = [ HttpRequestHeaders.Accept HttpContentTypes.Json ] @ headers,
      body = TextRequest (Json.serialize body)
    )

  makeRequest f

let inline put route body headers =
  let f () =
    Http.AsyncRequestString (
      routeTo route,
      httpMethod = "PUT",
      headers = [ HttpRequestHeaders.Accept HttpContentTypes.Json ] @ headers,
      body = TextRequest (Json.serialize body)
    )

  makeRequest f

let bearer token = HttpRequestHeaders.Authorization (sprintf "Bearer %s" token)

let signIn (r: SignInRequest) : AsyncT<SignInResponse> =
  post "/signin" r []

let signUp (r: SignUpRequest) : AsyncT<string> =
  post "/signup" r []

let getProfile token : AsyncT<ProfileResponse> =
  get "/profile" [] [ bearer token ]

let resendConfirmEmail token : AsyncT<string> =
  post "/email/resend" () [ bearer token ]

let updateProfile token (r: UpdateProfileRequest) : AsyncT<ProfileResponse> =
  put "/profile" r [ bearer token ]

let changeEmail token (r: ChangeEmailRequest) : AsyncT<string> =
  post "/email/change" r [ bearer token ]
