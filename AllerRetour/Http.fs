module AllerRetour.Http

open System
open FSharp.Data

open RequestTypes
open ResponseTypes

type AsyncT<'a> = Async<TwoTrackResult<'a, string list>>

let baseUrl = "http://46.101.209.128/api/customer"
let routeTo route = baseUrl + route

let inline makeRequest (f: unit -> Async<HttpResponse>) =
  async {
    do! Async.SwitchToThreadPool()

    let! res = f ()
    let body =
      match res.Body with
      | Text t -> t
      | _ -> failwith "Binary data returned but text was expected."

    return
      if res.StatusCode = 200 then
        match Json.deserialize body with
        | Choice1Of2 t -> Success t
        | Choice2Of2 e -> Failure [ "Parsing error: server returned invalid data" ]
      else
        let e =
          match Json.deserialize body with
          | Choice1Of2 s -> s
          | Choice2Of2 s -> s
        Failure [ e ]
  }

let inline get route query headers =
  let f () =
    Http.AsyncRequest (
      routeTo route,
      httpMethod = "GET",
      headers = [ HttpRequestHeaders.Accept HttpContentTypes.Json ] @ headers,
      query = query,
      silentHttpErrors = true
    )

  makeRequest f

let inline post route body headers =
  let f () =
    Http.AsyncRequest (
      routeTo route,
      httpMethod = "POST",
      headers = [ HttpRequestHeaders.Accept HttpContentTypes.Json ] @ headers,
      body = TextRequest (Json.serialize body),
      silentHttpErrors = true
    )

  makeRequest f

let inline put route body headers =
  let f () =
    Http.AsyncRequest (
      routeTo route,
      httpMethod = "PUT",
      headers = [ HttpRequestHeaders.Accept HttpContentTypes.Json ] @ headers,
      body = TextRequest (Json.serialize body),
      silentHttpErrors = true
    )

  makeRequest f

let bearer token = HttpRequestHeaders.Authorization (sprintf "Bearer %s" token)

let signIn (r: SignInRequest) : AsyncT<SignInResponse> =
  post "/signin" r []

let signUp (r: SignUpRequest) : AsyncT<string> =
  post "/signup" r []

let sendPin (r: PasswordResetEmailRequest) : AsyncT<string> =
  post "/password/pin" r []

let resetPassword (r: PasswordResetRequest) : AsyncT<string> =
  post "/password/reset" r []

let getProfile token : AsyncT<ProfileResponse> =
  get "/profile" [] [ bearer token ]

let resendConfirmEmail token : AsyncT<string> =
  post "/email/resend" () [ bearer token ]

let updateProfile token (r: UpdateProfileRequest) : AsyncT<ProfileResponse> =
  put "/profile" r [ bearer token ]

let changeEmail token (r: ChangeEmailRequest) : AsyncT<string> =
  post "/email/change" r [ bearer token ]

let changePassword token (r: ChangePasswordRequest) : AsyncT<string> =
  post "/password/change" r [ bearer token ]
