namespace AllerRetour

open System
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms
open TwoTrackResult
open RequestTypes

module App =
  type PageModel =
    | SignInPageModel of SignInPage.Model
    | SignUpPageModel of SignUpPage.Model
    | ForgotPasswordPageModel of ForgotPasswordPage.Model
    | SignUpSuccessPageModel of SignUpSuccessPage.Model
    | ResendEmailPageModel of ResendEmailPage.Model
    | ChangeEmailPageModel of ChangeEmailPage.Model
    | MainPageModel of MainPage.Model

  type Model = {
    PageModel: PageModel
    Token: string option
  }

  type PageMsg =
    | SignInPageMsg of SignInPage.Msg
    | SignUpPageMsg of SignUpPage.Msg
    | ForgotPasswordPageMsg of ForgotPasswordPage.Msg
    | SignUpSuccessPageMsg of SignUpSuccessPage.Msg
    | ResendEmailPageMsg of ResendEmailPage.Msg
    | ChangeEmailPageMsg of ChangeEmailPage.Msg
    | MainPageMsg of MainPage.Msg

  type Msg =
    | PageMsg of PageMsg
    | NavigateTo of PageModel
    | SignIn of SignInRequest
    | SignUp of SignUpRequest
    | SignOut
    | SendPasswordResetEmail of string
    | UpdateProfile of UpdateProfileRequest
    | ChangeEmail of ChangeEmailRequest
    | ChangePassword of ChangePasswordRequest
    | ResendConfirmEmail
    | ShowError of string

  let initModel = {
    PageModel = SignInPageModel SignInPage.initModel
    Token = None
  }

  let init () = initModel, Cmd.none

  let goToSignInCmd =
    SignInPage.initModel
    |> SignInPageModel
    |> NavigateTo
    |> Cmd.ofMsg

  let handleSignInMsg msg model =
    let newModel, eMsg = SignInPage.update msg model
    let cmd =
      match eMsg with
      | SignInPage.NoOp ->
        Cmd.none

      | SignInPage.SignIn r ->
        Cmd.ofMsg (SignIn r)

      | SignInPage.GoToSignUp ->
        SignUpPage.initModel
        |> SignUpPageModel
        |> NavigateTo
        |> Cmd.ofMsg

      | SignInPage.GoToForgotPassword ->
        ForgotPasswordPage.initModel
        |> ForgotPasswordPageModel
        |> NavigateTo
        |> Cmd.ofMsg

    newModel, cmd

  let handleSignUpMsg msg model =
    let newModel, eMsg = SignUpPage.update msg model
    let cmd =
      match eMsg with
      | SignUpPage.NoOp -> Cmd.none
      | SignUpPage.SignUp r -> Cmd.ofMsg (SignUp r)
      | SignUpPage.GoToSignIn -> goToSignInCmd
    newModel, cmd

  let handleForgotPasswordMsg msg model =
    let newModel, eMsg = ForgotPasswordPage.update msg model
    let cmd =
      match eMsg with
      | ForgotPasswordPage.NoOp -> Cmd.none
      | ForgotPasswordPage.Send e -> Cmd.ofMsg (SendPasswordResetEmail e)
      | ForgotPasswordPage.GoToSignIn -> goToSignInCmd
    newModel, cmd

  let handleSignUpSuccessMsg msg model =
    let newModel, eMsg = SignUpSuccessPage.update msg model
    let cmd =
      match eMsg with
      | SignUpSuccessPage.GoToSignIn -> goToSignInCmd
    newModel, cmd

  let handleResendEmailMsg msg model =
    let newModel, eMsg = ResendEmailPage.update msg model
    let cmd =
      match eMsg with
      | ResendEmailPage.ResendEmail ->
        Cmd.ofMsg ResendConfirmEmail

      | ResendEmailPage.GoToChangeEmail ->
        ChangeEmailPage.initModel
        |> ChangeEmailPageModel
        |> NavigateTo
        |> Cmd.ofMsg

      | ResendEmailPage.GoToSignIn ->
        Cmd.ofMsg SignOut
    newModel, cmd

  let handleChangeEmailMsg msg model =
    let newModel, eMsg = ChangeEmailPage.update msg model
    let cmd =
      match eMsg with
      | ChangeEmailPage.ExternalMsg.NoOp -> Cmd.none
      | ChangeEmailPage.ChangeEmail r -> Cmd.ofMsg (ChangeEmail r)
      | ChangeEmailPage.GoToSignIn -> Cmd.ofMsg SignOut
    newModel, cmd

  let handleMainMsg msg model =
    let newModel, eMsg = MainPage.update msg model
    let cmd =
      match eMsg with
      | MainPage.NoOp -> Cmd.none
      | MainPage.SignOut -> Cmd.ofMsg SignOut
      | MainPage.UpdateProfile p -> Cmd.ofMsg (UpdateProfile p)
      | MainPage.ChangeEmail r -> Cmd.ofMsg (ChangeEmail r)
      | MainPage.ChangePassword r -> Cmd.ofMsg (ChangePassword r)
    newModel, cmd

  let handlePageMsg pMsg aModel =
    match pMsg, aModel.PageModel with
    | SignInPageMsg msg, SignInPageModel model ->
      let newModel, cmd = handleSignInMsg msg model
      { aModel with PageModel = SignInPageModel newModel }, cmd

    | SignUpPageMsg msg, SignUpPageModel model ->
      let newModel, cmd = handleSignUpMsg msg model
      { aModel with PageModel = SignUpPageModel newModel }, cmd

    | ForgotPasswordPageMsg msg, ForgotPasswordPageModel model ->
      let newModel, cmd = handleForgotPasswordMsg msg model
      { aModel with PageModel = ForgotPasswordPageModel newModel }, cmd

    | SignUpSuccessPageMsg msg, SignUpSuccessPageModel model ->
      let newModel, cmd = handleSignUpSuccessMsg msg model
      { aModel with PageModel = SignUpSuccessPageModel newModel }, cmd

    | ResendEmailPageMsg msg, ResendEmailPageModel model ->
      let newModel, cmd = handleResendEmailMsg msg model
      { aModel with PageModel = ResendEmailPageModel newModel }, cmd

    | ChangeEmailPageMsg msg, ChangeEmailPageModel model ->
      let newModel, cmd = handleChangeEmailMsg msg model
      { aModel with PageModel = ChangeEmailPageModel newModel }, cmd

    | MainPageMsg msg, MainPageModel model ->
      let newModel, cmd = handleMainMsg msg model
      { aModel with PageModel = MainPageModel newModel }, cmd

    | _, _ -> aModel, Cmd.none

  let handleTwoTrackHttp model fSuccess =
    either
      fSuccess
      (fun es ->
        model,
        es
        |> foldErrors
        |> ShowError
        |> Cmd.ofMsg)

  let expireTokenCmd date =
    async {
      do! (date - DateTime.UtcNow).TotalMilliseconds
        |> int
        |> Async.Sleep

      return SignOut
    }
    |> Cmd.ofAsyncMsg

  let goToMainPageCmd token =
    async {
      let! response = Http.getProfile token
      
      return
        response
        |> bind MainPage.create
        |> either
          (MainPageModel >> NavigateTo)
          ("Can not open main page" |> ShowError |> ignore2)
    }
    |> Cmd.ofAsyncMsg

  let signIn model request =
    request
    |> Http.signIn
    |> Async.RunSynchronously
    |> handleTwoTrackHttp
      model
      (fun t ->
        let navCmd =
          if t.EmailConfirmed then
            goToMainPageCmd t.Token
          else
            request.Email
            |> ResendEmailPageModel
            |> NavigateTo
            |> Cmd.ofMsg
      
        let timerCmd = expireTokenCmd t.Expires
      
        { model with Token = Some t.Token }, Cmd.batch [navCmd; timerCmd])

  let signUp model request =
    request
    |> Http.signUp
    |> Async.RunSynchronously
    |> handleTwoTrackHttp
      model
      (fun _ ->
        model,
        request.Email
        |> SignUpSuccessPageModel
        |> NavigateTo
        |> Cmd.ofMsg)

  let resendConfirmEmail model =
    match model.Token with
    | Some t ->
      t
      |> Http.resendConfirmEmail
      |> Async.RunSynchronously
      |> handleTwoTrackHttp
        model
        (fun _ ->
          model,
          Cmd.none)

    | None ->
      model, Cmd.none
      
      
  let update aMsg aModel =
    match aMsg with
    | PageMsg pMsg ->
      handlePageMsg pMsg aModel

    | NavigateTo pModel ->
      { aModel with PageModel = pModel }, Cmd.none

    | SignIn r ->
      signIn aModel r
          
    | SignUp r ->
      signUp aModel r

    | SignOut ->
      { aModel with Token = None }, goToSignInCmd

    | SendPasswordResetEmail _ ->
      aModel, goToSignInCmd // TODO: Create real email send logic

    | UpdateProfile _ ->
      aModel, Cmd.none // TODO: Create real profile update logic

    | ChangeEmail _ ->
      aModel, Cmd.ofMsg SignOut // TODO: Create real email change logic

    | ChangePassword _ ->
      aModel, Cmd.none // TODO: Create real password change logic

    | ResendConfirmEmail ->
      resendConfirmEmail aModel

    | ShowError _ ->
      aModel, Cmd.none // TODO: Create real show error logic

  let view appModel dispatch =
    let pageDispatch = PageMsg >> dispatch

    match appModel.PageModel with
    | SignInPageModel model ->
      SignInPage.view model (SignInPageMsg >> pageDispatch)

    | SignUpPageModel model ->
      SignUpPage.view model (SignUpPageMsg >> pageDispatch)

    | ForgotPasswordPageModel model ->
      ForgotPasswordPage.view model (ForgotPasswordPageMsg >> pageDispatch)

    | SignUpSuccessPageModel model ->
      SignUpSuccessPage.view model (SignUpSuccessPageMsg >> pageDispatch)

    | ResendEmailPageModel model ->
      ResendEmailPage.view model (ResendEmailPageMsg >> pageDispatch)

    | ChangeEmailPageModel model ->
      ChangeEmailPage.view model (ChangeEmailPageMsg >> pageDispatch)

    | MainPageModel model ->
      MainPage.view model (MainPageMsg >> pageDispatch)

  // Note, this declaration is needed if you enable LiveUpdate
  let program = Program.mkProgram init update view

type App () as app =
  inherit Application ()

  let runner =
      App.program
#if DEBUG
      |> Program.withConsoleTrace
#endif
      |> XamarinFormsProgram.run app

#if DEBUG
  // Uncomment this line to enable live update in debug mode. 
  // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/tools.html#live-update for further  instructions.
  do runner.EnableLiveUpdate()
#endif    

  // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
  // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/models.html#saving-application-state for further  instructions.
#if APPSAVE
  let modelId = "model"
  override __.OnSleep() = 

    let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
    Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

    app.Properties.[modelId] <- json

  override __.OnResume() = 
    Console.WriteLine "OnResume: checking for model in app.Properties"
    try 
      match app.Properties.TryGetValue modelId with
      | true, (:? string as json) -> 

        Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
        let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

        Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
        runner.SetCurrentModel (model, Cmd.none)

      | _ -> ()
    with ex -> 
      App.program.onError("Error while restoring model found in app.Properties", ex)

  override this.OnStart() = 
    Console.WriteLine "OnStart: using same logic as OnResume()"
    this.OnResume()
#endif


