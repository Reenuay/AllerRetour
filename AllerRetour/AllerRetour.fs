namespace AllerRetour

open System
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms
open TwoTrackResult
open RequestTypes
open ResponseTypes
open Resources
open Views

module App =
  type PageModel =
    | SignInPageModel of SignInPage.Model
    | SignUpPageModel of SignUpPage.Model
    | ForgotPasswordPageModel of ForgotPasswordPage.Model
    | ResetPasswordPageModel of ResetPasswordPage.Model
    | SignUpSuccessPageModel of SignUpSuccessPage.Model
    | ResendEmailPageModel of ResendEmailPage.Model
    | ChangeEmailPageModel of ChangeEmailPage.Model
    | MainPageModel of MainPage.Model

  type Model = {
    PageModel: PageModel
    Route: Route
    Token: string option
    LoaderIsActive: bool
  }

  type PageMsg =
    | SignInPageMsg of SignInPage.Msg
    | SignUpPageMsg of SignUpPage.Msg
    | ForgotPasswordPageMsg of ForgotPasswordPage.Msg
    | ResetPasswordPageMsg of ResetPasswordPage.Msg
    | SignUpSuccessPageMsg of SignUpSuccessPage.Msg
    | ResendEmailPageMsg of ResendEmailPage.Msg
    | ChangeEmailPageMsg of ChangeEmailPage.Msg
    | MainPageMsg of MainPage.Msg

  type Msg =
    | PageMsg of PageMsg
    | NavigateTo of PageModel
    | SignUp of SignUpRequest
    | SignOut
    | SendPasswordResetEmail of PasswordResetEmailRequest
    | ResetPassword of PasswordResetRequest
    | UpdateProfile of UpdateProfileRequest
    | ChangeEmail of ChangeEmailRequest
    | ChangePassword of ChangePasswordRequest
    | ResendConfirmEmail
    | ShowMessage of string
    | LoadSettings
    | RefreshToken of string
    | RouteChanged of Route
    | LoaderStateChanged of bool

  let initModel = {
    PageModel = SignInPageModel SignInPage.initModel
    Route = Route.SignIn
    Token = None
    LoaderIsActive = false
  }

  let goToSignInCmd =
    SignInPage.initModel
    |> SignInPageModel
    |> NavigateTo
    |> Cmd.ofMsg

  let handleSignInMsg msg model =
    let
      ( newModel, cmd ) = SignInPage.update msg model
    in
    ( newModel, Cmd.map (SignInPageMsg >> PageMsg) cmd )

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
      | ForgotPasswordPage.Send r -> Cmd.ofMsg (SendPasswordResetEmail r)
      | ForgotPasswordPage.GoToSignIn -> goToSignInCmd
    newModel, cmd

  let handleResetPasswordMsg msg model =
    let newModel, eMsg = ResetPasswordPage.update msg model
    let cmd =
      match eMsg with
      | ResetPasswordPage.NoOp ->
        Cmd.none

      | ResetPasswordPage.Timer ->
        async {
          do! Async.SwitchToThreadPool()
          do! Async.Sleep 1000

          return
            if model.Timer > 0 then
              ResetPasswordPage.TimerTick
              |> ResetPasswordPageMsg
              |> PageMsg
              |> Some
            else
              None
        }
        |> Cmd.ofAsyncMsgOption

      | ResetPasswordPage.ResetPassword r ->
        r
        |> ResetPassword
        |> Cmd.ofMsg

      | ResetPasswordPage.GoToForgotPassword ->
        ForgotPasswordPage.initModel
        |> ForgotPasswordPageModel
        |> NavigateTo
        |> Cmd.ofMsg

      | ResetPasswordPage.GoToSignIn ->
        goToSignInCmd

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
        model
        |> ChangeEmailPage.create
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
      | MainPage.SaveSettings ->
        async {
          do! GlobalSettings.Save()
        }
        |> Async.Start

        Cmd.none

    newModel, cmd

  let handleTwoTrackHttp model fSuccess =
    either
      fSuccess
      (fun es ->
        model,
        es
        |> foldErrors
        |> ShowMessage
        |> Cmd.ofMsg)

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

  let sendPin model request =
    request
    |> Http.sendPin
    |> Async.RunSynchronously
    |> handleTwoTrackHttp
      model
      (fun _ ->
        model,
        Cmd.batch [
          request.Email
          |> ResetPasswordPage.initModel
          |> ResetPasswordPageModel
          |> NavigateTo
          |> Cmd.ofMsg;

          ResetPasswordPage.TimerTick
          |> ResetPasswordPageMsg
          |> PageMsg
          |> Cmd.ofMsg;
        ])

  let resetPassword model request =
    request
    |> Http.resetPassword
    |> Async.RunSynchronously
    |> handleTwoTrackHttp
      model
      (fun _ ->
        model,
        Cmd.batch [
          goToSignInCmd

          "Your password has been successfully changed!"
          |> ShowMessage
          |> Cmd.ofMsg
        ])

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
          "Email were sent successfully!"
          |> ShowMessage
          |> Cmd.ofMsg)

    | None ->
      model, Cmd.none

  let updateProfile model request =
    match model.Token with
    | Some t ->
      Http.updateProfile t request
      |> Async.RunSynchronously
      |> handleTwoTrackHttp
        model
        (fun r ->
          match model.PageModel with
          | MainPageModel m ->
            model,
            r
            |> MainPage.updateModel m
            |> either
              (MainPageModel >> NavigateTo)
              ("Can not open main page: server sent invalid data" |> ShowMessage |> ignore2)
            |> Cmd.ofMsg

          | _ ->
            model, Cmd.none)

    | None ->
      model, Cmd.none

  let changeEmail model request =
    match model.Token with
    | Some t ->
      Http.changeEmail t request
      |> Async.RunSynchronously
      |> handleTwoTrackHttp
        model
        (fun _ ->
          model,
          Cmd.batch [
            Cmd.ofMsg SignOut

            "Your email has been successfully changed!"
            +  " Check your inbox to confirm your new email ID."
            |> ShowMessage
            |> Cmd.ofMsg
          ])

    | None ->
      model, Cmd.none

  let changePassword model request =
    match model.Token with
    | Some t ->
      Http.changePassword t request
      |> Async.RunSynchronously
      |> handleTwoTrackHttp
        model
        (fun _ ->
          model,
          Cmd.batch [
            Cmd.ofMsg SignOut

            "Your password has been successfully changed!"
            +  " Please relogin using new password."
            |> ShowMessage
            |> Cmd.ofMsg
          ])

    | None ->
      model, Cmd.none

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

    | ResetPasswordPageMsg msg, ResetPasswordPageModel model ->
      let newModel, cmd = handleResetPasswordMsg msg model
      { aModel with PageModel = ResetPasswordPageModel newModel }, cmd

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
      
  let update aMsg aModel =
    match aMsg with
    | PageMsg pMsg ->
      handlePageMsg pMsg aModel

    | NavigateTo pModel ->
      { aModel with PageModel = pModel }, Cmd.none
          
    | SignUp r ->
      signUp aModel r

    | SignOut ->
      { aModel with Token = None }, goToSignInCmd

    | SendPasswordResetEmail r ->
      sendPin aModel r

    | ResetPassword r ->
      resetPassword aModel r

    | UpdateProfile r ->
      updateProfile aModel r

    | ChangeEmail r ->
      changeEmail aModel r

    | ChangePassword r ->
      changePassword aModel r

    | ResendConfirmEmail ->
      resendConfirmEmail aModel

    | ShowMessage err ->
      Application.Current.MainPage.DisplayAlert(String.Empty, err, "Ok") |> ignore
      aModel, Cmd.none

    | LoadSettings ->
      let cmd =
        match GlobalSettings.Load() with
        | Failure _ -> Cmd.ofMsg (ShowMessage "Error loading app settings.")
        | _ -> Cmd.none

      ( aModel, cmd )

    | RefreshToken token ->
      match aModel.Token with
      | Some t when t = token ->
        let
          cmd =
            Route.push Route.SignIn
        in
        ( { aModel with Token = None }, cmd )

      | _ ->
        ( aModel, Cmd.none )

    | RouteChanged route ->
      match route with
      | Route.SignIn ->
        let
          newModel =
            {
              aModel with
                PageModel = SignInPageModel SignInPage.initModel
                Route = route
            }
        in
        ( newModel, Cmd.none )

      | Route.SignUp ->
        let
          newModel =
            {
              aModel with
                PageModel = SignUpPageModel SignUpPage.initModel
                Route = route
            }
        in
        ( newModel, Cmd.none )

      | Route.ForgotPassword ->
        let
          newModel =
            {
              aModel with
                PageModel = ForgotPasswordPageModel ForgotPasswordPage.initModel
                Route = route
            }
        in
        ( newModel, Cmd.none )

      | Route.ResendEmail email ->
        let
          newModel =
            {
              aModel with
                PageModel = ResendEmailPageModel email
                Route = route
            }
        in
        ( newModel, Cmd.none )

      | Route.Main ( token, profile ) ->
        match MainPage.create profile with
        | Success mainModel ->
          let
            newModel =
              { aModel with
                  PageModel = MainPageModel mainModel
                  Route = route
                  Token = Some token.Token
              }
          let
            cmd =
              async {
                do! Async.SwitchToThreadPool ()

                do!
                  Async.Sleep <|
                  int (token.Expires - DateTime.UtcNow).TotalMilliseconds

                return RefreshToken token.Token
              }
              |> Cmd.ofAsyncMsg
          in
          ( newModel, cmd )

        | Failure errors ->
          ( aModel, AppMessage.show <| foldErrors errors )

    | LoaderStateChanged isActive ->
      ( { aModel with LoaderIsActive = isActive }, Cmd.none )

  let view model dispatch =
    let pageDispatch = PageMsg >> dispatch

    let pageLayout =
      match model.PageModel with
      | SignInPageModel pageModel ->
        SignInPage.view pageModel (SignInPageMsg >> pageDispatch)

      | SignUpPageModel pageModel ->
        SignUpPage.view pageModel (SignUpPageMsg >> pageDispatch)

      | ForgotPasswordPageModel pageModel ->
        ForgotPasswordPage.view pageModel (ForgotPasswordPageMsg >> pageDispatch)

      | ResetPasswordPageModel pageModel ->
        ResetPasswordPage.view pageModel (ResetPasswordPageMsg >> pageDispatch)

      | SignUpSuccessPageModel pageModel ->
        SignUpSuccessPage.view pageModel (SignUpSuccessPageMsg >> pageDispatch)

      | ResendEmailPageModel pageModel ->
        ResendEmailPage.view pageModel (ResendEmailPageMsg >> pageDispatch)

      | ChangeEmailPageModel pageModel ->
        ChangeEmailPage.view pageModel (ChangeEmailPageMsg >> pageDispatch)

      | MainPageModel pageModel ->
        MainPage.view pageModel (MainPageMsg >> pageDispatch)

    let
      whiteBoxSize =
        screenWidthP 0.3
    let
      indicatorSize =
        whiteBoxSize * 0.6
    let
      activityIndicator =
        if model.LoaderIsActive then
          [
            View.BoxView(
              backgroundColor = Colors.activity,
              opacity = 0.8
            )
              .LayoutFlags(AbsoluteLayoutFlags.All)
              .LayoutBounds(Rectangle(0., 0., 1., 1.))

            View.BoxView(
              backgroundColor = Color.White
            )
              .LayoutFlags(AbsoluteLayoutFlags.PositionProportional)
              .LayoutBounds(Rectangle(0.5, 0.5, whiteBoxSize, whiteBoxSize))

            View.ActivityIndicator(
              isRunning = true,
              color = Colors.accent
            )
              .LayoutFlags(AbsoluteLayoutFlags.PositionProportional)
              .LayoutBounds(Rectangle(0.5, 0.5, indicatorSize, indicatorSize))
          ]
        else
          []
    in
    View.ContentPage(
      useSafeArea = true,
      content = View.AbsoluteLayout(
        children = [
          pageLayout
            .LayoutFlags(AbsoluteLayoutFlags.All)
            .LayoutBounds(Rectangle(0., 0., 1., 1.))
        ]
        @ activityIndicator
      )
    )

  // let init () = initModel, Cmd.ofMsg (SignIn { Email = "reenuay777@gmail.com"; Password = "testtest4" })

  let init () = initModel, Cmd.ofMsg LoadSettings

  // Note, this declaration is needed if you enable LiveUpdate
  let program = Program.mkProgram init update view

  let routeSub dispatch =
    Route.Changed.Add(
      fun route ->
        dispatch <| RouteChanged route
    )

  let loaderSub dispatch =
    Loader.StateChanged.Add(
      fun state ->
        let
          isActive =
            match state with
            | LoaderState.Started ->
              true

            | LoaderState.Stopped ->
              false
        in
        dispatch <| LoaderStateChanged isActive
    )

type App () as app =
  inherit Application ()

  let runner =
      App.program
#if DEBUG
      |> Program.withConsoleTrace
#endif
      |> Program.withSubscription(fun _ ->  Cmd.ofSub App.routeSub)
      |> Program.withSubscription(fun _ ->  Cmd.ofSub App.loaderSub)
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
