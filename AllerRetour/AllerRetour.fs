namespace AllerRetour

open System
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms
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
    | SignOut
    | UpdateProfile of UpdateProfileRequest
    | ChangeEmail of ChangeEmailRequest
    | ChangePassword of ChangePasswordRequest
    | ShowMessage of string
    | LoadSettings
    | RefreshToken of string
    | RouteChanged of Route
    | LoaderStateChanged of bool

  let initModel =
    {
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

  let handleSignUpMsg msg model =
    let
      ( newModel, cmd ) =
        SignUpPage.update msg model
    in
    ( newModel, Cmd.map (SignUpPageMsg >> PageMsg) cmd )

  let handleForgotPasswordMsg msg model =
    let
      ( newModel, cmd ) =
        ForgotPasswordPage.update msg model
    in
    ( newModel, Cmd.map (ForgotPasswordPageMsg >> PageMsg) cmd )

  let handleResetPasswordMsg msg model =
    let
      ( newModel, cmd ) =
        ResetPasswordPage.update msg model
    in
    ( newModel, Cmd.map (ResetPasswordPageMsg >> PageMsg) cmd )

  let handleSignUpSuccessMsg msg model =
    let
      ( newModel, cmd ) =
        SignUpSuccessPage.update msg model
    in
    ( newModel, Cmd.map (SignUpSuccessPageMsg >> PageMsg) cmd )

  let handleResendEmailMsg token pageMsg pageModel =
    let
      ( newPageModel, pageCmd, externalMsg ) =
        ResendEmailPage.update token pageMsg pageModel
    let
      cmd =
        match externalMsg with
        | ResendEmailPage.NoOp ->
          Cmd.none

        | ResendEmailPage.SignOut ->
          Cmd.ofMsg SignOut
    let
      cmd =
        Cmd.batch
          [
            cmd

            Cmd.map (ResendEmailPageMsg >> PageMsg) pageCmd
          ]
    in
    ( newPageModel, cmd )

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
    Result.either
      fSuccess
      (fun es ->
        model,
        es
        |> Message.foldErrors
        |> ShowMessage
        |> Cmd.ofMsg)

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
            |> Result.either
              (MainPageModel >> NavigateTo)
              ("Can not open main page: server sent invalid data" |> ShowMessage |> always)
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
    | ( SignInPageMsg pageMsg, SignInPageModel pageModel ) ->
      let
        ( newPageModel, pageCmd ) =
          SignInPage.update pageMsg pageModel
      let
        newModel =
          { aModel with
              PageModel =
                SignInPageModel newPageModel
          }
      in
      ( newModel, Cmd.map (SignInPageMsg >> PageMsg) pageCmd )

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
      let newModel, cmd = handleResendEmailMsg aModel.Token msg model
      { aModel with PageModel = ResendEmailPageModel newModel }, cmd

    | ChangeEmailPageMsg msg, ChangeEmailPageModel model ->
      let newModel, cmd = handleChangeEmailMsg msg model
      { aModel with PageModel = ChangeEmailPageModel newModel }, cmd

    | MainPageMsg msg, MainPageModel model ->
      let newModel, cmd = handleMainMsg msg model
      { aModel with PageModel = MainPageModel newModel }, cmd

    | _, _ -> aModel, Cmd.none

  let refreshTokenCmd token =
    Cmd.ofAsyncMsg <|
      async {
        do! Async.SwitchToThreadPool ()

        do!
          Async.Sleep <|
          int (token.Expires - DateTime.UtcNow).TotalMilliseconds

        return RefreshToken token.Token
      }

  let routeChanged model route =
    match route with
    | Route.SignIn ->
      let
        newModel =
          { model with
              PageModel = SignInPageModel SignInPage.initModel
              Route = route
          }
      in
      ( newModel, Cmd.none )

    | Route.SignUp ->
      let
        newModel =
          { model with
              PageModel = SignUpPageModel SignUpPage.initModel
              Route = route
          }
      in
      ( newModel, Cmd.none )

    | Route.ForgotPassword ->
      let
        newModel =
          { model with
              PageModel = ForgotPasswordPageModel ForgotPasswordPage.initModel
              Route = route
          }
      in
      ( newModel, Cmd.none )

    | Route.ResetPassword email ->
      let
        ( pageModel, pageCmd ) =
          ResetPasswordPage.init email
      let
        newModel =
          { model with
              PageModel = ResetPasswordPageModel pageModel
          }
      in
      ( newModel, Cmd.map (ResetPasswordPageMsg >> PageMsg) pageCmd )

    | Route.SignUpSuccess email ->
      let
        pageModel =
          SignUpSuccessPage.initModel email
      let
        newModel =
          { model with
              PageModel = SignUpSuccessPageModel pageModel
              Route = route
          }
      in
      ( newModel, Cmd.none )

    | Route.ResendEmail ( token, email ) ->
      let
        pageModel =
          ResendEmailPage.initModel email
      let
        newModel =
          { model with
              PageModel = ResendEmailPageModel pageModel
              Route = route
              Token = Some token.Token
          }
      in
      ( newModel, refreshTokenCmd token )

    | Route.ChangeEmail email ->
      let
        pageModel =
          ChangeEmailPage.create email
      let
        newModel =
          { model with
              PageModel = ChangeEmailPageModel pageModel
              Route = route
          }
      in
      ( newModel, Cmd.none )

    | Route.Main ( token, profile ) ->
      match MainPage.create profile with
      | Ok pageModel ->
        let
          newModel =
            { model with
                PageModel = MainPageModel pageModel
                Route = route
                Token = Some token.Token
            }
        in
        ( newModel, refreshTokenCmd token )

      | Error errors ->
        ( model, Message.errors errors )
      
  let update aMsg aModel =
    match aMsg with
    | PageMsg pMsg ->
      handlePageMsg pMsg aModel

    | NavigateTo pModel ->
      { aModel with PageModel = pModel }, Cmd.none

    | SignOut ->
      ( { aModel with Token = None }, Route.push Route.SignIn ) // REPLACE WITH Cmd.none

    | UpdateProfile r ->
      updateProfile aModel r

    | ChangeEmail r ->
      changeEmail aModel r

    | ChangePassword r ->
      changePassword aModel r

    | ShowMessage err ->
      Application.Current.MainPage.DisplayAlert(String.Empty, err, "Ok") |> ignore
      aModel, Cmd.none

    | LoadSettings ->
      let cmd =
        match GlobalSettings.Load() with
        | Error _ -> Cmd.ofMsg (ShowMessage "Error loading app settings.")
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
      routeChanged aModel route

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

  let init () =
    ( initModel, Cmd.ofMsg LoadSettings )

  // Note, this declaration is needed if you enable LiveUpdate
  let program =
    Program.mkProgram init update view

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
