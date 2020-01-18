namespace AllerRetour

open System
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms

module App =
  type PageModel =
    | SignInPageModel of SignInPage.Model
    | SignUpPageModel of SignUpPage.Model
    | MainPageModel of MainPage.Model

  type Model = {
    PageModel: PageModel
  }

  type PageMsg =
    | SignInPageMsg of SignInPage.Msg
    | SignUpPageMsg of SignUpPage.Msg
    | MainPageMsg of MainPage.Msg

  type Msg =
    | PageMsg of PageMsg
    | NavigateTo of PageModel
    | SignIn
    | SignUp
    | SignOut

  let initModel = {
    PageModel = SignInPageModel SignInPage.initModel
  }

  let init () = initModel, Cmd.none

  let handleSignInMsg msg model =
    let newModel, eMsg = SignInPage.update msg model
    let cmd =
      match eMsg with
      | SignInPage.NoOp -> Cmd.none
      | SignInPage.SignIn -> Cmd.ofMsg SignIn
      | SignInPage.GoToSignUp ->
        SignUpPage.initModel
        |> SignUpPageModel
        |> NavigateTo
        |> Cmd.ofMsg
    newModel, cmd

  let handleSignUpMsg msg model =
    let newModel, eMsg = SignUpPage.update msg model
    let cmd =
      match eMsg with
      | SignUpPage.NoOp -> Cmd.none
      | SignUpPage.SignUp -> Cmd.ofMsg SignUp
      | SignUpPage.GoToSignIn ->
        SignInPage.initModel
        |> SignInPageModel
        |> NavigateTo
        |> Cmd.ofMsg
    newModel, cmd

  let handleMainMsg msg model  =
    let newModel, eMsg = MainPage.update msg model
    let cmd =
      match eMsg with
      | MainPage.NoOp -> Cmd.none
      | MainPage.SignOut -> Cmd.ofMsg SignOut
      | MainPage.UpdateProfile _ -> Cmd.none // TODO: Create real profile update logic
      | MainPage.ChangeEmail _ -> Cmd.none // TODO: Create real email change logic
      | MainPage.ChangePassword _ -> Cmd.none // TODO: Create real password change logic
    newModel, cmd

  let handlePageMsg pMsg aModel =
    match pMsg, aModel.PageModel with
    | SignInPageMsg msg, SignInPageModel model ->
      let newModel, cmd = handleSignInMsg msg model
      { aModel with PageModel = SignInPageModel newModel }, cmd

    | SignUpPageMsg msg, SignUpPageModel model ->
      let newModel, cmd = handleSignUpMsg msg model
      { aModel with PageModel = SignUpPageModel newModel }, cmd

    | MainPageMsg msg, MainPageModel model ->
      let newModel, cmd = handleMainMsg msg model
      { aModel with PageModel = MainPageModel newModel }, cmd

    | _, _ -> aModel, Cmd.none
      
  let update aMsg aModel =
    match aMsg with
    | PageMsg pMsg ->
      handlePageMsg pMsg aModel

    | NavigateTo pModel ->
      match pModel with
      | SignInPageModel sModel -> { aModel with PageModel = SignInPageModel sModel }, Cmd.none
      | SignUpPageModel sModel -> { aModel with PageModel = SignUpPageModel sModel }, Cmd.none
      | MainPageModel mModel -> { aModel with PageModel = MainPageModel mModel }, Cmd.none

    | SignIn ->
      aModel, Cmd.ofMsg (NavigateTo (MainPageModel MainPage.initModel)) // TODO: Create real sign in logic

    | SignUp ->
      aModel, Cmd.none // TODO: Create real sign up logic

    | SignOut ->
      aModel, Cmd.ofMsg (NavigateTo (SignInPageModel SignInPage.initModel)) // TODO: Create real sign out logic

  let view appModel dispatch =
    let pageDispatch = PageMsg >> dispatch

    match appModel.PageModel with
    | SignInPageModel model -> SignInPage.view model (SignInPageMsg >> pageDispatch)
    | SignUpPageModel model -> SignUpPage.view model (SignUpPageMsg >> pageDispatch)
    | MainPageModel model -> MainPage.view model (MainPageMsg >> pageDispatch)

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


