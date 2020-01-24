module AllerRetour.MainPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type MainPageModel = {
  CardId: string
  Email: string
  Profile: Profile
}

type Model = {
  MainPageModel: MainPageModel
  EditProfilePageModel: EditProfileSubPage.Model option
  ChangeEmailPageModel: ChangeEmailSubPage.Model option
  ChangePasswordPageModel: ChangePasswordSubPage.Model option
}

type Msg =
  | ClickEditProfile
  | ClickChangeEmail
  | ClickChangePassword
  | ClickSignOut
  | EditProfilePageMsg of EditProfileSubPage.Msg
  | ChangeEmailPageMsg of ChangeEmailSubPage.Msg
  | ChangePasswordPageMsg of ChangePasswordSubPage.Msg

type ExternalMsg =
  | NoOp
  | SignOut
  | UpdateProfile of Profile
  | ChangeEmail of EmailAndPassword
  | ChangePassword of ChangePasswordRequest

type Pages = {
  MainPage: ViewElement
  EditProfilePage: ViewElement option
  ChangeEmailPage: ViewElement option
  ChangePasswordPage: ViewElement option
}

let initMainPageModel = {
  CardId = ""
  Email = ""
  Profile = Profile.Empty
}

let initModel = {
  MainPageModel = initMainPageModel
  EditProfilePageModel = None
  ChangeEmailPageModel = None
  ChangePasswordPageModel = None
}

let navigationMapper (model : Model) =
    let editPageModel = model.EditProfilePageModel
    let changeEmailModel = model.ChangeEmailPageModel
    let changePasswordModel = model.ChangePasswordPageModel

    match editPageModel, changeEmailModel, changePasswordModel with
    | None, None, None ->
        model
    | Some _, None, None ->
        { model with EditProfilePageModel = None }
    | _, Some _, None ->
        { model with ChangeEmailPageModel = None }
    | _, _, Some _ ->
        { model with ChangePasswordPageModel = None }

let update mMsg mModel =
  match mMsg with
  | ClickEditProfile ->
    { mModel
      with
        EditProfilePageModel
          = Some (EditProfileSubPage.create mModel.MainPageModel.Profile)
    }, NoOp

  | ClickChangeEmail ->
    { mModel
      with
        ChangeEmailPageModel =
          Some (ChangeEmailSubPage.create mModel.MainPageModel.Email)
    }, NoOp

  | ClickChangePassword ->
    { mModel with ChangePasswordPageModel = Some ChangePasswordSubPage.initModel }, NoOp

  | ClickSignOut -> mModel, SignOut

  | EditProfilePageMsg msg ->
    match mModel.EditProfilePageModel with
    | Some model ->
      let newModel, eMsg = EditProfileSubPage.update msg model
      let newModelOption, eMsg2, profile =
        match eMsg with
        | EditProfileSubPage.NoOp ->
          Some newModel, NoOp, mModel.MainPageModel.Profile
        | EditProfileSubPage.UpdateProfile p ->
          None, UpdateProfile p, p

      { mModel
        with
          EditProfilePageModel = newModelOption
          MainPageModel = { mModel.MainPageModel with Profile = profile }
      }, eMsg2
    | None -> mModel, NoOp

  | ChangeEmailPageMsg msg ->
    match mModel.ChangeEmailPageModel with
    | Some model ->
      let newModel, eMsg = ChangeEmailSubPage.update msg model
      let newModelOption, eMsg2, email =
        match eMsg with
        | ChangeEmailSubPage.NoOp -> Some newModel, NoOp, mModel.MainPageModel.Email
        | ChangeEmailSubPage.ChangeEmail d ->
          None,
          ChangeEmail d,
          d.Email
      { mModel
        with
          ChangeEmailPageModel = newModelOption
          MainPageModel = { mModel.MainPageModel with Email = email }
      }, eMsg2
    | None -> mModel, NoOp

  | ChangePasswordPageMsg msg ->
    match mModel.ChangePasswordPageModel with
    | Some model ->
      let newModel, peMsg = ChangePasswordSubPage.update msg model
      let newModelOption, eMsg =
        match peMsg with
        | ChangePasswordSubPage.NoOp -> Some newModel, NoOp
        | ChangePasswordSubPage.ChangePassword r -> None, ChangePassword r
      { mModel with ChangePasswordPageModel = newModelOption }, eMsg
    | None -> mModel, NoOp

let mainPageView mModel dispatch =
  View.ContentPage(
    content = View.StackLayout(
      children = [
        View.Label(text = sprintf "Hello %s %s!" mModel.Profile.FirstName mModel.Profile.LastName)
        View.Label(text = mModel.CardId)
        View.Label(text = mModel.Email)
        View.Button(
          text = "Edit profile",
          command = (fun () -> dispatch ClickEditProfile))
        View.Button(
          text = "Change email",
          command = (fun () -> dispatch ClickChangeEmail))
        View.Button(
          text = "Change password",
          command = (fun () -> dispatch ClickChangePassword))
        View.Button(
          text = "Sign out",
          command = (fun () -> dispatch ClickSignOut))
      ]
    )
  )

let getPages allPages =
  let mainPage = allPages.MainPage
  let editProfilePage = allPages.EditProfilePage
  let changeEmailPage = allPages.ChangeEmailPage
  let changePasswordPage = allPages.ChangePasswordPage

  match editProfilePage, changeEmailPage, changePasswordPage with
  | None, None, None             -> [ mainPage ]
  | Some editProfile, None, None -> [ mainPage; editProfile ]
  | _, Some changeEmail, None    -> [ mainPage; changeEmail ]
  | _, _, Some changePassword    -> [ mainPage; changePassword ]

let view model dispatch =
  let mainPage = mainPageView model.MainPageModel dispatch

  let editProfilePage
    =  model.EditProfilePageModel
    |> Option.map(fun pModel -> EditProfileSubPage.view pModel (EditProfilePageMsg >> dispatch))

  let changeEmailPage
    =  model.ChangeEmailPageModel
    |> Option.map(fun eModel -> ChangeEmailSubPage.view eModel (ChangeEmailPageMsg >> dispatch))

  let changePasswordPage
    =  model.ChangePasswordPageModel
    |> Option.map(fun pModel -> ChangePasswordSubPage.view pModel (ChangePasswordPageMsg >> dispatch))

  let allPages = {
    MainPage = mainPage
    EditProfilePage = editProfilePage
    ChangeEmailPage = changeEmailPage
    ChangePasswordPage = changePasswordPage
  }

  View.NavigationPage(
    pages = getPages allPages
  )
