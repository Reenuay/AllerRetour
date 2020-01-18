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
  EditProfilePageModel: EditProfilePage.Model option
  ChangeEmailPageModel: ChangeEmailPage.Model option
  ChangePasswordPageModel: ChangePasswordPage.Model option
}

type Pages = {
  MainPage: ViewElement
  EditProfilePage: ViewElement option
  ChangeEmailPage: ViewElement option
  ChangePasswordPage: ViewElement option
}

type Msg =
  | ClickEditProfile
  | ClickChangeEmail
  | ClickChangePassword
  | ClickSignOut
  | EditProfilePageMsg of EditProfilePage.Msg
  | ChangeEmailPageMsg of ChangeEmailPage.Msg
  | ChangePasswordPageMsg of ChangePasswordPage.Msg

type ExternalMsg =
  | NoOp
  | SignOut
  | UpdateProfile of Profile
  | ChangeEmail of ChangeEmailRequest
  | ChangePassword of ChangePasswordRequest

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

let update mMsg mModel =
  match mMsg with
  | ClickEditProfile ->
    { mModel with EditProfilePageModel = Some EditProfilePage.initModel }, NoOp

  | ClickChangeEmail ->
    { mModel with ChangeEmailPageModel = Some ChangeEmailPage.initModel }, NoOp

  | ClickChangePassword ->
    { mModel with ChangePasswordPageModel = Some ChangePasswordPage.initModel }, NoOp

  | ClickSignOut -> mModel, SignOut

  | EditProfilePageMsg msg ->
    match mModel.EditProfilePageModel with
    | Some model ->
      let newModel, eMsg = EditProfilePage.update msg model
      let newModel2, eMsg2 =
        match eMsg with
        | EditProfilePage.NoOp -> mModel.MainPageModel.Profile, NoOp
        | EditProfilePage.UpdateProfile -> newModel.Profile, UpdateProfile newModel.Profile

      { mModel
        with
          EditProfilePageModel = None
          MainPageModel = { mModel.MainPageModel with Profile = newModel2 }
      }, eMsg2
    | None -> mModel, NoOp

  | ChangeEmailPageMsg msg ->
    match mModel.ChangeEmailPageModel with
    | Some model ->
      let newModel, eMsg = ChangeEmailPage.update msg model
      let newModel2, eMsg2 =
        match eMsg with
        | ChangeEmailPage.NoOp -> mModel.MainPageModel.Email, NoOp
        | ChangeEmailPage.ChangeEmail ->
          newModel.Email, ChangeEmail {
            Email = newModel.Email
            Password = newModel.Password
          }
      { mModel
        with
          ChangeEmailPageModel = None
          MainPageModel = { mModel.MainPageModel with Email = newModel2 }
      }, eMsg2
    | None -> mModel, NoOp

  | ChangePasswordPageMsg msg ->
    match mModel.ChangePasswordPageModel with
    | Some model ->
      let newModel, eMsg = ChangePasswordPage.update msg model
      let eMsg2 =
        match eMsg with
        | ChangePasswordPage.NoOp -> NoOp
        | ChangePasswordPage.ChangePassword ->
          ChangePassword {
            NewPassword = newModel.NewPassword
            OldPassword = newModel.OldPassword
          }
      { mModel with ChangeEmailPageModel = None }, eMsg2
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
  | None, None, None -> [mainPage]
  | Some editProfile, _, _ -> [mainPage; editProfile]
  | _, Some changeEmail, _ -> [mainPage; changeEmail]
  | _, _, Some changePassword -> [mainPage; changePassword]

let view model dispatch =
  let mainPage = mainPageView model.MainPageModel dispatch

  let editProfilePage
    =  model.EditProfilePageModel
    |> Option.map(fun pModel -> EditProfilePage.view pModel (EditProfilePageMsg >> dispatch))

  let changeEmailPage
    =  model.ChangeEmailPageModel
    |> Option.map(fun eModel -> ChangeEmailPage.view eModel (ChangeEmailPageMsg >> dispatch))

  let changePasswordPage
    =  model.ChangePasswordPageModel
    |> Option.map(fun pModel -> ChangePasswordPage.view pModel (ChangePasswordPageMsg >> dispatch))

  let allPages = {
    MainPage = mainPage
    EditProfilePage = editProfilePage
    ChangeEmailPage = changeEmailPage
    ChangePasswordPage = changePasswordPage
  }

  View.NavigationPage(
    pages = getPages allPages
  )
