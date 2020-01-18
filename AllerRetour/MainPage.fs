module AllerRetour.MainPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

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
          = Some (EditProfilePage.initModel mModel.MainPageModel.Profile)
    }, NoOp

  | ClickChangeEmail ->
    { mModel with ChangeEmailPageModel = Some ChangeEmailPage.initModel }, NoOp

  | ClickChangePassword ->
    { mModel with ChangePasswordPageModel = Some ChangePasswordPage.initModel }, NoOp

  | ClickSignOut -> mModel, SignOut

  | EditProfilePageMsg msg ->
    match mModel.EditProfilePageModel with
    | Some model ->
      let newModel, eMsg = EditProfilePage.update msg model
      let newModelOption, eMsg2, profile =
        match eMsg with
        | EditProfilePage.NoOp ->
          Some newModel, NoOp, mModel.MainPageModel.Profile
        | EditProfilePage.UpdateProfile ->
          None, UpdateProfile newModel.Profile, newModel.Profile

      { mModel
        with
          EditProfilePageModel = newModelOption
          MainPageModel = { mModel.MainPageModel with Profile = profile }
      }, eMsg2
    | None -> mModel, NoOp

  | ChangeEmailPageMsg msg ->
    match mModel.ChangeEmailPageModel with
    | Some model ->
      let newModel, eMsg = ChangeEmailPage.update msg model
      let newModelOption, eMsg2, email =
        match eMsg with
        | ChangeEmailPage.NoOp -> Some newModel, NoOp, mModel.MainPageModel.Email
        | ChangeEmailPage.ChangeEmail ->
          None,
          ChangeEmail {
            Email = newModel.Email
            Password = newModel.Password
          },
          newModel.Email
      { mModel
        with
          ChangeEmailPageModel = newModelOption
          MainPageModel = { mModel.MainPageModel with Email = email }
      }, eMsg2
    | None -> mModel, NoOp

  | ChangePasswordPageMsg msg ->
    match mModel.ChangePasswordPageModel with
    | Some model ->
      let newModel, peMsg = ChangePasswordPage.update msg model
      let newModelOption, eMsg =
        match peMsg with
        | ChangePasswordPage.NoOp -> Some newModel, NoOp
        | ChangePasswordPage.ChangePassword ->
          None,
          ChangePassword {
            NewPassword = newModel.NewPassword
            OldPassword = newModel.OldPassword
          }
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
