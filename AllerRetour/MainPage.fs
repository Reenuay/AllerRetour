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
}

type Pages = {
  MainPage: ViewElement
  EditProfilePage: ViewElement option
  ChangeEmailPage: ViewElement option
}

type Msg =
  | ClickEditProfile
  | ClickChangeEmail
  | ClickChangePassword
  | ClickSignOut
  | UpdateProfile of Profile
  | EditProfilePageMsg of EditProfilePage.Msg
  | ChangeEmailPageMsg of ChangeEmailPage.Msg

type ExternalMsg =
  | NoOp
  | SIgnOut

let initMainPageModel = {
  CardId = ""
  Email = ""
  Profile = Profile.Empty
}

let initModel = {
  MainPageModel = initMainPageModel
  EditProfilePageModel = None
  ChangeEmailPageModel = None
}

let handleEditProfileMsg mModel pModel eMsg =
  { mModel
    with
      EditProfilePageModel = Some pModel
      MainPageModel = {
        mModel.MainPageModel
        with
          Profile =
            match eMsg with
            | EditProfilePage.NoOp -> mModel.MainPageModel.Profile
            | EditProfilePage.UpdateProfile -> pModel.Profile
      }
  }

let handleChangeEmailMsg mModel eModel eMsg =
  { mModel
    with
      ChangeEmailPageModel = Some eModel
      MainPageModel = {
        mModel.MainPageModel
        with
          Email =
            match eMsg with
            | ChangeEmailPage.NoOp -> mModel.MainPageModel.Email
            | ChangeEmailPage.ChangeEmail -> eModel.Email
      }
  }

let update mMsg mModel =
  match mMsg with
  | ClickEditProfile -> { mModel with EditProfilePageModel = Some EditProfilePage.initModel }, NoOp
  | ClickSignOut -> mModel, SIgnOut
  | EditProfilePageMsg msg ->
    match mModel.EditProfilePageModel with
    | Some model ->
      let newModel, eMsg = EditProfilePage.update msg model
      handleEditProfileMsg mModel newModel eMsg, NoOp // Create real profile update logic

    | None -> mModel, NoOp
  | ChangeEmailPageMsg msg ->
    match mModel.ChangeEmailPageModel with
    | Some model ->
      let newModel, eMsg = ChangeEmailPage.update msg model
      handleChangeEmailMsg mModel newModel eMsg, NoOp // Create real email change logic
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

  match editProfilePage with
  | None -> [mainPage]
  | Some editProfile -> [mainPage; editProfile]

let view model dispatch =
  let mainPage = mainPageView model.MainPageModel dispatch

  let editProfilePage
    =  model.EditProfilePageModel
    |> Option.map(fun pModel -> EditProfilePage.view pModel (EditProfilePageMsg >> dispatch))

  let changeEmailPage
    =  model.ChangeEmailPageModel
    |> Option.map(fun eModel -> ChangeEmailPage.view eModel (ChangeEmailPageMsg >> dispatch))

  let allPages = {
    MainPage = mainPage
    EditProfilePage = editProfilePage
    ChangeEmailPage = changeEmailPage
  }

  View.NavigationPage(
    pages = getPages allPages
  )
