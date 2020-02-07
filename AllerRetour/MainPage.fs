module AllerRetour.MainPage

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open TwoTrackResult
open PrimitiveTypes
open RequestTypes
open ResponseTypes

type MainPageModel = {
  CardId: string
  Email: EmailAddress
  FirstName: NameString
  LastName: NameString
  Birtday: DateTime option
  Gender: Gender option
}
  with
    member this.Profile : Profile = {
      FirstName = this.FirstName
      LastName = this.LastName
      Birthday = this.Birtday
      Gender = this.Gender
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
  | UpdateProfile of UpdateProfileRequest
  | ChangeEmail of ChangeEmailRequest
  | ChangePassword of ChangePasswordRequest

type Pages = {
  MainPage: ViewElement
  EditProfilePage: ViewElement option
  ChangeEmailPage: ViewElement option
  ChangePasswordPage: ViewElement option
}

let createMain (r: ProfileResponse) =
  result {
    let cardId = r.CardId
    let! email = EmailAddress.create r.Email
    let! firstName = NameString.create r.FirstName
    let! lastName = NameString.create r.LastName
    let gender = Gender.fromString r.Gender

    return {
      CardId = cardId
      Email = email
      FirstName = firstName
      LastName = lastName
      Birtday = r.Birthday
      Gender = gender
    }
  }

let create r =
  result {
    let! main = createMain r
    return {
      MainPageModel = main
      EditProfilePageModel = None
      ChangeEmailPageModel = None
      ChangePasswordPageModel = None
    }
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
          =  mModel.MainPageModel.Profile
          |> EditProfileSubPage.create 
          |> Some
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
      let newModelOption, eMsg2 =
        match eMsg with
        | EditProfileSubPage.NoOp ->
          Some newModel,
          NoOp

        | EditProfileSubPage.UpdateProfile p ->
          None,
          UpdateProfile {
            FirstName = NameString.value p.FirstName
            LastName = NameString.value p.LastName
            Birthday = p.Birthday
            Gender = Gender.optionToString p.Gender
          }

      { mModel
        with
          EditProfilePageModel = newModelOption
      }, eMsg2
    | None -> mModel, NoOp

  | ChangeEmailPageMsg msg ->
    match mModel.ChangeEmailPageModel with
    | Some model ->
      let newModel, eMsg = ChangeEmailSubPage.update msg model
      let newModelOption, eMsg2, email =
        match eMsg with
        | ChangeEmailSubPage.NoOp ->
          Some newModel,
          NoOp,
          mModel.MainPageModel.Email

        | ChangeEmailSubPage.ChangeEmail d ->
          None,
          ChangeEmail {
            NewEmail = EmailAddress.value d.Email
            Password = Password.value d.Password
          },
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
        View.Label(
          text =
            sprintf
            "Hello %s %s!"
            (NameString.value mModel.FirstName)
            (NameString.value mModel.LastName))
        View.Label(text = mModel.CardId)
        View.Label(text = EmailAddress.value mModel.Email)
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
