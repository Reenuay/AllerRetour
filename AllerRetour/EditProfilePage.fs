module AllerRetour.EditProfilePage

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open AllerRetour.Controls

type Model = {
  GenderList: string list
  Profile: Profile
  SelectedGenderIndex: int
}

type Msg =
  | SetFirstName of string
  | SetLastName of string
  | SetBirthday of DateTime option
  | SetGender of int * string option
  | ClickSave

type ExternalMsg =
  | NoOp
  | UpdateProfile

let initModel = {
  GenderList = ["Male"; "Female"]
  Profile = Profile.Empty
  SelectedGenderIndex = -1
}

let setProfile model profile =
  { model with Profile = profile }

let toGenderOption = function
| Some x ->
  match x with
  | "Male" -> Some Male
  | "Female" -> Some Female
  | _ -> None
| None -> None

let update msg model =
  match msg with
  | SetFirstName f -> setProfile model { model.Profile with FirstName = f }, NoOp
  | SetLastName l -> setProfile model { model.Profile with LastName = l }, NoOp
  | SetBirthday b -> setProfile model { model.Profile with Birthday = b }, NoOp
  | SetGender (i, s) ->
    {
      model with
        Profile = { model.Profile with Gender = toGenderOption s }
        SelectedGenderIndex = i
    }, NoOp
  | ClickSave -> model, UpdateProfile

let view model dispatch =
  View.ContentPage(
    content = View.StackLayout(
      children = [
        View.Entry(
          text = model.Profile.FirstName,
          placeholder = "First name",
          textChanged = (fun args -> dispatch (SetFirstName args.NewTextValue)))
        View.Entry(
          text = model.Profile.LastName,
          placeholder = "Last name",
          textChanged = (fun args -> dispatch (SetLastName args.NewTextValue)))
        View.OptionalDatePicker(
          minimumDate = DateTime.Today.AddYears(-120),
          maximumDate = DateTime.Today.AddYears(-18),
          optionalDate = model.Profile.Birthday,
          dateSelected=(fun args -> dispatch (SetBirthday (Some args.NewDate))))
        View.Picker(
          title = "Gender",
          selectedIndex = model.SelectedGenderIndex,
          items = model.GenderList,
          selectedIndexChanged = (fun (i, item) -> dispatch (SetGender (i, item)))
        )
        View.Button(
          text = "Save",
          command = (fun () -> dispatch ClickSave))
      ]
    )
  )
