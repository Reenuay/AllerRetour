module AllerRetour.EditProfileSubPage

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open AllerRetour.Controls
open Views

type Model = {
  FirstName: Validatable<NameString, string>
  LastName: Validatable<NameString, string>
  Birthday: DateTime option
  Gender: Gender option
  PreviousProfile: Profile
  SelectedGenderIndex: int
}
with
  member this.ToDto() : Profile option =
    match this.FirstName, this.LastName with
    | Success f, Success l ->
      Some {
        FirstName = f
        LastName = l
        Birthday = this.Birthday
        Gender = this.Gender
      }
    | _ ->
      None

  member this.IsValid() =
    match this.FirstName, this.LastName with
    | Success f, Success l ->
      f <> this.PreviousProfile.FirstName
      && l <> this.PreviousProfile.LastName
      && this.Gender <> this.PreviousProfile.Gender
      && this.Birthday <> this.PreviousProfile.Birthday

    | _ -> false

  member this.Revalidate() = {
    this with
      FirstName = adaptV NameString.create (underV NameString.value this.FirstName)
      LastName = adaptV (NameString.create) (underV NameString.value this.LastName)
  }

type Msg =
  | SetFirstName of string
  | SetLastName of string
  | SetBirthday of DateTime option
  | SetGender of int * string option
  | ClickSave

type ExternalMsg =
  | NoOp
  | UpdateProfile of Profile

let genderList = ["Male"; "Female"]

let fromGenderOption = function
| Some x ->
  match x with
  | Male -> List.findIndex (fun x -> x = "Male") genderList
  | Female -> List.findIndex (fun x -> x = "Female") genderList
| None -> -1

let create (profile: Profile) = {
  FirstName = Success profile.FirstName
  LastName = Success profile.LastName
  Birthday = profile.Birthday
  Gender = profile.Gender
  PreviousProfile = profile
  SelectedGenderIndex = fromGenderOption profile.Gender
}

let update msg (model: Model) =
  match msg with
  | SetFirstName f ->
    { model with FirstName = adaptV NameString.create f }, NoOp
  | SetLastName l ->
    { model with LastName = adaptV NameString.create l }, NoOp
  | SetBirthday b ->
    { model with Birthday = b }, NoOp
  | SetGender (i, s) ->
    {
      model with
        Gender = Gender.fromStringOption s
        SelectedGenderIndex = i
    }, NoOp
  | ClickSave ->
    match model.ToDto() with
    | Some p -> model, UpdateProfile p
    | None -> model.Revalidate(), NoOp

let view model dispatch =
  View.ContentPage(
    content = View.StackLayout(
      children = [
        makeEntry
          None
          "First name"
          None
          NameString.value
          (fun args -> dispatch (SetFirstName args.NewTextValue))
          model.FirstName
        
        makeEntry
          None
          "Last name"
          None
          NameString.value
          (fun args -> dispatch (SetLastName args.NewTextValue))
          model.LastName

        View.OptionalDatePicker(
          minimumDate = DateTime.Today.AddYears(-120),
          maximumDate = DateTime.Today.AddYears(-18),
          optionalDate = model.Birthday,
          dateSelected=(fun args -> dispatch (SetBirthday (Some args.NewDate)))
        )

        View.Picker(
          title = "Gender",
          selectedIndex = model.SelectedGenderIndex,
          items = genderList,
          selectedIndexChanged = (fun (i, item) -> dispatch (SetGender (i, item)))
        )

        View.Button(
          text = "Save",
          command = (fun () -> dispatch ClickSave)
        )
      ]
    )
  )
