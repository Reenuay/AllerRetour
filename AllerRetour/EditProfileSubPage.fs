module AllerRetour.EditProfileSubPage

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open AllerRetour.Controls
open Resources
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
  member this.IsValid() =
    match this.FirstName, this.LastName with
    | Ok f, Ok l ->
      f <> this.PreviousProfile.FirstName
      || l <> this.PreviousProfile.LastName
      || this.Gender <> this.PreviousProfile.Gender
      || this.Birthday <> this.PreviousProfile.Birthday

    | _ -> false

  member this.ToDto() : Profile option =
    match this.IsValid() with
    | true ->
      Some {
        FirstName = Result.getOk this.FirstName
        LastName = Result.getOk this.LastName
        Birthday = this.Birthday
        Gender = this.Gender
      }
    | false ->
      None

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
  | ClickGoBack
  | ClickSave

type ExternalMsg =
  | NoOp
  | GoBack
  | UpdateProfile of Profile

let genderList = ["Male"; "Female"]

let fromGenderOption = function
| Some x ->
  match x with
  | Male -> List.findIndex (fun x -> x = "Male") genderList
  | Female -> List.findIndex (fun x -> x = "Female") genderList
| None -> -1

let create (profile: Profile) = {
  FirstName = Ok profile.FirstName
  LastName = Ok profile.LastName
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

  | ClickGoBack ->
    model, GoBack

let view model dispatch =
  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    dispatchBack = bindClick dispatch ClickGoBack,
    verticalOptions = LayoutOptions.StartAndExpand,
    children = [
      View.MakeAvatar(
        source = Images.profile,
        margin = Thicknesses.mediumUpperBigLowerSpace
      )

      View.MakeEntry(
        model.FirstName,
        "First name",
        NameString.value,
        (bindNewText dispatch SetFirstName),
        image = Images.userIcon
      )

      View.MakeEntry(
        model.LastName,
        "Last name",
        NameString.value,
        (bindNewText dispatch SetLastName),
        image = Images.userIcon
      )

      View.OptionalDatePicker(
        minimumDate = DateTime.Today.AddYears(-120),
        maximumDate = DateTime.Today.AddYears(-18),
        optionalDate = model.Birthday,
        textColor = Colors.accent,
        fontSize = FontSizes.light,
        fontFamily = Fonts.segoeUiLight,
        dateSelected = (fun args -> dispatch (SetBirthday (Some args.NewDate)))
      )
      |> opacity Opacities.light

      View.Picker(
        title = "Gender",
        selectedIndex = model.SelectedGenderIndex,
        items = genderList,
        opacity = Opacities.light,
        textColor = Colors.accent,
        fontSize = FontSizes.light,
        fontFamily = Fonts.segoeUiLight,
        titleColor = Colors.accent,
        selectedIndexChanged = (fun (i, item) -> dispatch (SetGender (i, item)))
      )
      |> margin Thicknesses.mediumLowerSpace

      View.MakeButton(
        text = "save",
        command = bindClick dispatch ClickSave,
        isEnabled = model.IsValid(),
        margin = Thicknesses.mediumLowerSpace
      )
    ]
  )
