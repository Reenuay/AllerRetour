module AllerRetour.EditProfileSubPage

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open AllerRetour.Controls
open Resources
open Views

type Model =
  private
    {
      FirstName: Validatable<Name>
      LastName: Validatable<Name>
      Birthday: DateTime option
      Gender: Gender option
      OldProfile: Profile
      SelectedGenderIndex: int
    }

type Msg =
  private
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

let private tryGetProfile model =
  let
    fields =
      (
        Validatable.tryValue model.FirstName,
        Validatable.tryValue model.LastName
      )
  in
  match fields with
  | ( Some firstName, Some lastName )
    when firstName <> model.OldProfile.FirstName
    || lastName <> model.OldProfile.LastName
    || model.Gender <> model.OldProfile.Gender
    || model.Birthday <> model.OldProfile.Birthday ->

    Some
      {
        FirstName = firstName
        LastName = lastName
        Birthday = model.Birthday
        Gender = model.Gender
      }

  | _ ->
    None

let private genderList = [ "Male"; "Female" ]

let private fromGenderOption = function
| Some gender ->
  List.findIndex
    ((=) (Gender.toString gender))
    genderList

| None ->
  -1

let create (profile: Profile) =
  {
    FirstName = Validatable.empty <| Name.value profile.FirstName
    LastName = Validatable.empty <| Name.value profile.LastName
    Birthday = profile.Birthday
    Gender = profile.Gender
    OldProfile = profile
    SelectedGenderIndex = fromGenderOption profile.Gender
  }

let update msg (model: Model) =
  match msg with
  | SetFirstName f ->
    { model with FirstName = Name.validate f }, NoOp

  | SetLastName l ->
    { model with LastName = Name.validate l }, NoOp

  | SetBirthday b ->
    { model with Birthday = b }, NoOp

  | SetGender (i, s) ->
    {
      model with
        Gender = Gender.fromStringOption s
        SelectedGenderIndex = i
    }, NoOp

  | ClickSave ->
    match tryGetProfile model with
    | Some profile ->
      ( model, UpdateProfile profile )

    | _ ->
      let
        firstName =
          Name.revalidate model.FirstName
      let
        lastName =
          Name.revalidate model.LastName
      let
        newModel =
          {
            model with
              FirstName = firstName
              LastName = lastName
          }
      in
      ( newModel, NoOp )

  | ClickGoBack ->
    ( model, GoBack )

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
        (bindNewText dispatch SetFirstName),
        image = Images.userIcon
      )

      View.MakeEntry(
        model.LastName,
        "Last name",
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

      let
        isEnabled =
          tryGetProfile model
          |> Option.isSome
      in
      View.MakeButton(
        text = "save",
        margin = Thicknesses.mediumLowerSpace,
        command = bindClick dispatch ClickSave,
        isEnabled = isEnabled
      )
    ]
  )
