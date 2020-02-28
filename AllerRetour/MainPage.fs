module AllerRetour.MainPage

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open TwoTrackResult
open PrimitiveTypes
open RequestTypes
open ResponseTypes
open Resources
open Views

type Page =
  | ForProfileEdit of EditProfileSubPage.Model
  | ForEmailChange of ChangeEmailSubPage.Model
  | ForPasswordChange of ChangePasswordSubPage.Model

type Model = {
  CardId: string
  Email: EmailAddress
  FirstName: NameString
  LastName: NameString
  Birtday: DateTime option
  Gender: Gender option
  ActiveTabId: int
  PageStack: Page list
}
  with
    member this.Profile : Profile = {
      FirstName = this.FirstName
      LastName = this.LastName
      Birthday = this.Birtday
      Gender = this.Gender
    }

type Msg =
  | GoBack
  | ClickEditProfile
  | ClickChangeEmail
  | ClickChangePassword
  | ClickSignOut
  | ChangeTab of int
  | EditProfilePageMsg of EditProfileSubPage.Msg
  | ChangeEmailPageMsg of ChangeEmailSubPage.Msg
  | ChangePasswordPageMsg of ChangePasswordSubPage.Msg

type ExternalMsg =
  | NoOp
  | SignOut
  | UpdateProfile of UpdateProfileRequest
  | ChangeEmail of ChangeEmailRequest
  | ChangePassword of ChangePasswordRequest

let create (r: ProfileResponse) =
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
      ActiveTabId = 0
      PageStack = []
    }
  }

let updateModel model (r: ProfileResponse) =
  result {
    let! newModel = create r

    return {
      newModel
      with
        ActiveTabId = model.ActiveTabId
        PageStack = model.PageStack
    }
  }

let update msg model =
  match msg with
  | GoBack ->
    { model with PageStack = model.PageStack.Tail }, NoOp

  | ChangeTab i ->
    { model with ActiveTabId = i }, NoOp

  | ClickEditProfile ->
    let p = ForProfileEdit (EditProfileSubPage.create model.Profile)
    { model with PageStack = p::model.PageStack }, NoOp

  | ClickChangeEmail ->
    let e = ForEmailChange (ChangeEmailSubPage.create model.Email)
    { model with PageStack = e::model.PageStack }, NoOp

  | ClickChangePassword ->
    let p = ForPasswordChange ChangePasswordSubPage.initModel
    { model with PageStack = p::model.PageStack }, NoOp

  | ClickSignOut ->
    model, SignOut

  | EditProfilePageMsg nMsg ->
    match model.PageStack.Head with
    | ForProfileEdit pModel ->
      let newModel, eMsg = EditProfileSubPage.update nMsg pModel
      let newModelOption, eMsg2 =
        match eMsg with
        | EditProfileSubPage.NoOp ->
          Some newModel, NoOp

        | EditProfileSubPage.UpdateProfile p ->
          None,
          UpdateProfile {
            FirstName = NameString.value p.FirstName
            LastName = NameString.value p.LastName
            Birthday = p.Birthday
            Gender = Gender.optionToString p.Gender
          }

        | EditProfileSubPage.GoBack ->
          None, NoOp

      { model
        with
          PageStack =
            (newModelOption
            |> Option.map ForProfileEdit
            |> Option.toList)
            @  model.PageStack.Tail
      }, eMsg2

    | _ ->
      model, NoOp

  | ChangeEmailPageMsg nMsg ->
    match model.PageStack.Head with
    | ForEmailChange eModel ->
      let newModel, eMsg = ChangeEmailSubPage.update nMsg eModel
      let newModelOption, eMsg2, email =
        match eMsg with
        | ChangeEmailSubPage.NoOp ->
          Some newModel, NoOp, model.Email

        | ChangeEmailSubPage.ChangeEmail d ->
          None,
          ChangeEmail {
            NewEmail = EmailAddress.value d.Email
            Password = Password.value d.Password
          },
          d.Email

        | ChangeEmailSubPage.GoBack ->
          None, NoOp, model.Email

      { model
        with
          PageStack =
            (newModelOption
            |> Option.map ForEmailChange
            |> Option.toList)
            @  model.PageStack.Tail
      }, eMsg2

    | _ ->
      model, NoOp

  | ChangePasswordPageMsg nMsg ->
    match model.PageStack.Head with
    | ForPasswordChange pModel ->
      let newModel, eMsg = ChangePasswordSubPage.update nMsg pModel
      let newModelOption, eMsg2 =
        match eMsg with
        | ChangePasswordSubPage.NoOp ->
          Some newModel, NoOp

        | ChangePasswordSubPage.ChangePassword r ->
          None, ChangePassword r

        | ChangePasswordSubPage.GoBack ->
          None, NoOp

      { model
        with
          PageStack =
            (newModelOption
            |> Option.map ForPasswordChange
            |> Option.toList)
            @  model.PageStack.Tail
      }, eMsg2

    | _ ->
      model, NoOp

let tabs = [
  Images.profileIcon, Images.profileIconActive, "home"
  Images.profileIcon, Images.profileIconActive, "notifications"
  Images.profileIcon, Images.profileIconActive, "my card"
  Images.profileIcon, Images.profileIconActive, "search"
  Images.profileIcon, Images.profileIconActive, "profile"
]

let tabCount = List.length tabs

let makeTab column isLast isActive dispatch icon iconActive title =
  let c = if isActive then Colors.accent else Colors.backgroundDark

  [
    View.Image(
      source = (if isLast then Images.tabLight else Images.tabLightShadow),
      aspect = Aspect.Fill
    ).Column(column)

    View.Grid(
      rowdefs = [Star; Auto],
      rowSpacing = 0.,
      columnSpacing = 0.,
      horizontalOptions = LayoutOptions.Center,
      verticalOptions = LayoutOptions.Center,
      padding = Thickness 2.,
      children = [
        View.Image(
          source = (if isActive then iconActive else icon),
          aspect = Aspect.AspectFill
        )
        |> horizontalOptions LayoutOptions.Center
        |> verticalOptions LayoutOptions.Center
        |> margin (Thickness 6.)

        (makeText FontSizes.xtrasmall Fonts.renogare Opacities.opaque title).Row(1)
        |> textColor c
        |> horizontalOptions LayoutOptions.Center
        |> verticalOptions LayoutOptions.Center
        |> margin (Thickness (2., 0., 2., 10.))
      ],
      gestureRecognizers = [
        View.TapGestureRecognizer(command = dispatch column)
      ]
    ).Column(column)
  ]

let homePage =
  fix (fun () ->
    makeScrollStack LayoutOptions.CenterAndExpand [
      View.Grid(
        rowSpacing = 0.,
        columnSpacing = 0.,
        coldefs = List.replicate 3 Star,
        margin = Thickness (20., 35., 20., 0.),
        children = [
          makeDuoStack
            (makeCircle2 (screenWidth() * 0.2) (makeInfoText "0,00"))
            (makeInfoText "last month"
            |> margin Thicknesses.eight)

          (makeDuoStack
            ((makeCircle2 (screenWidth() * 0.3) (makeHomeText "0,00")))
            ((makeInfoText "total")
            |> margin Thicknesses.eight)).Column(1)

          (makeDuoStack
            ((makeCircle2 (screenWidth() * 0.2) (makeInfoText "0,00")))
            ((makeInfoText "current balance")
            |> margin Thicknesses.eight)).Column(2)
        ]
      )
      
      View.Image(
        source = Images.list,
        aspect = Aspect.AspectFill
      )
      |> horizontalOptions LayoutOptions.Center
      |> margin (Thickness (20., 20., 20., 70.))
    ]
  )

let stub =
  fix (
    fun () ->
      makeScrollStack LayoutOptions.CenterAndExpand [
        makeInfoText "Coming soon..."
      ]
  )

let divider =
  makeHorizontalDivider ()
  |> margin (Thickness (20., 0.))
  |> verticalOptions LayoutOptions.Start

let view model (dispatch: Msg -> unit) =
  let cardStub =
    dependsOn (model.CardId) (
      fun model (cardId) ->
        makeScrollStack LayoutOptions.CenterAndExpand [
          makeInfoText "Your card id"
          makeHomeText (String.Format("{0:0000 0000 0000 0000}", Int64.Parse(cardId)))
        ]
    )

  let profilePage =
    dependsOn
      (model.CardId, model.FirstName, model.LastName, model.Email)
      (fun model (cardId, firstName, lastName, email) ->
        makeScrollStack LayoutOptions.StartAndExpand [
          View.Grid(
            rowSpacing = 0.,
            columnSpacing = 0.,
            coldefs = [Auto; Star],
            margin = (Thickness (20., 35., 20., 50.)),
            children = [
              QRCode.create cardId

              View.StackLayout(
                children = [
                  (makeHomeText (NameString.value firstName + " " + NameString.value lastName))
                  |> margin (Thickness 4.)
                  |> horizontalOptions LayoutOptions.Start

                  (makeInfoText (EmailAddress.value email))
                  |> margin (Thickness 4.)
                  |> horizontalOptions LayoutOptions.Start
                ]
              ).Column(1)
              |> horizontalOptions LayoutOptions.Start
              |> verticalOptions LayoutOptions.Start
              |> margin (Thickness (10., 0., 0., 0.))
            ]
          )
          |> verticalOptions LayoutOptions.StartAndExpand

          makeProfilePageButton
            (bindPress dispatch ClickEditProfile)
            "Edit profile"

          divider

          makeProfilePageButton
            (bindPress dispatch ClickChangeEmail)
            "Change email"

          divider

          makeProfilePageButton
            (bindPress dispatch ClickChangePassword)
            "Change password"

          divider
          
          makeProfilePageButton
            (bindPress dispatch ClickSignOut)
            "Log out"
        ])

  let tabsView =
    dependsOn
      (model.ActiveTabId)
      (fun model (activeTabId) ->
        View.Grid(
          coldefs = List.replicate 5 Star,
          rowSpacing = 0.,
          columnSpacing = 0.,
          verticalOptions = LayoutOptions.End,
          height = screenHeight () * 0.1,
          children = [
            for t
              in List.mapi
                (
                  fun i (icon, iconActive, title) ->
                    makeTab
                      i
                      (tabCount - 1 = i)
                      (activeTabId = i)
                      (fun i () -> i |> ChangeTab |> dispatch)
                      icon
                      iconActive
                      title
                ) tabs
              do
              yield! t
          ]
        )
      )

  View.ContentPage(
    useSafeArea = true,
    content = View.Grid(
      rowSpacing = 0.,
      columnSpacing = 0.,
      children = [
        yield!
          if not model.PageStack.IsEmpty then
            [
              match model.PageStack.Head with
              | ForProfileEdit pm ->
                EditProfileSubPage.view
                  pm
                  (EditProfilePageMsg >> dispatch)

              | ForEmailChange em ->
                ChangeEmailSubPage.view
                  em
                  (ChangeEmailPageMsg >> dispatch)

              | ForPasswordChange pm ->
                ChangePasswordSubPage.view
                  pm
                  (ChangePasswordPageMsg >> dispatch)
            ]
          else
            [
              match model.ActiveTabId with
              | 0 -> homePage
              | 1 | 3 -> stub
              | 2 -> cardStub
              | 4 -> profilePage
              | _ -> stub

              tabsView
            ]
      ]
    )
  )
