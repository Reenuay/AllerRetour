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
  | ForSettings

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
  | ClickSettings
  | ClickSignOut
  | ChangeTab of int
  | EditProfilePageMsg of EditProfileSubPage.Msg
  | ChangeEmailPageMsg of ChangeEmailSubPage.Msg
  | ChangePasswordPageMsg of ChangePasswordSubPage.Msg
  | SettingsPageMsg of SettingsPage.Msg

type ExternalMsg =
  | NoOp
  | SignOut
  | UpdateProfile of UpdateProfileRequest
  | ChangeEmail of ChangeEmailRequest
  | ChangePassword of ChangePasswordRequest
  | SaveSettings

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

  | ClickSettings ->
    let s = ForSettings
    { model with PageStack = s::model.PageStack }, NoOp

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

  | SettingsPageMsg sMsg ->
    match sMsg with
    | SettingsPage.ChangeTheme t ->
      GlobalSettings.Change({ GlobalSettings.Settings with Theme = t })
      model, SaveSettings

    | SettingsPage.ClickGoBack ->
      { model with PageStack = model.PageStack.Tail }, NoOp

let tabs = [
  Images.mainIcon, Images.mainIconLight, Images.mainIconActive, "home"
  Images.notificationIcon, Images.notificationIconLight, Images.notificationIconActive, "notifications"
  Images.profileIcon, Images.profileIconLight, Images.profileIconActive, "my card"
  Images.searchIcon, Images.searchIconLight, Images.searchIconActive, "search"
  Images.profileIcon, Images.profileIconLight, Images.profileIconActive, "profile"
]

let tabCount = List.length tabs

let makeTab column isLast isActive dispatch icon iconActive title isDarkTheme =
  let c =
    if not isDarkTheme then
      Color.White
    else if isActive then
      Colors.accent
    else
      Colors.backgroundDark

  let opacity =
    if isActive || isDarkTheme then
      Opacities.light
    else
      0.6

  [
    View.Image(
      source = (
        if isLast then
          if isDarkTheme then
            Images.tabLight
          else
            Images.tabDark
        else
          if isDarkTheme then
            Images.tabLightShadow
          else
            Images.tabDarkShadow
      ),
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
          source = (
            if isActive && isDarkTheme then
              iconActive
             else
              icon
          ),
          opacity = opacity,
          aspect = Aspect.AspectFill,
          margin = Thickness 6.,
          horizontalOptions = LayoutOptions.Center,
          verticalOptions = LayoutOptions.Center
        )

        (makeText
          FontSizes.xtrasmall
          Fonts.renogare
          opacity
          title
          ).Row(1)
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

let divider =
  makeHorizontalDivider ()
  |> margin (Thickness (20., 0.))
  |> verticalOptions LayoutOptions.Start

let view model (dispatch: Msg -> unit) =
  let homePage =
    dependsOn
      (GlobalSettings.IsDarkTheme)
      (fun model isDarkTheme ->
        View.MakeScrollStack(
          isDarkTheme = isDarkTheme,
          verticalOptions = LayoutOptions.StartAndExpand,
          children = [
            View.Grid(
              rowSpacing = 0.,
              columnSpacing = 0.,
              coldefs = List.replicate 3 Star,
              margin = Thickness (20., 50., 20., 0.),
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
        
            View.CollectionView(
              margin = Thickness (0., 20., 0., 0.),
              itemsLayout = GridItemsLayout(ItemsLayoutOrientation.Vertical),
              items = [
                for (image, cashback)
                  in
                    [
                      (Images.zara, 5)
                      (Images.asos, 3)
                      (Images.amazon, 2)
                      (Images.reebok, 3)
                      (Images.aliexpress, 4)
                    ]
                  do
                  yield
                    View.Grid(
                      coldefs = [Auto; Star; Auto; Auto],
                      horizontalOptions = LayoutOptions.Fill,
                      height = 100.,
                      margin = Thicknesses.zero,
                      padding = Thickness (20., 3.),
                      children = [
                        View.BoxView(
                          backgroundColor = (
                            if isDarkTheme then
                              Colors.frontDark
                            else
                              Colors.frontLight
                          ),
                          horizontalOptions = LayoutOptions.Fill,
                          verticalOptions = LayoutOptions.Fill,
                          effects = [
                            View.ShadowEffect(10.)
                          ]
                        )
                          .ColumnSpan(4)

                        View.Image(
                          source = Images.star,
                          aspect = Aspect.AspectFill,
                          margin = Thickness (20., 35., 0., 35.)
                        )

                        View.Image(
                          source = image,
                          aspect = Aspect.AspectFit,
                          margin = Thickness (20., 35.)
                        )
                          .Column(1)

                        View.Label(
                          text = sprintf "%i" cashback,
                          fontSize = FontSize 60.,
                          textColor = Colors.accent,
                          fontFamily = Fonts.renogare,
                          horizontalTextAlignment = TextAlignment.Center,
                          horizontalOptions = LayoutOptions.Center,
                          verticalOptions = LayoutOptions.End,
                          margin = Thickness (0., 0., 0., 10.) 
                        )
                          .Column(2)

                        View.Label(
                          text = "%",
                          fontSize = FontSize 30.,
                          textColor = Colors.accent,
                          fontFamily = Fonts.renogare,
                          horizontalTextAlignment = TextAlignment.Center,
                          horizontalOptions = LayoutOptions.Center,
                          verticalOptions = LayoutOptions.End,
                          margin = Thickness (0., 0., 20., 17.)
                        )
                          .Column(3)

                        View.Label(
                          text = "cashback up to",
                          fontSize = FontSizes.xtrasmall,
                          opacity = Opacities.light,
                          textColor = Colors.accent,
                          fontFamily = Fonts.renogare,
                          horizontalTextAlignment = TextAlignment.Center,
                          horizontalOptions = LayoutOptions.Center,
                          verticalOptions = LayoutOptions.Start,
                          margin = Thickness (0., 10., 0., 0.)
                        )
                          .Column(2)
                          .ColumnSpan(2)
                      ]
                    )
              ]
            )
          ]
        )
      )

  let stub =
    dependsOn
      (GlobalSettings.IsDarkTheme)
      (fun model isDarkTheme ->
        View.MakeScrollStack(
          isDarkTheme = isDarkTheme,
          verticalOptions = LayoutOptions.CenterAndExpand,
          children = [
            makeInfoText "Coming soon..."
          ]
        )
      )

  let cardStub =
    dependsOn
      (model.CardId, GlobalSettings.IsDarkTheme)
      (fun model (cardId, isDarkTheme) ->
        View.MakeScrollStack(
          isDarkTheme = isDarkTheme,
          verticalOptions = LayoutOptions.CenterAndExpand,
          children = [
            makeInfoText "Your card id"
            makeHomeText (String.Format("{0:0000 0000 0000 0000}", Int64.Parse(cardId)))
          ]
        )
      )

  let profilePage =
    dependsOn
      (model.CardId, model.FirstName, model.LastName, model.Email, GlobalSettings.IsDarkTheme)
      (fun model (cardId, firstName, lastName, email, isDarkTheme) ->
        View.MakeScrollStack(
          isDarkTheme = isDarkTheme,
          verticalOptions = LayoutOptions.StartAndExpand,
          children = [
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
              (bindPress dispatch ClickSettings)
              "Settings"

            divider
          
            makeProfilePageButton
              (bindPress dispatch ClickSignOut)
              "Log out"
          ]
        )
      )

  let tabsView =
    dependsOn
      (model.ActiveTabId, GlobalSettings.IsDarkTheme)
      (fun model (activeTabId, isDarkTheme) ->
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
                  fun i (icon, iconLight, iconActive, title) ->
                    makeTab
                      i
                      (tabCount - 1 = i)
                      (activeTabId = i)
                      (fun i () -> i |> ChangeTab |> dispatch)
                      (if isDarkTheme then icon else iconLight)
                      iconActive
                      title
                      isDarkTheme
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

              | ForSettings ->
                SettingsPage.view (SettingsPageMsg >> dispatch)
            ]
          else
            [
              match model.ActiveTabId with
              | 0 -> homePage
              | 2 -> cardStub
              | 4 -> profilePage
              | _ -> stub

              tabsView
            ]
      ]
    )
  )
