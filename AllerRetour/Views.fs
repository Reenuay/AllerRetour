module AllerRetour.Views

open System
open Fabulous.XamarinForms
open Xamarin.Forms
open Resources

let screenSize () = Device.Info.ScaledScreenSize
let screenWidth () = (screenSize ()).Width
let screenHeight () = (screenSize ()).Height
let screenWidthP percent = screenWidth () * percent

let bindClick dispatch msg () = dispatch msg

let bindNewText dispatch msg (args: TextChangedEventArgs) =
  args.NewTextValue
  |> msg
  |> dispatch

type View with
  static member MakeText
    (
      text,
      ?margin,
      ?opacity,
      ?fontSize,
      ?verticalOptions,
      ?horizontalOptions,
      ?horizontalTextAlignment
    ) =
    let fontSize = Option.defaultValue FontSizes.light fontSize
    let horizontalTextAlignment = Option.defaultValue TextAlignment.Center horizontalTextAlignment

    View.Label(
      text = text,
      ?margin = margin,
      ?opacity = opacity,
      fontSize = fontSize,
      textColor = Colors.accent,
      fontFamily = Fonts.segoeUiLight,
      ?verticalOptions = verticalOptions,
      ?horizontalOptions = horizontalOptions,
      horizontalTextAlignment = horizontalTextAlignment
    )

  static member MakeThinText
    (
      text,
      ?margin,
      ?horizontalTextAlignment
    ) =
    View.MakeText(
      text = text,
      ?margin = margin,
      opacity = Opacities.light,
      fontSize = FontSizes.thin,
      ?horizontalTextAlignment = horizontalTextAlignment
    )

  static member MakeButton
    (
      text,
      command,
      ?isEnabled,
      ?margin
    ) =
      let isEnabled = Option.defaultValue true isEnabled

      let ifEnabled enabled disabled =
         if isEnabled then enabled else disabled

      View.Button(
        text = text,
        height = 32.,
        command = command,
        cornerRadius = 32,
        fontSize = FontSizes.light,
        fontFamily = Fonts.renogare,
        borderColor = Colors.accent,
        padding = Thickness (40., 0., 40., -4.),
        horizontalOptions = LayoutOptions.Center,
        textColor = ifEnabled Colors.backgroundLight Colors.accent,
        backgroundColor = ifEnabled Colors.accent Color.Transparent,
        opacity = ifEnabled Opacities.opaque Opacities.light,
        borderWidth = ifEnabled 0. 1.,
        ?margin = margin
      )

  static member MakeTextButton
    (
      text,
      command,
      ?margin,
      ?fontFamily,
      ?horizontalOptions
    ) =
    let fontFamily = Option.defaultValue Fonts.segoeUiLight fontFamily
    let horizontalOptions = Option.defaultValue LayoutOptions.Center horizontalOptions

    View.Label(
      text = text,
      ?margin = margin,
      padding = Thicknesses.zero,
      textColor = Colors.accent,
      fontSize = FontSizes.light,
      fontFamily = fontFamily,
      horizontalOptions = horizontalOptions,
      gestureRecognizers = [
        View.TapGestureRecognizer(
          command = command
        )
      ]
    )

  static member MakeEntry
    (
      value,
      placeholder,
      textChanged,
      ?passwordOptions,
      ?keyboard,
      ?image,
      ?margin
    ) =
      let isPassword = function
      | Some ( isSet, _ ) ->
        isSet

      | None ->
        false

      let text =
        Validatable.getInput value

      let state =
        Validatable.getState value

      View.Grid(
        coldefs = [Auto; Star],
        rowdefs = [Auto; Auto],
        rowSpacing = 0.,
        columnSpacing = 0.,
        width = screenWidthP 0.8,
        ?margin = margin,
        children = [
          if Option.isSome image then
            yield
              View.Image(
                source = Option.get image,
                opacity = Opacities.light,
                margin = Thickness (0., 0., 5., 0.),
                width = 20.
              )

          yield
            View.Entry(
              text = text,
              isPassword = isPassword passwordOptions,
              placeholder = placeholder,
              textChanged = debounce 500 textChanged,
              opacity = Opacities.light,
              placeholderColor = Colors.accent,
              fontSize = FontSizes.light,
              textColor = Colors.accent,
              fontFamily = Fonts.segoeUiLight,
              keyboard = Option.defaultValue Keyboard.Default keyboard
            )
              .Column(1)

          if Option.isSome passwordOptions then
            yield
              View.Image(
                source = (
                  if isPassword passwordOptions then
                    Images.eyeIcon
                  else
                    Images.eyeCrossedIcon
                ),
                opacity = Opacities.light,
                width = 20.,
                aspect = Aspect.AspectFit,
                margin = Thickness (0., 0., 5., 0.),
                backgroundColor = Color.Transparent,
                horizontalOptions = LayoutOptions.End,
                gestureRecognizers = [
                  View.TapGestureRecognizer(
                    command = (passwordOptions |> Option.get |> snd)
                  )
                ]
              )
                .Column(1)

          match state with
          | Invalid errors ->
            let
              errorString =
                List.fold (fun acc error -> acc + "\n" + error) "" errors
            in
            yield
              View.MakeThinText(
                text = errorString,
                margin = Thickness (4., -15., 4., 0.),
                horizontalTextAlignment = TextAlignment.Start
              )
                .Row(1)
                .Column(1)

          | _ ->
            ()
        ]
      )

  static member MakeAvatar
    (
      source,
      ?margin
    ) =
    let bigRadius = screenWidthP 0.6
    let littleRadius = bigRadius * 0.75
    let margin = Option.defaultValue Thicknesses.zero margin
    let c = Colors.accent

    let circle radius margin content =
      View.Frame(
        width = radius,
        height = radius,
        margin = margin,
        content = content,
        padding = Thicknesses.zero,
        cornerRadius = radius,
        verticalOptions = LayoutOptions.Center,
        backgroundColor = Color.FromRgba(c.R, c.G, c.B, Opacities.ten),
        horizontalOptions = LayoutOptions.Center
      )

    View.Image(
      source = source,
      verticalOptions = LayoutOptions.Center,
      horizontalOptions = LayoutOptions.Center
    )
    |> widthRequest (littleRadius * 0.65)
    |> circle littleRadius Thicknesses.zero
    |> circle bigRadius margin

  static member MakeCircle
    (
      content
    ) =
    View.Frame(
      margin = Thicknesses.zero,
      content = content,
      padding = Thicknesses.zero,
      borderColor = Colors.accent,
      cornerRadius = 1000.,
      backgroundColor = Color.Transparent,
      verticalOptions = LayoutOptions.Fill,
      horizontalOptions = LayoutOptions.Fill,
      created =
        fun frame ->
          frame.SizeChanged.Add(
            fun _ ->
              if frame.Width <> frame.Height then
                frame.HeightRequest <- frame.Width
          )
    )
    |> borderWidth 1.

  static member MakeScrollStack
    (
      ?children,
      ?isDarkTheme,
      ?dispatchBack,
      ?verticalOptions,
      ?horizontalOptions
    ) =
    let isDarkTheme = Option.defaultValue false isDarkTheme
    let allowBackButton = Option.isSome dispatchBack
    let horizontalOptions = Option.defaultValue LayoutOptions.CenterAndExpand horizontalOptions

    let scrollView =
      View.ScrollView(
        content =
          View.StackLayout(
            horizontalOptions = horizontalOptions,
            padding = Thicknesses.bigUpperSpace,
            ?verticalOptions = verticalOptions,
            ?children = children
          )
      )

    View.AbsoluteLayout(
      verticalOptions = LayoutOptions.Fill,
      children = [
        View.Image(
          source = (if isDarkTheme then Images.backgroundDark else Images.backgroundLight),
          aspect = Aspect.AspectFill
        )
          .LayoutFlags(AbsoluteLayoutFlags.PositionProportional ||| AbsoluteLayoutFlags.WidthProportional)
          .LayoutBounds(Rectangle(0., 0., 1., screenHeight()))

        (
          if allowBackButton then
            View.Grid(
              rowdefs = [Auto; Star],
              rowSpacing = 0.,
              columnSpacing = 0.,
              children = [
                scrollView
                  .RowSpan(2)

                View.Image(
                  source = Images.backButton,
                  width = 15.,
                  aspect = Aspect.AspectFit,
                  margin = Thickness(30., 50., 0., 0.),
                  horizontalOptions = LayoutOptions.Start,
                  opacity = Opacities.light,
                  gestureRecognizers = [
                    View.TapGestureRecognizer(
                      command = Option.get dispatchBack
                    )
                  ],
                  effects = [
                    View.ShadowEffect(10.)
                  ]
                )
              ]
            )
          else
            scrollView
        )
          .LayoutFlags(AbsoluteLayoutFlags.All)
          .LayoutBounds(Rectangle(0., 0., 1., 1.))
      ]
    )
