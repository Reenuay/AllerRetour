module AllerRetour.Views

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open PrimitiveTypes
open Resources

let screenSize () = Device.Info.ScaledScreenSize
let screenWidth () = (screenSize ()).Width
let screenHeight () = (screenSize ()).Height
let screenWidthP percent = screenWidth () * percent

let bindPress dispatch msg () = dispatch msg

let bindNewText dispatch msg (args: TextChangedEventArgs) =
  args.NewTextValue
  |> msg
  |> dispatch
  
let makeLogo () =
  View.Image(
    source = Images.logo,
    width = screenWidthP 0.5,
    margin = Thicknesses.bigUpperSpace
  )

let makeText fontSize fontFamily opacity text =
  View.Label(
    text = text,
    opacity = opacity,
    fontSize = fontSize,
    textColor = Colors.accent,
    fontFamily = fontFamily,
    horizontalTextAlignment = TextAlignment.Center
  )

let makeLabel = makeText FontSizes.big Fonts.renogare Opacities.opaque

let makeHomeText = makeText FontSizes.medium Fonts.segoeUiLight Opacities.opaque

let makeInfoText = makeText FontSizes.light Fonts.segoeUiLight Opacities.opaque

let makeThinText = makeText FontSizes.thin Fonts.segoeUiLight Opacities.light

let makeEntry passwordOptions keyboard placeholder image map dispatch v =
  let isPassword = function
  | Some (isSet, _) -> isSet
  | None -> false

  let text, error =
    match v with
    | Success x -> map x, ""
    | Failure (v, l) -> v, foldErrors l

  View.Grid(
    coldefs = [Auto; Star],
    rowdefs = [Auto; Auto],
    rowSpacing = 0.,
    columnSpacing = 0.,
    width = screenWidthP 0.8,
    children = [
      if Option.isSome image then
        yield
          View.Image(
            source = Option.get image,
            opacity = Opacities.light,
            margin = Thicknesses.rightLittleSpace,
            width = 20.
          )

      yield
        View.Entry(
          text = text,
          isPassword = isPassword passwordOptions,
          placeholder = placeholder,
          textChanged = debounce 500 dispatch,
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
            margin = Thicknesses.rightLittleSpace,
            backgroundColor = Color.Transparent,
            horizontalOptions = LayoutOptions.End,
            gestureRecognizers = [
              View.TapGestureRecognizer(
                command = (passwordOptions |> Option.get |> snd)
              )
            ]
          )
          .Column(1)

      if error <> String.Empty then
        yield
          (makeThinText error)
          .Row(1)
          .Column(1)
          |> margin Thicknesses.paddingForEntryError
          |> horizontalTextAlignment TextAlignment.Start
    ]
  )

let makeButton isEnabled command text =
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
    padding = Thicknesses.paddingForButton,
    horizontalOptions = LayoutOptions.Center,
    textColor = ifEnabled Colors.backgroundLight Colors.accent,
    backgroundColor = ifEnabled Colors.accent Color.Transparent,
    opacity = ifEnabled Opacities.opaque Opacities.light,
    borderWidth = ifEnabled 0. 1.
  )

let makeTextButton font fontSize margin horizontalOptions command text =
  View.Label(
    text = text,
    fontFamily = font,
    fontSize = fontSize,
    textColor = Colors.accent,
    padding = Thicknesses.zero,
    horizontalOptions = horizontalOptions,
    margin = margin,
    gestureRecognizers = [
      View.TapGestureRecognizer(
        command = command
      )
    ]
  )

let makeLink =
  makeTextButton
    Fonts.segoeUiLight
    FontSizes.light
    Thicknesses.zero
    LayoutOptions.Center

let makeNavButton =
  makeTextButton
    Fonts.renogare
    FontSizes.light
    Thicknesses.zero
    LayoutOptions.Center

let makeDuoGrid (v1: ViewElement) (v2: ViewElement) =
  View.Grid(
    coldefs = [Star; Star],
    rowSpacing = 0.,
    columnSpacing = 0.,
    width = screenWidthP 0.8,
    horizontalOptions = LayoutOptions.CenterAndExpand,
    children = [
      v1.Column(0)
        |> horizontalOptions LayoutOptions.Start
        |> verticalOptions LayoutOptions.Center

      v2.Column(1)
        |> horizontalOptions LayoutOptions.End
        |> verticalOptions LayoutOptions.Center
    ]
  )

let makeCircle path =
  let bigRadius = screenWidthP 0.6
  let littleRadius = bigRadius * 0.75
  let a = Colors.accent

  let circle radius content =
    View.Frame(
      backgroundColor = Color.FromRgba(a.R, a.G, a.B, Opacities.ten),
      width = radius,
      height = radius,
      cornerRadius = radius,
      horizontalOptions = LayoutOptions.Center,
      verticalOptions = LayoutOptions.Center,
      padding = Thicknesses.zero,
      margin = Thicknesses.zero,
      content = content
    )

  View.Image(source = path)
  |> widthRequest (littleRadius * 0.65)
  |> horizontalOptions LayoutOptions.Center
  |> verticalOptions LayoutOptions.Center
  |> circle littleRadius
  |> circle bigRadius

let makeCircle2 radius content =
  View.Frame(
    backgroundColor = Color.Transparent,
    width = radius,
    height = radius,
    cornerRadius = radius,
    borderColor = Colors.accent,
    horizontalOptions = LayoutOptions.Center,
    verticalOptions = LayoutOptions.Center,
    padding = Thicknesses.zero,
    margin = Thicknesses.zero,
    content = (
      content
      |> horizontalOptions LayoutOptions.Center
      |> verticalOptions LayoutOptions.Center
    )
  )
  |> borderWidth 1.

let makeDuoStack first second =
  View.StackLayout(
    children = [
      first
      second
    ]
  )

let makeHorizontalDivider () =
  View.BoxView(
    height = 1.,
    backgroundColor = Colors.accent,
    horizontalOptions = LayoutOptions.FillAndExpand
  )

let makeBackButton dispatch =
  View.Image(
    source = Images.backButton,
    width = 15.,
    aspect = Aspect.AspectFit,
    margin = Thickness(0., 35., 0., 0.),
    horizontalOptions = LayoutOptions.Start,
    opacity = Opacities.light,
    gestureRecognizers = [
      View.TapGestureRecognizer(
        command = dispatch
      )
    ]
  )

let makeProfilePageButton command text =
  View.Button(
    fontFamily = Fonts.segoeUiLight,
    fontSize = FontSizes.light,
    margin = Thickness(20., 0.),
    padding = Thickness(0., 10.),
    horizontalOptions = LayoutOptions.FillAndExpand,
    verticalOptions = LayoutOptions.Start,
    command = command,
    text = text,
    backgroundColor = Color.Transparent,
    textColor = Colors.accent
  )

type View with
  static member MakeEntry
    (
      value,
      placeholder,
      map,
      dispatch,
      ?passwordOptions,
      ?keyboard,
      ?image
    ) =
    makeEntry passwordOptions keyboard placeholder image map dispatch value

  static member MakeScrollStack
    (
      ?children,
      ?isDarkTheme,
      ?verticalOptions
    ) =
    let isDarkTheme = Option.defaultValue false isDarkTheme

    View.AbsoluteLayout(
      verticalOptions = LayoutOptions.FillAndExpand,
      children = [
        View.Image(
          source = (if isDarkTheme then Images.backgroundDark else Images.backgroundLight),
          aspect = Aspect.AspectFill
        )
          .LayoutFlags(AbsoluteLayoutFlags.PositionProportional ||| AbsoluteLayoutFlags.WidthProportional)
          .LayoutBounds(Rectangle(0., 0., 1., screenHeight()))

        View.ScrollView(
          content =
            View.StackLayout(
              horizontalOptions = LayoutOptions.CenterAndExpand,
              ?verticalOptions = verticalOptions,
              ?children = children
            )
        )
          .LayoutFlags(AbsoluteLayoutFlags.All)
          .LayoutBounds(Rectangle(0., 0., 1., 1.))
      ]
    )

  static member MakeScrollStackPage
    (
      ?children,
      ?isDarkTheme,
      ?verticalOptions
    ) =
    let verticalOptions = Option.defaultValue LayoutOptions.FillAndExpand verticalOptions

    View.ContentPage(
      useSafeArea = true,
      content = View.MakeScrollStack(
        ?children = children,
        ?isDarkTheme = isDarkTheme,
        verticalOptions = verticalOptions
      )
    )
