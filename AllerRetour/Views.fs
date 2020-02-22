module AllerRetour.Views

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
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

let makeInfoText = makeText FontSizes.light Fonts.segoeUiLight Opacities.opaque

let makeThinText = makeText FontSizes.thin Fonts.segoeUiLight Opacities.light

let makeEntry passwordOptions keyboard placeholder image fSuccess dispatch v =
  let isPassword = function
  | Some (isSet, _) -> isSet
  | None -> false

  let text, error =
    match v with
    | Success x -> fSuccess x, ""
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

let makeTextButton font command text =
  View.Label(
    text = text,
    fontFamily = font,
    fontSize = FontSizes.light,
    textColor = Colors.accent,
    padding = Thicknesses.zero,
    horizontalOptions = LayoutOptions.Center,
    gestureRecognizers = [
      View.TapGestureRecognizer(
        command = command
      )
    ]
  )

let makeLink = makeTextButton Fonts.segoeUiLight

let makeNavButton = makeTextButton Fonts.renogare

let makeDuoGrid (v1: ViewElement) (v2: ViewElement) =
  View.Grid(
    coldefs = [Star; Star],
    rowSpacing = 0.,
    columnSpacing = 0.,
    width = screenWidthP 0.8,
    children = [
      v1.Column(0)
        |> horizontalOptions LayoutOptions.Start
        |> verticalOptions LayoutOptions.Center

      v2.Column(1)
        |> horizontalOptions LayoutOptions.End
        |> verticalOptions LayoutOptions.Center
    ]
  )

let makeCircle image =
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

  image
  |> widthRequest (littleRadius * 0.65)
  |> horizontalOptions LayoutOptions.Center
  |> verticalOptions LayoutOptions.Center
  |> circle littleRadius
  |> circle bigRadius

let makePage children =
  View.ContentPage(
    content = View.ScrollView(
      content = View.Grid(
        children = [
          View.Image(
            source = Images.backgroundDark,
            aspect = Aspect.AspectFill
          )
          View.StackLayout(
            horizontalOptions = LayoutOptions.CenterAndExpand,
            verticalOptions = LayoutOptions.CenterAndExpand,
            children = children
          )
        ]
      )
    )
  )
