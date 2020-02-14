module AllerRetour.Views

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open Resources

let screenSize () = Device.Info.ScaledScreenSize
let screenWidth () = (screenSize ()).Width
let screenHeight () = (screenSize ()).Height

let bindPress dispatch msg () = dispatch msg

let bindNewText dispatch msg (args: TextChangedEventArgs) =
  args.NewTextValue
  |> msg
  |> dispatch
  
let makeLogo () =
  View.Image(
    source = Images.logo,
    width = screenWidth () * 0.5,
    margin = Thicknesses.bigUpperSpace
  )

let makeLabel text =
  View.Label(
    text = text,
    fontSize = FontSizes.big,
    textColor = Colors.accent,
    fontFamily = Fonts.renogare,
    horizontalTextAlignment = TextAlignment.Center,
    margin = Thicknesses.bigUpperSpace
  )

let makeThinText text =
  View.Label(
    text = text,
    opacity = Opacities.light,
    fontSize = FontSizes.thin,
    textColor = Colors.accent,
    fontFamily = Fonts.segoeUiLight,
    horizontalTextAlignment = TextAlignment.Center
  )

let makeEntry passwordOptions placeholder image fSuccess dispatch v =
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
    width = screenWidth () * 0.8,
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
          textChanged = dispatch,
          opacity = Opacities.light,
          placeholderColor = Colors.accent,
          fontSize = FontSizes.light,
          textColor = Colors.accent,
          fontFamily = Fonts.segoeUiLight
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
          |> margin Thicknesses.entryError
          |> horizontalTextAlignment TextAlignment.Start
    ]
  )

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
