module AllerRetour.SignUpSuccessPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open Resources
open Views

type Model = string

type Msg = ClickGoToSignIn

type ExternalMsg = GoToSignIn

let update msg (model: Model) =
  match msg with
  | ClickGoToSignIn -> model, GoToSignIn

let view model dispatch =
  View.MakeScrollStackPage(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    children = [
      Images.success
      |> makeCircle
      |> margin Thicknesses.bigLowerSpace

      makeInfoText "Success!"

      makeThinText (
        sprintf "We sent a confirmation link to your email %s." model
        + "Use it to confirm your ID.\nIt will be valid for 12 hours."
      )
      |> margin Thicknesses.mediumLowerSpace

      makeNavButton
        (bindPress dispatch ClickGoToSignIn)
        "log in"
      |> margin Thicknesses.mediumLowerSpace
    ]
  )
