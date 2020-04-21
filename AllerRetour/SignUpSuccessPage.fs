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
      View.MakeAvatar(
        source = Images.success,
        margin = Thicknesses.bigLowerSpace
      )

      View.MakeText("Success!")

      View.MakeThinText(
        text =
          sprintf "We sent a confirmation link to your email %s." model
          + "Use it to confirm your ID.\nIt will be valid for 12 hours.",

        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeTextButton(
        text = "log in",
        command = bindPress dispatch ClickGoToSignIn,
        margin = Thicknesses.mediumLowerSpace,
        fontFamily = Fonts.renogare
      )
    ]
  )
