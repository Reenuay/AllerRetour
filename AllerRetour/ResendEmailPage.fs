module AllerRetour.ResendEmailPage

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open Resources
open Views

type Model = string

type Msg =
  | ClickResendEmail
  | ClickGoToChangeEmail
  | ClickGoToSignIn

type ExternalMsg =
  | ResendEmail
  | GoToChangeEmail
  | GoToSignIn

let update msg (model: Model) =
  match msg with
  | ClickResendEmail -> model, ResendEmail
  | ClickGoToChangeEmail -> model, GoToChangeEmail
  | ClickGoToSignIn -> model, GoToSignIn

let view model dispatch =
  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    children = [
      View.MakeAvatar(
        source = Images.verificationCode,
        margin = Thicknesses.bigLowerSpace
      )

      View.MakeText("Please confirm your registered email ID")

      View.MakeThinText(
        text =
          sprintf "We sent a confirmation link to your email %s.\n" model
          + "Use it to confirm your ID.\nIt will be valid for 12 hours.",

        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeButton(
        text = "send again",
        command = bindPress dispatch ClickResendEmail,
        margin = Thicknesses.mediumLowerSpace
      )

      View.Grid(
        coldefs = [Star; Star],
        rowSpacing = 0.,
        columnSpacing = 0.,
        width = screenWidthP 0.8,
        margin = Thicknesses.mediumLowerSpace,
        horizontalOptions = LayoutOptions.CenterAndExpand,
        children = [
          View.MakeTextButton(
            text = "log in",
            command = bindPress dispatch ClickGoToSignIn,
            margin = Thickness (0.,-8., 0., 0.),
            fontFamily = Fonts.renogare,
            horizontalOptions = LayoutOptions.Start
          )
            .Column(0)

          View.MakeTextButton(
            text = "change email",
            command = bindPress dispatch ClickGoToChangeEmail,
            horizontalOptions = LayoutOptions.End
          )
            .Column(1)
        ]
      )
    ]
  )
