module AllerRetour.SignUpSuccessPage

open Fabulous
open Resources
open PrimitiveTypes
open Views
open Xamarin.Forms

type Model =
  private
    {
      Email: EmailAddress
    }

type Msg =
  private
  | SignIn

let initModel email =
  {
    Email = email
  }

let update msg (model: Model) =
  match msg with
  | SignIn ->
    ( model, Route.push Route.SignIn )

let view (model: Model) dispatch =
  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    children = [
      View.MakeAvatar(
        source = Images.success,
        margin = Thicknesses.bigLowerSpace
      )

      View.MakeText(
        text = "Success!"
      )

      let
        email =
          EmailAddress.value model.Email
      in
      View.MakeThinText(
        text =
          sprintf "We sent a confirmation link to your email %s." email
          + "Use it to confirm your ID.\nIt will be valid for 12 hours.",

        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeTextButton(
        text = "log in",
        margin = Thicknesses.mediumLowerSpace,
        command = bindClick dispatch SignIn,
        fontFamily = Fonts.renogare
      )
    ]
  )
