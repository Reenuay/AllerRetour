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
  makeScrollStackPage [
    Images.verificationCode
    |> makeCircle
    |> margin Thicknesses.mediumUpperBigLowerSpace

    makeInfoText "Please confirm your registered email ID"

    makeThinText (
      sprintf "We sent a confirmation link to your email %s.\n" model
      + "Use it to confirm your ID.\nIt will be valid for 12 hours."
    )
    |> margin Thicknesses.mediumLowerSpace

    makeButton
      true
      (bindPress dispatch ClickResendEmail)
      "send again"
    |> margin Thicknesses.mediumLowerSpace

    makeDuoGrid
      (makeLink (bindPress dispatch ClickGoToChangeEmail) "change email"
      |> margin (Thicknesses.duoGridCentering))
      (makeNavButton (bindPress dispatch ClickGoToSignIn) "log in")
    |> margin Thicknesses.mediumLowerSpace
  ]

