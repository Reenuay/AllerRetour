module AllerRetour.ResendEmailPage

open Fabulous
open Fabulous.XamarinForms
open PrimitiveTypes
open Resources
open Views
open Xamarin.Forms

type Model =
  private
    {
      Email: EmailAddress
    }

type Msg =
  private
  | ResendEmail
  | ChangeEmail
  | SignIn
  | EmailResent of Http.Result<string>

let initModel email =
  {
    Email = email
  }

let update tokenMaybe msg (model: Model) =
  match msg with
  | ResendEmail ->
    let
      cmd =
        match tokenMaybe with
        | Some token ->
          Cmd.ofAsyncMsg <|
            async {
              let! res = Http.resendConfirmEmail token
              return EmailResent res
            }

        | None ->
          Cmd.batch
            [
              Route.push Route.SignIn

              Message.show "You are not signed in."
            ]
    in
    ( model, cmd )

  | ChangeEmail ->
    let
      cmd =
        model.Email
        |> Route.ChangeEmail 
        |> Route.push
    in
    ( model, cmd )

  | SignIn ->
    let
      cmd =
        Cmd.batch
          [
            Security.dropToken

            Route.push Route.SignIn
          ]
    in
    ( model, cmd )

  | EmailResent (Ok _) ->
    ( model, Message.show "Email were sent successfully!" )

  | EmailResent (Error errors) ->
    ( model, Message.errors errors )

let view model dispatch =
  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    children = [
      View.MakeAvatar(
        source = Images.verificationCode,
        margin = Thicknesses.bigLowerSpace
      )

      View.MakeText(
        text = "Please confirm your registered email ID"
      )

      let
        email =
          EmailAddress.value model.Email
      in
      View.MakeThinText(
        text =
          sprintf "We sent a confirmation link to your email %s.\n" email
          + "Use it to confirm your ID.\nIt will be valid for 12 hours.",

        margin = Thicknesses.mediumLowerSpace
      )

      View.MakeButton(
        text = "send again",
        margin = Thicknesses.mediumLowerSpace,
        command = bindClick dispatch ResendEmail
      )

      View.Grid(
        width = screenWidthP 0.8,
        margin = Thicknesses.mediumLowerSpace,
        coldefs = [ Star; Star ],
        rowSpacing = 0.,
        columnSpacing = 0.,
        horizontalOptions = LayoutOptions.CenterAndExpand,
        children = [
          View.MakeTextButton(
            text = "change email",
            margin = Thickness (0., -8., 0., 0.),
            command = bindClick dispatch ChangeEmail,
            horizontalOptions = LayoutOptions.Start
          )
            .Column(0)

          View.MakeTextButton(
            text = "log in",
            command = bindClick dispatch SignIn,
            fontFamily = Fonts.renogare,
            horizontalOptions = LayoutOptions.End
          )
            .Column(1)
        ]
      )
    ]
  )
