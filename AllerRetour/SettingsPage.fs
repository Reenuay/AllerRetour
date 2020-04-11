module AllerRetour.SettingsPage

open Xamarin.Forms
open Fabulous.XamarinForms
open Views

type Msg =
  | ChangeTheme of AppTheme
  | ClickGoBack

let view dispatch =
  let model = GlobalSettings.Settings

  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    dispatchBack = bindPress dispatch ClickGoBack,
    verticalOptions = LayoutOptions.StartAndExpand,
    children = [
      View.StackLayout(
        orientation = StackOrientation.Horizontal,
        margin = Thickness (0., 40.),
        children = [
          View.Switch(
            isToggled = AppTheme.isDark model.Theme,
            toggled = (fun args -> dispatch (ChangeTheme (AppTheme.trueToDark args.Value)))
          )

          makeInfoText "Dark mode"
        ]
      )
    ]
  )
