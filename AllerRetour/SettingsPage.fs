module AllerRetour.SettingsPage

open Xamarin.Forms
open Fabulous.XamarinForms
open Views

type Msg =
  | ChangeTheme of AppTheme
  | ClickGoBack

let view dispatch =
  let model = GlobalSettings.Settings.Value

  View.MakeScrollStack(
    isDarkTheme = GlobalSettings.IsDarkTheme,
    verticalOptions = LayoutOptions.StartAndExpand,
    children = [
      makeBackButton (bindPress dispatch ClickGoBack)

      View.Switch(
        isToggled = AppTheme.isDark model.Theme,
        toggled = (fun args -> dispatch (ChangeTheme (AppTheme.trueToDark args.Value)))
      )
    ]
  )
