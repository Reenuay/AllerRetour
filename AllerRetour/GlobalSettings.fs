namespace AllerRetour

type AppTheme = Light | Dark

module AppTheme =
  let isDark = function Light -> false | Dark -> true
  let trueToDark = function false -> Light | true -> Dark

type Settings = {
  Theme: AppTheme
}
  with static member Default = { Theme = Light }

[<AbstractClass; Sealed>]
type GlobalSettings private ()  =
  static member val Settings = ref Settings.Default with get, set
  static member IsDarkTheme = AppTheme.isDark GlobalSettings.Settings.Value.Theme
