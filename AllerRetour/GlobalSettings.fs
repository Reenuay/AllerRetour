namespace AllerRetour

open Xamarin.Forms

type AppTheme = Light | Dark

module AppTheme =
  let isDark = function Light -> false | Dark -> true
  let trueToDark = function false -> Light | true -> Dark

type Settings = {
  Theme: AppTheme
}
  with static member Default = { Theme = Light }

[<AbstractClass; Sealed>]
type GlobalSettings private () =
  static let _settingsKey = "settings"
  static let _settings = ref Settings.Default

  static member Settings = !_settings

  static member IsDarkTheme = AppTheme.isDark _settings.Value.Theme

  static member Serialize() = Json.serialize !_settings

  static member Change(settings) =
    _settings := settings

  static member Save() =
    Application
      .Current
      .Properties
      .[_settingsKey] <- GlobalSettings.Serialize()

    Application
      .Current
      .SavePropertiesAsync()
    |> Async.AwaitTask

  static member Load() =
    let ok, value =
      Application
        .Current
        .Properties
        .TryGetValue(_settingsKey)

    if ok then
      match Json.deserialize (value :?> string) with
      | Choice1Of2 s ->
        _settings := s
        Ok ()

      | Choice2Of2 s ->
        Error s
    else
      Ok ()
