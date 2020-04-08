namespace AllerRetour.Android

open System
open Android.App
open Android.Content.PM
open Android.Runtime
open Android.Views
open Android.OS
open Xamarin.Forms.Platform.Android

[<Activity(
  Label = "AllerRetour.Android",
  Icon = "@mipmap/icon",
  Theme = "@style/MainTheme",
  MainLauncher = true,
  ScreenOrientation = ScreenOrientation.Portrait,
  ConfigurationChanges = (ConfigChanges.ScreenSize ||| ConfigChanges.Orientation)
)>]
type MainActivity() =
  inherit FormsAppCompatActivity()

  let mutable softInputAssist : SoftInputAssist =  null
        
  override this.OnCreate (bundle: Bundle) =
    FormsAppCompatActivity.TabLayoutResource <- Resources.Layout.Tabbar
    FormsAppCompatActivity.ToolbarResource <- Resources.Layout.Toolbar

    base.OnCreate (bundle)

    global.ZXing.Net.Mobile.Forms.Android.Platform.Init()

    Xamarin.Essentials.Platform.Init(this, bundle)
    Xamarin.Forms.Forms.Init(this, bundle)

    if Build.VERSION.SdkInt >= BuildVersionCodes.Kitkat && Build.VERSION.SdkInt < BuildVersionCodes.Lollipop then
      this.Window.AddFlags(WindowManagerFlags.TranslucentStatus)

    if Build.VERSION.SdkInt >= BuildVersionCodes.Kitkat then
      this.Window.DecorView.SystemUiVisibility <-
        SystemUiFlags.LayoutStable
        ||| SystemUiFlags.LayoutFullscreen
        |> LanguagePrimitives.EnumToValue
        |> LanguagePrimitives.EnumOfValue
    
    if Build.VERSION.SdkInt >= BuildVersionCodes.Lollipop then
      this.Window.ClearFlags(WindowManagerFlags.TranslucentStatus)
      this.Window.SetStatusBarColor(Android.Graphics.Color.Transparent)

    this.Window.SetSoftInputMode(SoftInput.AdjustResize)

    FFImageLoading.Forms.Platform.CachedImageRenderer.Init(enableFastRenderer = Nullable(true))

    let appcore = new AllerRetour.App()
    this.LoadApplication (appcore)

    softInputAssist <- SoftInputAssist(this)

  override _.OnRequestPermissionsResult(requestCode: int, permissions: string[], [<GeneratedEnum>] grantResults: Android.Content.PM.Permission[]) =
    Xamarin.Essentials.Platform.OnRequestPermissionsResult(requestCode, permissions, grantResults)

    //global.ZXing.Net.Mobile.Android.PermissionsHandler.OnRequestPermissionsResult (requestCode, permissions, grantResults)

    base.OnRequestPermissionsResult(requestCode, permissions, grantResults)

  override _.OnResume() =
    softInputAssist.OnResume()
    base.OnResume()

  override _.OnPause() =
    softInputAssist.OnPause()
    base.OnPause()

  override _.OnDestroy() =
    softInputAssist.OnDestroy()
    base.OnDestroy()
