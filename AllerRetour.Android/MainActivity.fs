namespace AllerRetour.Android

open System

open Android.App
open Android.Content
open Android.Content.PM
open Android.Runtime
open Android.Views
open Android.Widget
open Android.OS
open Xamarin.Forms
open Xamarin.Forms.Platform.Android
open Xamarin.Forms.PlatformConfiguration.AndroidSpecific

[<Activity(
  Label = "AllerRetour.Android",
  Icon = "@mipmap/icon",
  Theme = "@style/MainTheme",
  MainLauncher = true,
  WindowSoftInputMode = SoftInput.AdjustResize,
  ConfigurationChanges = (ConfigChanges.ScreenSize ||| ConfigChanges.Orientation)
)>]
type MainActivity() =
    inherit FormsAppCompatActivity()
    override this.OnCreate (bundle: Bundle) =
        FormsAppCompatActivity.TabLayoutResource <- Resources.Layout.Tabbar
        FormsAppCompatActivity.ToolbarResource <- Resources.Layout.Toolbar
        base.OnCreate (bundle)

        Xamarin.Essentials.Platform.Init(this, bundle)

        Xamarin.Forms.Forms.Init (this, bundle)

        let appcore  = new AllerRetour.App()
        this.LoadApplication (appcore)

        Application
          .Current
          .On<Xamarin.Forms.PlatformConfiguration.Android>()
          .UseWindowSoftInputModeAdjust(WindowSoftInputModeAdjust.Resize);
        |> ignore

    override this.OnRequestPermissionsResult(requestCode: int, permissions: string[], [<GeneratedEnum>] grantResults: Android.Content.PM.Permission[]) =
        Xamarin.Essentials.Platform.OnRequestPermissionsResult(requestCode, permissions, grantResults)

        base.OnRequestPermissionsResult(requestCode, permissions, grantResults)
