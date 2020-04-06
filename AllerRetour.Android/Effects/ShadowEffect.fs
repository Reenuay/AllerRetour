namespace AllerRetour.Android

open System
open System.Linq
open Xamarin.Forms
open Xamarin.Forms.Platform.Android

type ShadowEffect() =
  inherit PlatformEffect()

  override this.OnAttached() =
    try
      let effect =
        this
          .Element
          .Effects
          .FirstOrDefault(fun e -> e :? AllerRetour.ShadowEffect)
          :?> AllerRetour.ShadowEffect

      this.Container.SetElevation(float32 effect.Elevation)
      |> ignore
    with
    | ex ->
        Console.WriteLine("Cannot set property on attached control. Error: {0}", ex.Message)

  override this.OnDetached() = ()
