namespace AllerRetour

open Xamarin.Forms
open Fabulous
open Fabulous.XamarinForms

type ShadowEffect() =
  inherit RoutingEffect("AllerRetour.ShadowEffect")

  member val Elevation = 0. with get, set

[<AutoOpen>]
module ShadowEffectViewExtension =
  let ElevationAttribKey = AttributeKey "ShadowEffect_Elevation"
    
  type View with
    static member inline ShadowEffect(?elevation) =
      let attribCount = 0
      let attribCount = match elevation with Some _ -> attribCount + 1 | None -> attribCount
            
      let attribs = AttributesBuilder(attribCount)
                
      match elevation with None -> () | Some v -> attribs.Add(ElevationAttribKey, v)
            
      let create () = ShadowEffect()
            
      let update (prevOpt: ViewElement voption) (source: ViewElement) (target: ShadowEffect) =
          source.UpdatePrimitive(
            prevOpt,
            target,
            ElevationAttribKey,
            (fun target v -> target.Elevation <- v)
          )
                
      ViewElement.Create(create, update, attribs)
