namespace AllerRetour.Android

open Android.App
open Android.Views

type MyListener(method) =
  inherit Java.Lang.Object()

  interface ViewTreeObserver.IOnGlobalLayoutListener with
    member _.OnGlobalLayout() = method ()

[<AllowNullLiteral>]
type SoftInputAssist (activity: Activity) as this =
  let mutable contentContainer = activity.FindViewById(Android.Resource.Id.Content) :?> ViewGroup
  let mutable rootView = contentContainer.GetChildAt(0)
  let mutable viewTreeObserver : ViewTreeObserver = null
  let mutable listener = new MyListener(this.PossiblyResizeChildOfContent)
  let mutable contentAreaOfWindowBounds = new Android.Graphics.Rect()
  let mutable usableHeightPrevious = 0
  let rootViewLayout = rootView.LayoutParameters

  member _.OnPause() =
    if viewTreeObserver.IsAlive then
      viewTreeObserver.RemoveOnGlobalLayoutListener(listener)

  member _.OnResume() =
    if viewTreeObserver = null || not viewTreeObserver.IsAlive then
      viewTreeObserver <- rootView.ViewTreeObserver

    viewTreeObserver.AddOnGlobalLayoutListener(listener)
    
  member _.OnDestroy() =
    rootView <- null
    contentContainer <- null
    viewTreeObserver <- null

  member _.PossiblyResizeChildOfContent() =
    contentContainer.GetWindowVisibleDisplayFrame(contentAreaOfWindowBounds)
    let usableHeightNow = contentAreaOfWindowBounds.Bottom

    if usableHeightNow <> usableHeightPrevious then
      rootViewLayout.Height <- usableHeightNow
      rootView.Layout(
        contentAreaOfWindowBounds.Left,
        contentAreaOfWindowBounds.Top,
        contentAreaOfWindowBounds.Right,
        contentAreaOfWindowBounds.Bottom
      )

      rootView.RequestLayout();

      usableHeightPrevious <- usableHeightNow
