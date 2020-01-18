namespace AllerRetour.Controls

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type OptionalDatePicker() =
  inherit DatePicker()

  let mutable _format = null

  static let optionalDateProperty =
    BindableProperty.Create("OptionalDate", typeof<DateTime option>, typeof<OptionalDatePicker>, None)

  member this.OptionalDate
    with get () =
      this.GetValue(optionalDateProperty) :?> DateTime option
    and set (value: DateTime option) =
      this.SetValue(optionalDateProperty, value)
      this.UpdateDate()

  member this.UpdateDate () =
    match this.OptionalDate with
    | Some d ->
      if _format |> isNull |> not then
        this.Format <- _format
        this.Date <- d
    | None ->
      _format <- this.Format
      this.Format <- "Pick a date..."

  override this.OnBindingContextChanged() =
    base.OnBindingContextChanged()
    this.UpdateDate()

  override this.OnPropertyChanged(propertyName) =
    base.OnPropertyChanged(propertyName);
    if propertyName = "Date" then
      this.OptionalDate <- Some this.Date

[<AutoOpen>]
module OptionalDatePickerExtension =
  let OptionalDatePickerOptionalDateAttributeKey = AttributeKey<_> "OptionalDatePicker_OptionalDate"

  type Fabulous.XamarinForms.View with
    static member inline OptionalDatePicker
      (
        ?optionalDate: DateTime option,
        ?minimumDate,
        ?maximumDate,
        ?format,
        ?textColor,
        ?fontAttributes,
        ?fontFamily,
        ?fontSize,
        ?dateSelected
      ) =
      let attribCount = match optionalDate with None -> 0 | Some _ -> 1
      let attribs =
        ViewBuilders.BuildDatePicker(
          attribCount,
          ?minimumDate = minimumDate,
          ?maximumDate = maximumDate,
          ?format = format,
          ?textColor = textColor,
          ?fontAttributes = fontAttributes,
          ?fontFamily = fontFamily,
          ?fontSize = fontSize,
          ?dateSelected = dateSelected
        )

      match optionalDate with
      | None -> ()
      | Some v -> attribs.Add(OptionalDatePickerOptionalDateAttributeKey, v)

      let update (prevOpt: ViewElement voption) (source: ViewElement) (target: OptionalDatePicker) =
        ViewBuilders.UpdateDatePicker(prevOpt, source, target)
        source.UpdatePrimitive(
          prevOpt,
          target,
          OptionalDatePickerOptionalDateAttributeKey,
          (fun target v -> target.OptionalDate <- v))

      ViewElement.Create(OptionalDatePicker, update, attribs)
