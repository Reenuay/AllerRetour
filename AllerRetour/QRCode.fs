module AllerRetour.QRCode

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open ZXing.Net.Mobile.Forms

let create value =
  let qr =
    ZXingBarcodeImageView(
      HorizontalOptions = LayoutOptions.FillAndExpand,
      VerticalOptions = LayoutOptions.FillAndExpand,
      WidthRequest = 100.,
      HeightRequest = 100.
    )

  qr.BarcodeFormat <- ZXing.BarcodeFormat.QR_CODE;
  qr.BarcodeOptions.Width <- 300
  qr.BarcodeOptions.Height <- 300
  qr.BarcodeValue <- value

  View.External(qr)
  
