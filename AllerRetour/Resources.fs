module AllerRetour.Resources

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

module Colors =
  let backgroundDark = Color.FromHex("#141E32")
  let accent = Color.FromHex("#E52535")

module Images =
  let backgroundDark = Path "background_dark.png"
  let logo = Path "logo.png"
  let envelopeIcon = Path "envelope.png"
  let lockIcon = Path "lock.png"
  let eyeIcon = Path "eye.png"
  let eyeCrossedIcon = Path "eyeCrossed.png"

module Fonts =
  let renogare = "Renogare.ttf#Renogare-Regular"
  let segoeUiLight = "SegoeUILight.ttf#Segoe UI"

module FontSizes =
  let big = FontSize 32.
  let light = FontSize 16.
  let thin = FontSize 11.

module Thicknesses =
  let zero = Thickness 0.
  let bigUpperSpace = Thickness (0., 50., 0., 0.)
  let bigLowerSpace = Thickness (0., 0., 0., 50.)
  let mediumLowerSpace = Thickness (0., 0., 0., 35.)
  let entryError = Thickness (4., -15., 4., 0.)
  let rightLittleSpace = Thickness (0., 0., 5., 0.)

module Opacities =
  let light = 0.8
