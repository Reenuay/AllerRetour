module AllerRetour.Resources

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

module Colors =
  let backgroundDark = Color.FromHex("#141E32")
  let backgroundLight = Color.FromHex("#E2DEE3")
  let accent = Color.FromHex("#E52535")

module Images =
  let backgroundDark = Path "background_dark.png"
  let logo = Path "logo.png"
  let envelopeIcon = Path "envelope.png"
  let lockIcon = Path "lock.png"
  let userIcon = Path "user.png"
  let eyeIcon = Path "eye.png"
  let eyeCrossedIcon = Path "eyeCrossed.png"
  let forgotPassword = Path "forgotPassword.png"
  let verificationCode = Path "verificationCode.png"
  let passwordChange = Path "passwordChange.png"
  let success = Path "success.png"
  let profileIcon = Path "profileIcon.png"
  let profileIconActive = Path "profileIconActive.png"
  let tabLight = Path "tabLight.png"
  let tabLightShadow = Path "tabLightShadow.png"
  let list = Path "list.png"
  let backButton = Path "backButton.png"
  let profile = Path "profile.png"

module Fonts =
  let renogare = "Renogare.ttf#Renogare-Regular"
  let segoeUiLight = "SegoeUILight.ttf#Segoe UI"

module FontSizes =
  let big = FontSize 32.
  let medium = FontSize 24.
  let light = FontSize 18.
  let thin = FontSize 14.
  let xtrasmall = FontSize 8.

module Thicknesses =
  let zero = Thickness 0.
  let bigUpperSpace = Thickness (0., 50., 0., 0.)
  let bigLowerSpace = Thickness (0., 0., 0., 50.)
  let mediumLowerSpace = Thickness (0., 0., 0., 35.)
  let mediumUpperBigLowerSpace = Thickness (0., 35., 0., 50.)
  let paddingForEntryError = Thickness (4., -15., 4., 0.)
  let rightLittleSpace = Thickness (0., 0., 5., 0.)
  let paddingForButton = Thickness (40., 0., 40., -4.)
  let duoGridCentering = Thickness (0.,-8., 0., 0.)
  let eight = Thickness 8.

module Opacities =
  let opaque = 1.
  let light = 0.8
  let ten = 0.1
