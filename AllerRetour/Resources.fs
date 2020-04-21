module AllerRetour.Resources

open Fabulous.XamarinForms
open Xamarin.Forms

module Colors =
  let backgroundDark = Color.FromHex("#141E32")
  let backgroundLight = Color.FromHex("#E2DEE3")
  let frontDark = Color.FromHex("#1b2e4d")
  let frontLight = Color.FromHex("#F2F0F2")
  let accent = Color.FromHex("#E52535")

module Images =
  let backgroundDark = Path "background_dark.png"
  let backgroundLight = Path "backgroundLight.png"
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
  let mainIcon = Path "mainIcon.png"
  let mainIconActive = Path "mainIconActive.png"
  let mainIconLight = Path "mainIconLight.png"
  let notificationIcon = Path "notificationIcon.png"
  let notificationIconActive = Path "notificationIconActive.png"
  let notificationIconLight = Path "notificationIconLight.png"
  let cardIcon = Path "cardIcon.png"
  let cardIconActive = Path "cardIconActive.png"
  let cardIconLight = Path "cardIconLight.png"
  let searchIcon = Path "searchIcon.png"
  let searchIconActive = Path "searchIconActive.png"
  let searchIconLight = Path "searchIconLight.png"
  let profileIcon = Path "profileIcon.png"
  let profileIconActive = Path "profileIconActive.png"
  let profileIconLight = Path "profileIconLight.png"
  let tabLight = Path "tabLight.png"
  let tabLightShadow = Path "tabLightShadow.png"
  let tabDark = Path "tabDark.png"
  let tabDarkShadow = Path "tabDarkShadow.png"
  let list = Path "list.png"
  let backButton = Path "backButton.png"
  let profile = Path "profile.png"
  let star = Path "star.png"
  let amazon = Path "amazon.png"
  let reebok = Path "reebok.png"
  let aliexpress = Path "aliexpress.png"
  let zara = Path "zara.png"
  let asos = Path "asos.png"

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
  let bigUpperSpace = Thickness (0., 60., 0., 0.)
  let bigLowerSpace = Thickness (0., 0., 0., 50.)
  let mediumLowerSpace = Thickness (0., 0., 0., 35.)
  let mediumUpperBigLowerSpace = Thickness (0., 55., 0., 50.)

module Opacities =
  let opaque = 1.
  let light = 0.8
  let ten = 0.1
