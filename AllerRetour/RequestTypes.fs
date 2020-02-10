module AllerRetour.RequestTypes

open System

type SignInRequest = {
  Email: string
  Password: string
}

type SignUpRequest = {
  FirstName: string
  LastName: string
  Email: string
  Password: string
}

type PasswordResetEmailRequest = {
  Email: string
}

type PasswordResetRequest = {
  Email: string
  NewPassword: string
  Token: string
}

type UpdateProfileRequest = {
  FirstName: string
  LastName: string
  Birthday: DateTime option
  Gender: string
}

type ChangeEmailRequest = {
  NewEmail: string
  Password: string
}

type ChangePasswordRequest = {
  NewPassword: string
  OldPassword: string
}
