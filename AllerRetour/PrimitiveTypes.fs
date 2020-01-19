namespace AllerRetour

open System

type Gender = Male | Female

type Profile = {
  FirstName: string
  LastName: string
  Birthday: DateTime option
  Gender: Gender option
}
  with
    static member Empty = {
      FirstName = ""
      LastName = ""
      Birthday = None
      Gender = None
    }

type EmailAndPassword = {
  Email: string
  Password: string
}

type SignUpRequest = {
  FirstName: string
  LastName: string
  Email: string
  Password: string
}

type ChangePasswordRequest = {
  NewPassword: string
  OldPassword: string
}
