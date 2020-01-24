namespace AllerRetour

open System

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
with
  static member Empty = {
    Email = ""
    Password = ""
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
