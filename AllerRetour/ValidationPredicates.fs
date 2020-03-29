module AllerRetour.ValidationPredicates

open System.Net.Mail
open System.Text.RegularExpressions

let isValidEmail s =
  try
    MailAddress(s).Address = s
  with
  | _ -> false

let hasMinLengthOf l s = String.length s >= l

let hasMaxLengthOf l s = String.length s <= l

let hasExactLengthOf l s = String.length s = l

let containsWords words s =
  Regex.IsMatch(
    s,
    "(" + (List.reduce (fun a b -> a + "|" + b) words) + ")",
    RegexOptions.IgnoreCase
  )

let isValidPin s = Regex.IsMatch(s, "\d{6}")
