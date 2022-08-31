open Nom_Core

let eq = (str, err) => {
  satisfy(v => v == str, err)
}

let oneOf = (arr: array<string>, err) => {
  satisfy(v => Js.Array.indexOf(v, arr) >= 0, err)
}

let noneOf = (arr: array<string>, err) => {
  satisfy(v => Js.Array.indexOf(v, arr) < 0, err)
}
