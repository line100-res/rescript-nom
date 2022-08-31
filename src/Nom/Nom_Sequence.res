open Nom_Core

// all methods in this module are Context Safe

// * err(parseResult)
let preceded = (p1: parser<'a, 'b, 'c>, p2: parser<'a, 'b, 'c>, err) => {
  let p = tuple(p1, p2)
  contextMapError(((_, v)) => v, p, err)
}

// * err(parseResult)
let terminated = (p1: parser<'a, 'b, 'c>, p2: parser<'a, 'b, 'c>, err) => {
  let p = tuple(p1, p2)
  contextMapError(((v, _)) => v, p, err)
}

// * err(parseResult)
let separated = (p1: parser<'a, 'b, 'c>, p2: parser<'a, 'b, 'c>, p3: parser<'a, 'b, 'c>, err) => {
  let p = tuple3(p1, p2, p3)
  contextMapError(((v1, _, v3)) => (v1, v3), p, err)
}

// * err(parseResult)
let delimited = (p1: parser<'a, 'b, 'c>, p2: parser<'a, 'b, 'c>, p3: parser<'a, 'b, 'c>, err) => {
  let p = tuple3(p1, p2, p3)
  contextMapError(((_, v, _)) => v, p, err)
}

// let append = (p1: parser<'a, Seq.t<'b>, 'c>, p2: parser<'a, Seq.t<'b>, 'c>) => {
// 	mapValue(tuple(p1, p2), ((v1, v2)) => {
// 		return(Seq.append(v1, () => v2))
// 	})
// }
