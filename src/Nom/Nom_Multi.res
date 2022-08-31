open Nom_Core
open Nom_Sequence

// all methods with error handler are Not Context Safe

// many0, many1, manyMN

// always return Ok

// many0, many0By

let many0 = (p: parser<'a, 'b, 'c>) => {
  (input, i) => {
    let arr = []
    let rec loop = (current, i0) => {
      switch p(current, i0) {
      | Pass(rest, v, i1) => {
          let _ = Js.Array2.push(arr, v)
          loop(rest, i1)
        }
      | _ => Pass(current, arr, i0)
      }
    }
    loop(input, i)
  }
}

// 根据位置创建不同的parser
let many0By = (f: int => parser<'a, 'b, 'c>) => {
  (input, i) => {
    let arr = []
    let rec loop = (current, i0, count) => {
      let p = f(0)
      switch p(current, i0) {
      | Pass(rest, v, i1) => {
          let _ = Js.Array2.push(arr, v)
          loop(rest, i1, count + 1)
        }
      | _ => Pass(current, arr, i0)
      }
    }
    loop(input, i, 0)
  }
}

// many1

let many1 = (p: parser<'a, 'b, 'c>, err) => {
  let p_many = many0(p)
  (input, i) => {
    switch p_many(input, i) {
    | Pass(rest, v, i0) =>
      if Js.Array.length(v) > 0 {
        Pass(rest, v, i0)
      } else {
        Fail(rest, err(), i0)
      }
    | _ => raise(Err("unreachable"))
    }
  }
}

// let many1By = (f, err) => {
//   let p_many = many0By(f)
//   (input, i) => {
//     switch p_many(input, i) {
//     | Pass(rest, v, i0) =>
//       if Js.Array.length(v) > 0 {
//         Pass(rest, v, i0)
//       } else {
//         Fail(rest, err(), i0)
//       }
//     | _ => raise(Err("unreachable"))
//     }
//   }
// }

// manyMN

// [m, n)
let manyMN = (p: parser<'a, 'b, 'c>, m, n, err) => {
  let p_many = many0(p)
  (input, i) => {
    switch p_many(input, i) {
    | Pass(rest, v, i0) => {
        let len = Js.Array.length(v)
        if len >= m && len < n {
          Pass(rest, v, i0)
        } else {
          Fail(rest, err(len), i0)
        }
      }
    | _ => raise(Err("unreachable"))
    }
  }
}

// let manyMNBy = (f, m, n, err) => {
//   let p_many = many0By(f)
//   (input, i) => {
//     switch p_many(input, i) {
//     | Pass(rest, v, i0) => {
//         let len = Js.Array.length(v)
//         if len >= m && len < n {
//           Pass(rest, v, i0)
//         } else {
//           Fail(rest, err(len), i0)
//         }
//       }
//     | _ => raise(Err("unreachable"))
//     }
//   }
// }

// sep, sep2

let sep = (p: parser<'a, 'b, 'c>, s: parser<'a, 'b, 'c>) => {
  let p_next = many0(preceded(s, p, a => a))
  bind(p, first => {
    (input, i) => {
      switch p_next(input, i) {
      | Pass(rest, v, i0) => {
          let _ = Js.Array.unshift(first, v)
          Pass(rest, v, i0)
        }
      | _ => raise(Err("unreachable"))
      }
    }
  })
}

let sep2 = (p: parser<'a, 'b, 'c>, s: parser<'a, 'b, 'c>, err) => {
  let p_next = many0(preceded(s, p, a => a))
  bind(p, first => {
    (input, i) => {
      switch p_next(input, i) {
      | Pass(rest, v, i0) =>
        if Js.Array.length(v) > 0 {
          let _ = Js.Array.unshift(first, v)
          Pass(rest, v, i0)
        } else {
          Fail(rest, err(v), i)
        }
      | _ => raise(Err("unreachable"))
      }
    }
  })
}

// reduce

let reduceBy = (p: parser<'a, 'b, 'c>, f, acc) => {
  (input, i) => {
    let rec loop = (current, acc, i0) => {
      switch p(current, i0) {
      | Pass(rest, v, i1) =>
        switch f(acc, v) {
        | None => Pass(current, acc, i0)
        | Some(merged) => loop(rest, merged, i1)
        }
      | _ => Pass(current, acc, i0)
      }
    }
    loop(input, acc, i)
  }
}
