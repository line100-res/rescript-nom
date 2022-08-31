type parseResult<'a, 'b, 'c> =
  | Pass(Seq.t<'a>, 'b, int)
  | Fail(Seq.t<'a>, 'c, int)

type parser<'a, 'b, 'c> = (Seq.t<'a>, int) => parseResult<'a, 'b, 'c>

exception Err(string)
exception TransformException(int)

let identity = endErr => (input, i) => {
	switch input {
	| Seq.Nil => Fail(input, endErr, i)
	| Seq.Cons(c) => Pass(Seq.cdr(c), c.head, i + 1)
	}
}

let satisfy = (test, err) => {
  (input, i) => {
    switch input {
    | Seq.Nil => Fail(input, err(None), i)
    | Seq.Cons(c) =>
      if test(c.head) {
        Pass(Seq.cdr(c), c.head, i + 1)
      } else {
        Fail(input, err(Some(c.head)), i)
      }
    }
  }
}

let satisfyN = (test, n, err) => {
  (input, i) => {
    let lst = Seq.take(input, n)
    switch lst {
    | Seq.Nil => Fail(input, err(None), i)
    | seq =>
      if test(seq) {
        Pass(Seq.drop(input, n), seq, i + n)
      } else {
        Fail(input, err(Some(lst)), i)
      }
    }
  }
}

let isEnd = (v, err) => {
  (input, i) => {
    switch input {
    | Seq.Nil => Pass(input, v, i)
    | _ => Fail(input, err(), i)
    }
  }
}

let return = v => {
  (input, i) => {
    Pass(input, v, i)
  }
}

let error = e => {
  (input, i) => {
    Fail(input, e, i)
  }
}

let mapValue = (p: parser<'a, 'b, 'c>, f) => {
  (input, i) => {
    switch p(input, i) {
    | Pass(rest, v, i) => Pass(rest, f(v), i)
    | Fail(rest, e, i) => Fail(rest, e, i)
    }
  }
}

let mapError = (p: parser<'a, 'b, 'c>, f) => {
  (input, i) => {
    switch p(input, i) {
    | Fail(rest, e, i) => Fail(rest, f(e), i)
    | Pass(rest, v, i) => Pass(rest, v, i)
    }
  }
}

let map = (p: parser<'a, 'b, 'c>, fv, fe) => {
  (input, i) => {
    switch p(input, i) {
    | Pass(rest, v, i) => Pass(rest, fv(v), i)
    | Fail(rest, e, i) => Fail(rest, fe(e), i)
    }
  }
}

let mapPassed = (p: parser<'a, 'b, 'c>, f) => {
  (input, i) => {
    switch p(input, i) {
    | Pass(rest, v, i) => f(rest, v, i)
    | Fail(rest, e, i) => Fail(rest, e, i)
    }
  }
}

let mapFailed = (p: parser<'a, 'b, 'c>, f) => {
  (input, i) => {
    switch p(input, i) {
    | Fail(rest, e, i) => f(rest, e, i)
    | Pass(rest, v, i) => Pass(rest, v, i)
    }
  }
}

// bind

let bind = (p: parser<'a, 'b, 'c>, f: 'b => parser<'a, 'd, 'c>) => {
  (input, i) => {
    switch p(input, i) {
    | Pass(input, v, i) => f(v)(input, i)
    | Fail(input, e, i) => Fail(input, e, i)
    }
  }
}

// or, alt

let or = (p1: parser<'a, 'b, 'c>, p2: parser<'a, 'b, 'c>) => {
  (input, i) => {
    switch p1(input, i) {
    | Fail(_, _, _) => p2(input, i)
    | pass => pass
    }
  }
}

let alt = (lst: list<parser<'a, 'b, 'c>>, err) => {
  let p = List.fold_right(or, lst, error(err()))
  (input, i) => {
    switch input {
    | Seq.Nil => Fail(input, err(), i)
    | _ => p(input, i)
    }
  }
}

// cons, seq

let cons = (p1: parser<'a, 'b, 'c>, p2: parser<'a, Seq.t<'b>, 'c>) => {
  (input, i) => {
    switch p1(input, i) {
    | Pass(input, v1, i) =>
      switch p2(input, i) {
      | Pass(input, v2, i) => Pass(input, Seq.cons(v1, Seq.Forced(v2)), i)
      | Fail(input, e, i) => Fail(input, e, i)
      }
    | Fail(input, e, i) => Fail(input, e, i)
    }
  }
}

let seq = (lst: list<parser<'a, 'b, 'c>>) => {
  List.fold_right(cons, lst, return(Seq.Nil))
}

// listPrepend, list

let listPrepend = (p1: parser<'a, 'b, 'c>, p2: parser<'a, list<'b>, 'c>) => {
  (input, i) => {
    switch p1(input, i) {
    | Pass(input, v1, i) =>
      switch p2(input, i) {
      | Pass(input, v2, i) => Pass(input, list{v1, ...v2}, i)
      | Fail(input, e, i) => Fail(input, e, i)
      }
    | Fail(input, e, i) => Fail(input, e, i)
    }
  }
}

let list = (lst: list<parser<'a, 'b, 'c>>) => {
  List.fold_right(listPrepend, lst, return(list{}))
}

// tuple, tuple3

let tuple = (p1: parser<'a, 'b, 'c>, p2: parser<'a, 'b, 'c>) => {
  (input, i) => {
    switch p1(input, i) {
    | Pass(input, v1, i) =>
      switch p2(input, i) {
      | Pass(input, v2, i) => Pass(input, (v1, v2), i)
      | Fail(input, e, i) => Fail(input, e, i)
      }
    | Fail(input, e, i) => Fail(input, e, i)
    }
  }
}

// * safe context
let tuple3 = (p1: parser<'a, 'b, 'c>, p2: parser<'a, 'b, 'c>, p3: parser<'a, 'b, 'c>) => {
  bind(p1, v1 => {
    bind(p2, v2 => {
      bind(p3, v3 => return((v1, v2, v3)))
    })
  })
}

// takeWhile, takeWhile1, reduceWhile

let takeWhile = test => {
  (input, i) => {
    let (slice, rest) = Slice.seqSplitWhile(input, test)
    let (len, _) = slice
    Pass(rest, slice, i + len)
  }
}

let takeWhile1 = (test, err) => {
  let p = takeWhile(test)
  (input, i) => {
    switch p(input, i) {
    | Pass(rest, v, i0) =>
      if i0 > i {
        Pass(rest, v, i)
      } else {
        Fail(input, err(), i)
      }
    | _ => raise(Err("unreachable"))
    }
  }
}

let reduceWhile = (test, f, init) => {
  (input, i) => {
    let (acc, n, rest) = Seq.reduceWhile(input, test, f, init)
    Pass(rest, acc, i + n)
  }
}

// context, contextTuple, contextMap

let context = (make, p: parser<'a, 'b, 'c>) => {
  (input, i) => {
    switch p(input, i) {
    | Pass(rest, v, i) => Pass(rest, make(v), i)
    | Fail(input, err, _) => Fail(input, err, i)
    }
  }
}

let contextMapError = (make, p: parser<'a, 'b, 'c>, err) => {
  (input, i) => {
    switch p(input, i) {
    | Pass(rest, v, i0) => Pass(rest, make(v), i0)
    | Fail(rest, e, i0) => Fail(input, err((rest, e, i0)), i)
    }
  }
}

let contextTuple = (make, p: parser<'a, 'b, 'c>) => {
  context(((a, b)) => make(a, b), p)
}

let contextTuple3 = (make, p: parser<'a, 'b, 'c>) => {
  context(((a, b, c)) => make(a, b, c), p)
}

// all consuming

// * err(None), means not all consumed
// * err(Some(rest, e, i)), means parse error
let allConsuming = (p: parser<'a, 'b, 'c>, err) => {
  (input, i) => {
    switch p(input, i) {
    | Pass(rest, v, i0) =>
      if rest == Seq.Nil {
        Pass(rest, v, i0)
      } else {
        Fail(input, err(None), i)
      }
    | Fail(rest, e, i0) => Fail(input, err(Some(rest, e, i0)), i)
    }
  }
}

// seq 2 seq transform
let rec transform = (p: parser<'a, 'b, 'c>, seq, i) => {
  switch p(seq, i) {
  | Pass(rest, v, i0) => Seq.cons(v, Seq.Lazy(() => transform(p, rest, i0)))
  | _ => raise(TransformException(i))
  }
}

// gc友好的transform，针对特别长的seq
let transfromInPlace = (p: parser<'a, 'b, 'c>, s: ref<Seq.t<'a>>) => {
  let rec next = () => {
    if s.contents == Seq.Nil {
      Seq.Nil
    } else {
      switch p(s.contents, 0) {
      | Pass(rest, v, _) => {
          s := rest
          Seq.cons(v, Seq.Lazy(() => next()))
        }
      | _ => raise(TransformException(0))
      }
    }
  }
  next()
}
