// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Nom_Core = require("./Nom_Core.bs.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Nom_Sequence = require("./Nom_Sequence.bs.js");

function many0(p, input, i) {
  var arr = [];
  var _current = input;
  var _i0 = i;
  while(true) {
    var i0 = _i0;
    var current = _current;
    var match = Curry._2(p, current, i0);
    if (match.TAG !== /* Pass */0) {
      return {
              TAG: /* Pass */0,
              _0: current,
              _1: arr,
              _2: i0
            };
    }
    arr.push(match._1);
    _i0 = match._2;
    _current = match._0;
    continue ;
  };
}

function many0By(f, input, i) {
  var arr = [];
  var _current = input;
  var _i0 = i;
  var _count = 0;
  while(true) {
    var count = _count;
    var i0 = _i0;
    var current = _current;
    var p = Curry._1(f, 0);
    var match = Curry._2(p, current, i0);
    if (match.TAG !== /* Pass */0) {
      return {
              TAG: /* Pass */0,
              _0: current,
              _1: arr,
              _2: i0
            };
    }
    arr.push(match._1);
    _count = count + 1 | 0;
    _i0 = match._2;
    _current = match._0;
    continue ;
  };
}

function many1(p, err) {
  return function (input, i) {
    var match = many0(p, input, i);
    if (match.TAG === /* Pass */0) {
      var i0 = match._2;
      var v = match._1;
      var rest = match._0;
      if (v.length > 0) {
        return {
                TAG: /* Pass */0,
                _0: rest,
                _1: v,
                _2: i0
              };
      } else {
        return {
                TAG: /* Fail */1,
                _0: rest,
                _1: Curry._1(err, undefined),
                _2: i0
              };
      }
    }
    throw {
          RE_EXN_ID: Nom_Core.Err,
          _1: "unreachable",
          Error: new Error()
        };
  };
}

function manyMN(p, m, n, err) {
  return function (input, i) {
    var match = many0(p, input, i);
    if (match.TAG === /* Pass */0) {
      var i0 = match._2;
      var v = match._1;
      var rest = match._0;
      var len = v.length;
      if (len >= m && len < n) {
        return {
                TAG: /* Pass */0,
                _0: rest,
                _1: v,
                _2: i0
              };
      } else {
        return {
                TAG: /* Fail */1,
                _0: rest,
                _1: Curry._1(err, len),
                _2: i0
              };
      }
    }
    throw {
          RE_EXN_ID: Nom_Core.Err,
          _1: "unreachable",
          Error: new Error()
        };
  };
}

function sep(p, s) {
  var partial_arg = Nom_Sequence.preceded(s, p, (function (a) {
          return a;
        }));
  var p_next = function (param, param$1) {
    return many0(partial_arg, param, param$1);
  };
  return function (param, param$1) {
    return Nom_Core.bind(p, (function (first, input, i) {
                  var match = Curry._2(p_next, input, i);
                  if (match.TAG === /* Pass */0) {
                    var v = match._1;
                    v.unshift(first);
                    return {
                            TAG: /* Pass */0,
                            _0: match._0,
                            _1: v,
                            _2: match._2
                          };
                  }
                  throw {
                        RE_EXN_ID: Nom_Core.Err,
                        _1: "unreachable",
                        Error: new Error()
                      };
                }), param, param$1);
  };
}

function sep2(p, s, err) {
  var partial_arg = Nom_Sequence.preceded(s, p, (function (a) {
          return a;
        }));
  var p_next = function (param, param$1) {
    return many0(partial_arg, param, param$1);
  };
  return function (param, param$1) {
    return Nom_Core.bind(p, (function (first, input, i) {
                  var match = Curry._2(p_next, input, i);
                  if (match.TAG === /* Pass */0) {
                    var v = match._1;
                    var rest = match._0;
                    if (v.length > 0) {
                      v.unshift(first);
                      return {
                              TAG: /* Pass */0,
                              _0: rest,
                              _1: v,
                              _2: match._2
                            };
                    } else {
                      return {
                              TAG: /* Fail */1,
                              _0: rest,
                              _1: Curry._1(err, v),
                              _2: i
                            };
                    }
                  }
                  throw {
                        RE_EXN_ID: Nom_Core.Err,
                        _1: "unreachable",
                        Error: new Error()
                      };
                }), param, param$1);
  };
}

function reduceBy(p, f, acc, input, i) {
  var _current = input;
  var _acc = acc;
  var _i0 = i;
  while(true) {
    var i0 = _i0;
    var acc$1 = _acc;
    var current = _current;
    var match = Curry._2(p, current, i0);
    if (match.TAG !== /* Pass */0) {
      return {
              TAG: /* Pass */0,
              _0: current,
              _1: acc$1,
              _2: i0
            };
    }
    var merged = Curry._2(f, acc$1, match._1);
    if (merged === undefined) {
      return {
              TAG: /* Pass */0,
              _0: current,
              _1: acc$1,
              _2: i0
            };
    }
    _i0 = match._2;
    _acc = Caml_option.valFromOption(merged);
    _current = match._0;
    continue ;
  };
}

exports.many0 = many0;
exports.many0By = many0By;
exports.many1 = many1;
exports.manyMN = manyMN;
exports.sep = sep;
exports.sep2 = sep2;
exports.reduceBy = reduceBy;
/* No side effect */
