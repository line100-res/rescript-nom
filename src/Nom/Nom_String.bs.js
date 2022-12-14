// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj = require("rescript/lib/js/caml_obj.js");
var Nom_Core = require("./Nom_Core.bs.js");

function eq(str, err) {
  return function (param, param$1) {
    return Nom_Core.satisfy((function (v) {
                  return Caml_obj.caml_equal(v, str);
                }), err, param, param$1);
  };
}

function oneOf(arr, err) {
  return function (param, param$1) {
    return Nom_Core.satisfy((function (v) {
                  return arr.indexOf(v) >= 0;
                }), err, param, param$1);
  };
}

function noneOf(arr, err) {
  return function (param, param$1) {
    return Nom_Core.satisfy((function (v) {
                  return arr.indexOf(v) < 0;
                }), err, param, param$1);
  };
}

exports.eq = eq;
exports.oneOf = oneOf;
exports.noneOf = noneOf;
/* No side effect */
