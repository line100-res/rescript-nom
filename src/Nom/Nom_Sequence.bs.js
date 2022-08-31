// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Nom_Core = require("./Nom_Core.bs.js");

function preceded(p1, p2, err) {
  var p = function (param, param$1) {
    return Nom_Core.tuple(p1, p2, param, param$1);
  };
  return function (param, param$1) {
    return Nom_Core.contextMapError((function (param) {
                  return param[1];
                }), p, err, param, param$1);
  };
}

function terminated(p1, p2, err) {
  var p = function (param, param$1) {
    return Nom_Core.tuple(p1, p2, param, param$1);
  };
  return function (param, param$1) {
    return Nom_Core.contextMapError((function (param) {
                  return param[0];
                }), p, err, param, param$1);
  };
}

function separated(p1, p2, p3, err) {
  var p = Nom_Core.tuple3(p1, p2, p3);
  return function (param, param$1) {
    return Nom_Core.contextMapError((function (param) {
                  return [
                          param[0],
                          param[2]
                        ];
                }), p, err, param, param$1);
  };
}

function delimited(p1, p2, p3, err) {
  var p = Nom_Core.tuple3(p1, p2, p3);
  return function (param, param$1) {
    return Nom_Core.contextMapError((function (param) {
                  return param[1];
                }), p, err, param, param$1);
  };
}

exports.preceded = preceded;
exports.terminated = terminated;
exports.separated = separated;
exports.delimited = delimited;
/* No side effect */
