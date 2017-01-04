"use strict";

exports.normalizeExp = function (expiresIn) {
  return new Date((new Date().getTime()) + expiresIn * 1000).getTime();
};
