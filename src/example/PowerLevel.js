"use strict";

function withDefaults(defaults) {
  return function (f) {
    return function(props) {
      return f(Object.assign(defaults, props));
    }
  }
}

exports.withDefaults = withDefaults;
