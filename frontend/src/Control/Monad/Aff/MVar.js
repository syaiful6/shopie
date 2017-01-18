"use strict";

// Content
function Blocked (p, c) {
  this._p = p;
  this._c = c;
}

function Empty () {}

function Full (v) {
  this._v = v;
}

function Killed(e) {
  this._e = e;
}

exports._makeMVar = function (nonCanceler) {
  return function (success) {
    return success({ state: new Empty() });
    return nonCanceler;
  }
}

exports._takeMVar = function (nonCanceler, mvar) {
  return function (success, error) {
    if (mvar.state instanceof Killed) {
      error(mvar.state._e);
    } else if (mvar.state instanceof Full) {
      mvar.state = new Empty();
      success(mvar.state._v);
    } else if (mvar.state instanceof Empty) {
      mvar.state = new Blocked([], [{ success: success, error: error }]);
    } else if (mvar.state instanceof Blocked) {
      if (mvar.state._p.length > 0) {
        mvar.state._p.shift()(success, error);
        if (mvar.state._p.length === 0 && mvar.state._c.length === 0) {
          mvar.state = new Empty();
        }
      } else {
        mvar._c.push({ success: success, error: error });
      }
    }
    return nonCanceler;
  }
}

exports._putMVar = function (nonCanceler, mvar, a) {
  return function (success, error) {
    if (mvar.state instanceof Killed) {
      error(mvar.state._e);
    } else if (mvar.state instanceof Empty) {
      mvar.state = new Full(a);
      success({});
    } else if (mvar.state instanceof Full) {
      mvar.state = new Blocked([
        function (s) {
          s(mvar.state._v);
          return nonCanceler;
        },
        function (s) {
          s(a);
          return nonCanceler;
        }
      ], []);
      success({});
    } else if (mvar.state instanceof Blocked) {
      for (var i = 0, len = mvar.state._c.length; i < len; i++) {
        mvar.state._c[i].success(a);
      }
      mvar.state._c = [];
      if (mvar.state._p.length === 0 && mvar.state._c.length === 0) {
        mvar.state = new Empty();
      }
      success({});
    }
    return nonCanceler;
  };
}

exports._killMVar = function (nonCanceler, mvar, e) {
  return function (success, error) {
    if (mvar.state instanceof Killed) {
      error(mvar.state._e);
    } else if (mvar.state instanceof Blocked) {
      var consumers = mvar.state._c;
      while (consumers.length) {
        consumers.shift().error(e);
      }
      mvar.state = new Killed(e);
      success({});
    } else {
      mvar.state = new Killed(e);
      success({});
    }
    return nonCanceler;
  };
};
