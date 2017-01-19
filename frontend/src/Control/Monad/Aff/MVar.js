"use strict";

// Content
function Empty () {}

function Full (v) {
  this.val = v;
}

function Killed(err) {
  this.err = err;
}

function Consumers(blocked) {
  this.blocked = blocked;
}

function Producers(blocked) {
  this.blocked = blocked;
}

exports._makeMVar = function (nonCanceler) {
  return function (success) {
    return success({ state: new Empty() });
    return nonCanceler;
  };
}

exports._takeMVar = function (nonCanceler, mvar) {
  return function (success, error) {
    if (mvar.state instanceof Killed) {
      error(mvar.state.err);
    } else if (mvar.state instanceof Full) {
      var val = mvar.state.val;
      mvar.state = new Empty();
      success(val);
    } else if (mvar.state instanceof Empty) {
      mvar.state = new Consumers([{success: success, error: error }]);
    } else if (mvar.state instanceof Consumers) {
      mvar.state.blocked.push({ success: success, error: error });
    } else if (mvar.state instanceof Producers) {
      mvar.state.blocked.shift()(success, error);
      if (mvar.state.blocked.length === 0) {
        mvar.state = new Empty();
      }
    }
    return nonCanceler;
  };
}

exports._putMVar = function (nonCanceler, mvar, a) {
  return function (success, error) {
    if (mvar.state instanceof Killed) {
      error(mvar.state.err);
    } else if (mvar.state instanceof Empty) {
      mvar.state = new Full(a);
      success();
    } else if (mvar.state instanceof Full) {
      var val = mvar.state.val;
      var ns = new Producers([
        function (s) {
          s(val);
          return nonCanceler;
        },
        function (s) {
          s(a);
          // so it block until consumed
          success();
          return nonCanceler;
        }
      ]);
      mvar.state = ns;
    } else if (mvar.state instanceof Consumers) {
      var consumers = mvar.state.blocked.shift();
      consumers.success(a);
      if (mvar.state.blocked.length === 0) {
        mvar.state = new Empty();
      }
      success();
    } else if (mvar.state instanceof Producers) {
      mvar.state.blocked.push(function (s) {
        s(a);
        success();
      });
    }
    return nonCanceler;
  };
}

exports._killMVar = function (nonCanceler, mvar, e) {
  return function (success, error) {
    if (mvar.state instanceof Killed) {
      error(mvar.state.err);
    } else if (mvar.state instanceof Consumers) {
      var consumers = mvar.state.blocked;
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
