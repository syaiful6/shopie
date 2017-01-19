"use strict";

exports._makeMVar = function (nonCanceler) {
  return function (success) {
    success({
      consumers: [],
      producers: [],
      error: undefined
    });
    return nonCanceler;
  };
};

exports._takeMVar = function (nonCanceler, avar) {
  return function (success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.producers.length > 0) {
      avar.producers.shift()(success, error);
    } else {
      avar.consumers.push({ peek: false, success: success, error: error });
    }

    return nonCanceler;
  };
};

exports._tryTakeMVar = function (nonCanceler, avar, nothing, just) {
  return function (success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.producers.length > 0) {
      avar.producers.shift()(function (x) {
        success(just(x));
        return nonCanceler
      }, error);
    } else {
      success(nothing);
    }
    return nonCanceler;
  };
}

exports._peekMVar = function (nonCanceler, avar) {
  return function (success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.producers.length > 0) {
      avar.producers[0](success, error);
    } else {
      avar.consumers.push({ peek: true, success: success, error: error });
    }
    return nonCanceler;
  };
};

exports._putMVar = function (nonCanceler, avar, a) {
  return function (success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.producers.length > 0) {
      avar.producers.push(function (s) {
        s(a);
        success();
        return nonCanceler;
      });
    } else {
      performPut(a, avar, success, nonCanceler);
    }
    return nonCanceler;
  };
};

exports._tryPutMVar = function (nonCanceler, avar, a) {
  return function (success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.producers.length > 0) {
      success(false);
    } else {
      performPut(a, avar, success, nonCanceler, true);
    }
    return nonCanceler;
  }
}

exports._killMVar = function (nonCanceler, avar, e) {
  return function (success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else {
      avar.error = e;
      while (avar.consumers.length) {
        avar.consumers.shift().error(e);
      }
      success();
    }

    return nonCanceler;
  };
};

function performPut(a, avar, success, nonCanceler, val) {
  var shouldQueue = true;
  var consumers = [];
  var consumer;

  while (true) {
    consumer = avar.consumers.shift();
    if (consumer) {
      consumers.push(consumer);
      if (consumer.peek) {
        continue;
      } else {
        shouldQueue = false;
      }
    }
    break;
  }

  if (shouldQueue) {
    avar.producers.push(function (s) {
      s(a);
      return nonCanceler;
    });
  }

  for (var i = 0; i < consumers.length; i++) {
    consumers[i].success(a);
  }

  success(val);
}
