var G = require("graphql");

exports._execute = function(schema, doc) {
  return function(onError, onSuccess) {
    var maybePromise;
    try {
      maybePromise = G.execute(schema, doc);
      if (typeof maybePromise.then === "function") {
        maybePromise.then(onSuccess).catch(onError);
      } else {
        onSuccess(maybePromise);
      }
    } catch (error) {
      onError(error);
    }

    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerError(new Error("Cannot cancel GraphQL execution..."));
    };
  };
};
