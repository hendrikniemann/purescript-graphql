var G = require("graphql");

exports._graphql = function(schema, query, rootValue, variables, operation) {
  return function(onError, onSuccess) {
    G.graphql(schema, query, rootValue, null, variables, operation)
      .then(onSuccess)
      .catch(onError);

    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerError(new Error("Cannot cancel GraphQL execution..."));
    };
  };
};
