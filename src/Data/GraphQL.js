var G = require("graphql");

exports.nativeParse = function(left, right, query) {
  try {
    return right(G.parse(query));
  } catch (error) {
    return left(error);
  }
};

exports.nativeExecute = function(schema, doc) {
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

exports.nativeValidate = function(schema, doc) {
  return G.validate(schema, doc);
};

exports.nativeSchema = function(query, mutation) {
  return new G.GraphQLSchema({
    query: query,
    mutation: mutation || undefined
  });
};

exports.nativeObjectType = function(name, description, fields) {
  return new G.GraphQLObjectType({
    name: name,
    description: description || undefined,
    fields: fields
  });
};

exports.nativeField = function(type, description, args, resolve) {
  return {
    type: type,
    description: description || undefined,
    args: args,
    resolve: function(parent, args, context, resolveInfo) {
      return resolve(parent)(args)();
    }
  };
};

exports.nativeArgument = function(type, description) {
  return {
    type: type,
    description: description || undefined
  };
};

exports.floatScalar = G.GraphQLFloat;
exports.intScalar = G.GraphQLInt;
exports.stringScalar = G.GraphQLString;
exports.idScalar = G.GraphQLID;
