var G = require("graphql");

exports.float = G.GraphQLFloat;

exports.int = G.GraphQLInt;

exports.string = G.GraphQLString;

exports.id = G.GraphQLID;

exports.list = function(type) {
  return new GraphQLList(type);
};

exports.nonNull = function(type) {
  return new GraphQLNonNull(type);
};

exports._schema = function(query, mutation) {
  return new G.GraphQLSchema({
    query: query,
    mutation: mutation || undefined
  });
};

exports._objectType = function(name, description, fields) {
  return new G.GraphQLObjectType({
    name: name,
    description: description || undefined,
    fields: fields
  });
};

exports._field = function(type, description, args, resolve) {
  return {
    type: type,
    description: description || undefined,
    args: args,
    resolve: function(parent, args, context, resolveInfo) {
      return resolve(parent)(args)();
    }
  };
};

exports._argument = function(type, description) {
  return {
    type: type,
    description: description || undefined
  };
};
