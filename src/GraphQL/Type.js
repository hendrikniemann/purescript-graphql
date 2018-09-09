var G = require("graphql");

exports.float = G.GraphQLFloat;

exports.int = G.GraphQLInt;

exports.string = G.GraphQLString;

exports.id = G.GraphQLID;

exports.list = function() {
  return function(type) {
    return new G.GraphQLList(type);
  };
};

exports.nonNull = function() {
  return function(type) {
    return new G.GraphQLNonNull(type);
  };
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

exports._field = function(toNullable, toMaybe) {
  // Mapping to the JS implementation has the major problem that in JS we work
  // with null values but in PS we want to make use of the Maybe type. This
  // function transforms the input object so that it contains Justs and Nothings
  // at the right places by inspecting the argument type.
  function transformArgs(args, types) {
    var result = {};
    Object.keys(types).forEach(function(key) {
      result[key] = transformArg(args[key], types[key].type);
    });
    return result;
  }

  function transformArg(arg, type) {
    if (G.isNonNullType(type)) {
      if (G.isInputObjectType(type.ofType)) {
        return transformArgs(arg, type.ofType.getFieldMap());
      } else if (G.isListType(type.ofType)) {
        return arg.map(function(elem) {
          return transformArg(elem, type.ofType.ofType);
        });
      }
      return arg;
    }
    if (G.isInputObjectType(type)) {
      return toMaybe(transformArgs(arg, type.getFieldMap()) || null);
    } else if (G.isListType(type)) {
      return toMaybe(
        value.map(function(value) {
          return transformArg(value, type.ofType);
        }) || null
      );
    }
    return toMaybe(arg || null);
  }

  return function(type, description, argDef, resolve) {
    const nullable = !G.isNonNullType(type);
    return {
      type: type,
      description: description || undefined,
      args: argDef,
      resolve: function(parent, args, context, resolveInfo) {
        var transformedArgs = transformArgs(args, argDef);
        var result = resolve(parent)(transformedArgs)();
        if (nullable) {
          // Hack to handle returned Maybe values from the original resolver
          return result.then(toNullable);
        }
        return result;
      }
    };
  };
};

exports._argument = function(type, description) {
  return {
    type: type,
    description: description || undefined
  };
};

exports._inputObjectType = function(name, description, fields) {
  return new G.GraphQLInputObjectType({
    name: name,
    description: description || undefined,
    fields: fields
  });
};

exports._inputField = function(type, description) {
  return {
    type: type,
    description: description || undefined
  };
};
