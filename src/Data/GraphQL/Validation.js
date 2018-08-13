var G = require("graphql");

exports._validate = function(schema, doc) {
  return G.validate(schema, doc);
};
