var G = require("graphql");

exports._parse = function(left, right, query) {
  try {
    return right(G.parse(query));
  } catch (error) {
    return left(error);
  }
};
