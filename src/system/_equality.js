import { to_write } from "./_writer.js"

//
// equality
//
const eq = function(a, b){
  return a === b;
};

// TODO: Records (etc.)
const eqv = function(a, b){
  return a == b && (typeof(a) == typeof(b));
};

const equal = function(a, b){
  return to_write(a) == to_write(b);
};

export { eq, eqv, equal };
