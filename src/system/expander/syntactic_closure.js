import { to_write } from "../_writer.js";
import { isSymbol, isVector } from "../_types.js"
import { Cons, isPair } from "../pair.js"

class SyntacticClosure {
  constructor(env, freeNames, form) {
    this.environment = env; // `Environment`
    this.freeNames = freeNames; // TODO: what is this for?
    this.form = form; // A scheme expression
  }

  to_write() {
    return `#<SC ${this.environment.name} ${to_write(this.form)}>`
  }
}

const isSyntacticClosure = obj => obj instanceof SyntacticClosure;

// Returns whether `obj` is an identifier
const isIdentifier = obj => {
  if (isSymbol(obj)) return true;
  if (isSyntacticClosure(obj) && isIdentifier(obj.form)) return true;
  return false;
};

// Strip syntax closures from `obj`
const unwrapSyntax = obj => {
  if (isSyntacticClosure(obj)) {
    return unwrapSyntax(obj.form);
  }
  else if (isPair(obj)) {
    return Cons(unwrapSyntax(obj.car), unwrapSyntax(obj.cdr));
  }
  else if (isVector(obj)) {
    return obj.map(unwrapSyntax);
  }
  else {
    return obj;
  }
}

export { SyntacticClosure, isSyntacticClosure, isIdentifier, unwrapSyntax };
