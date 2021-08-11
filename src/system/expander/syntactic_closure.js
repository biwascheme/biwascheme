class SyntacticClosure {
  constructor(env, freeNames, form) {
    this.environment = env; // `Environment`
    this.freeNames = freeNames; // TODO: what is this for?
    this.form = form; // A scheme expression
  }

  to_write() {
    return "#<SyntacticClosure>"
  }
}

const isSyntacticClosure = obj => obj instanceof SyntacticClosure;

export { SyntacticClosure, isSyntacticClosure };
