class SyntacticClosure {
  // freeNamesはArrayか？
  constructor(env, freeNames, form) {
    this.environment = env;
    this.freeNames = freeNames;
    this.form = form;
  }

  to_write() {
    return "#<SyntacticClosure>"
  }
}

const isSyntacticClosure = obj => obj instanceof SyntacticClosure;

export { SyntacticClosure, isSyntacticClosure };
