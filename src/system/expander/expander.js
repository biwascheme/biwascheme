class Expander {
  constructor(transformer, environment) {
    this.transformer = transformer;
    this.environment = environment;
  }

  to_write() {
    return "#<Expander>"
  }
}

const isExpander = obj => obj instanceof Expander;

export { Expander, isExpander };
