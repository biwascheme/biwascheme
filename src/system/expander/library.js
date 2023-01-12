import { nil } from "../../header.js";
import { isSymbol, isVector } from "../_types.js"
import { to_write } from "../_writer.js"
import { Bug, BiwaError } from "../error.js"
import { List, array_to_list, Cons, isPair, isList } from "../pair.js"
import { Sym } from "../symbol.js"
import { Environment } from "./environment.js"
import { isMacro, Macro } from "./macro.js"

// A R7RS library.
class Library {
  // Holds spec (not Library)
  static currentLibrary = null;
  static featureList = [];

  constructor(environment) {
    this.environment = environment;
    this.exports = new Map();
  }

  static create(spec) {
    const name = spec.map(x => to_write(x)).join(".");
    const env = Environment.makeToplevelEnvironment(name, `${name}:`);
    return new Library(env);
  }

  /** Interprets imports, exports, etc. of this library
   * (original: interpret-library-declaration)
   * @return Form
   */
  interpretLibraryDeclaration(decl, engine) {
    switch (decl.car) {
      case Sym("begin"):
        return decl.cdr;
      case Sym("import"):
        decl.cdr.forEach(x => this.environment.findAndImportLibrary(x, engine));
        return nil
      case Sym("export"):
        decl.cdr.forEach(x => this.export(x));
        return nil
      case Sym("cond-expand"):
        return this._interpretCondExpand(decl.cdr);
      case Sym("include"):
        TODO
      case Sym("include-library-declarations"):
        TODO
      default:
        throw new BiwaError("interpretLibraryDeclaration: unknown decl.car", decl);
    }
  }

//  /** Import a library into this library
//   * @param {lib} Library
//   */
//  import(lib) {
//    this.environment.importLibrary(lib);
//  }

  nameMap() {
    const ret = [];
    this.exports.forEach((nickname, id) => {
      ret.push([nickname, this.environment.assq(id)])
    });
    return ret;
  }

  exportMacro(sym, transformer) {
    const expander = new Macro(sym.name, this.environment, transformer);
    this.environment.installExpander(sym, expander);
    this.export(sym);
  }

  // Add a exported item (eg. library function)
  addExport(sym, value) {
    if (value === undefined) {
      throw new Bug(`addExport: tried to insert \`undefined\` as ${sym} (use \`undef\` if intentional)`)
    }
    this.environment.set(sym, value);
    this.export(sym);
  }

  // Register an export item of the current library
  // `spec` is either a symbol or `(id nickname)`
  export(spec) {
    let id, nickname;
    if (isSymbol(spec)) {
      id = spec; nickname = spec;
    } else {
      id = spec.cdr.car; nickname = spec.cdr.cdr.car;
    }
    this.exports.set(nickname, id);
  }

  static _interpretCondExpand(clauses) {
    TODO
  }

  to_write() {
    return "#<Library>"
  }
}

const isLibrary = obj => obj instanceof Library;

export { Library, isLibrary };
