import { List } from "./pair.js"
import { Sym } from "./symbol.js"
import { BiwaError, Bug } from "./error.js"
import { libSchemeBase } from "../r7rs/base.js"

const mangle = (spec) => spec.to_write;
class Libraries {
  constructor() {
    this.libraries = new Map(); // spec => BiwaScheme.Library
  }
  
  get(spec) {
    const key = mangle(spec)
    if (this.libraries.has(key)) {
      return this.libraries.get(key);
    } else {
      throw new BiwaError("unknown library", spec);
    }
  }

  set(spec, lib) {
    this.libraries.set(mangle(spec), lib);
  }
}

const stdLibraries = new Libraries();
stdLibraries.set(List(Sym('scheme'), Sym('base')), libSchemeBase); 

export { stdLibraries };
