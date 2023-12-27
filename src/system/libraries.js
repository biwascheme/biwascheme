import { List } from "./pair.js"
import { Sym } from "./symbol.js"
import { BiwaError, Bug } from "./error.js"
import { to_write } from "./_writer.js"
import { libSchemeBase } from "../r7rs/base.js"
import { libSchemeCaseLambda } from "../r7rs/case_lambda.js"
import { libSchemeChar } from "../r7rs/char.js"
import { libSchemeComplex } from "../r7rs/complex.js"
import { libSchemeCxr } from "../r7rs/cxr.js"
import { libSchemeEval } from "../r7rs/eval.js"
import { libSchemeFile } from "../r7rs/file.js"
import { libSchemeInexact } from "../r7rs/inexact.js"
import { libSchemeLazy } from "../r7rs/lazy.js"
import { libSchemeLoad } from "../r7rs/load.js"
import { libSchemeProcessContext } from "../r7rs/process_context.js"
import { libSchemeR5RS } from "../r7rs/r5rs.js"
import { libSchemeRead } from "../r7rs/read.js"
import { libSchemeRepl } from "../r7rs/repl.js"
import { libSchemeTime } from "../r7rs/time.js"
import { libSchemeWrite } from "../r7rs/write.js"
import { libBiwaschemeErMacro } from "../lib/biwascheme/er-macro.js"

const mangle = (spec) => to_write(spec);
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

  add(lib) {
    this.libraries.set(mangle(lib.spec), lib);
  }
}

const stdLibraries = new Libraries();
stdLibraries.add(libSchemeBase); 
stdLibraries.add(libSchemeWrite); 
stdLibraries.add(libSchemeWrite); 
stdLibraries.add(libSchemeCaseLambda);
stdLibraries.add(libSchemeChar);
stdLibraries.add(libSchemeComplex);
stdLibraries.add(libSchemeCxr);
stdLibraries.add(libSchemeEval);
stdLibraries.add(libSchemeFile);
stdLibraries.add(libSchemeInexact);
stdLibraries.add(libSchemeLazy);
stdLibraries.add(libSchemeLoad);
stdLibraries.add(libSchemeProcessContext);
stdLibraries.add(libSchemeR5RS);
stdLibraries.add(libSchemeRead);
stdLibraries.add(libSchemeRepl);
stdLibraries.add(libSchemeTime);
stdLibraries.add(libBiwaschemeErMacro); 

export { Libraries, stdLibraries };
