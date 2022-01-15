// Expanders for core syntaxes
import { List, Cons, isPair, isList } from "../pair.js"
import { Sym } from "../symbol.js"

const installCore = (lib) => {
  lib.exportMacro(Sym("if"), async ([form, xp]) => {
    const l = form.to_array();
    switch (l.length) {
      case 3:
        return List(Sym("if"), await xp.expand(l[1]), await xp.expand(l[2]));
      case 4:
        return List(Sym("if"), await xp.expand(l[1]), await xp.expand(l[2]), await xp.expand(l[3]));
      default:
        throw new BiwaError("malformed if", form);
    }
  });

  //TODO
  //lib.environment.installToplevelBinding(Sym("cons"), CoreEnv["cons"]);
  //lib.export(Sym("cons"));
};

export { installCore };
