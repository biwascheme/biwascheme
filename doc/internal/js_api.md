## BiwaScheme JavaScript API

Moved to [reference.md](http://www.biwascheme.org/doc/reference.html#js-api)


### Type conversion

* Scheme number == JavaScript number
* Scheme string == JavaScript string
* Scheme vector == JavaScript array

* Scheme nil = `BiwaScheme.nil`
* Scheme pair = `BiwaScheme.Pair`
  * make a pair in js : `new BiwaScheme.Pair(1, 2)`
  * check x is a pair : `BiwaScheme.isPair(x)`  (returns `false` for nil)
  * check x is a proper list : `BiwaScheme.isList(x)`  (returns `true` for nil)
  * pair -> js array : `list.to_array()`
  * js array -> list (shallow): `BiwaScheme.array_to_list(1, 2, [3])` => `(list 1 2 (vector 3 4))`
  * js array -> list (deep): `BiwaScheme.deep_array_to_list(1, 2, [3])` => `(list 1 2 (list 3 4))`
  * alist -> js obj : `BiwaScheme.alist_to_js_obj(alist)`
    * eg. `'((a 1) (b 2))` is converted to `{a: 1, b: 2}`
  * js obj -> alist : `BiwaScheme.js_obj_to_alist({a: 1, b: 2})`

* Scheme symbol = `BiwaScheme.Symbol`
  * js string -> symbol : `Sym("asdf")`
  * symbol -> js string : `sym.name`

* Scheme function = an array, with `ary.closure_p` is set to `true`
  * `BiwaScheme.isClosure(x)` returns `true` when `x` is a Scheme function
