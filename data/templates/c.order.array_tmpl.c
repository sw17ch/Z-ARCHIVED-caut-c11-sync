{{#CArray}}
enum caut_ord order_{{ctName}}(struct {{ctName}} const * const _c_a, struct {{ctName}} const * const _c_b) {
  for (size_t _c_i = 0; _c_i < ARR_LEN(_c_a->elems); _c_i++) {
    const enum caut_ord _c_o = order_{{ctdReprName}}(&_c_a->elems[_c_i], &_c_b->elems[_c_i]);
    if (caut_ord_eq != _c_o) {
      return _c_o;
    }
  }
  
  return caut_ord_eq;
}
{{/CArray}}
