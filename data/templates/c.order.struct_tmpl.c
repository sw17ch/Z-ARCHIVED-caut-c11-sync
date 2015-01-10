{{#CStruct}}
enum caut_ord order_{{ctName}}(struct {{ctName}} const * const _c_a, struct {{ctName}} const * const _c_b) {
  enum caut_ord _c_o;
{{#ctdFields}}
{{#CNamedRef}}
  if (caut_ord_eq != (_c_o = order_{{cnrRefName}}(&_c_a->{{cnrName}}, &_c_b->{{cnrName}}))) {
    return _c_o;
  }
{{/CNamedRef}}
{{#CNamedEmpty}}
  /* No data for field `{{cneName}}`. */
{{/CNamedEmpty}}
{{/ctdFields}}
  return caut_ord_eq;
}
{{/CStruct}}
