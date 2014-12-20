{{#cLibTypes}}
{{#ctDetails}}
{{#CBuiltIn}}
enum caut_ord order_{{ctName}}({{ctName}} const * const _c_a, {{ctName}} const * const _c_b) {
  return CAUT_ORDER(*_c_a, *_c_b);
}
{{/CBuiltIn}}
{{#CConst}}
enum caut_ord order_{{ctName}}({{ctName}} const * const _c_a, {{ctName}} const * const _c_b) {
  (void)_c_a;
  (void)_c_b;
  return caut_ord_eq;
}
{{/CConst}}
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
{{#CVector}}
enum caut_ord order_{{ctName}}(struct {{ctName}} const * const _c_a, struct {{ctName}} const * const _c_b) {
  for (size_t _c_i = 0; _c_i < _c_a->_length && _c_i < _c_b->_length; _c_i++) {
    const enum caut_ord _c_o = order_{{ctdReprName}}(&_c_a->elems[_c_i], &_c_b->elems[_c_i]);
    if (caut_ord_eq != _c_o) {
      return _c_o;
    }
  }
  
  return CAUT_ORDER(_c_a->_length, _c_b->_length);
}
{{/CVector}}
{{#CScalar}}
enum caut_ord order_{{ctName}}({{ctName}} const * const _c_a, {{ctName}} const * const _c_b) {
  return CAUT_ORDER(*_c_a, *_c_b);
}
{{/CScalar}}
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
{{#CEnum}}
enum caut_ord order_{{ctName}}(struct {{ctName}} const * const _c_a, struct {{ctName}} const * const _c_b) {
  enum caut_ord _c_o = CAUT_ORDER(_c_a->_tag, _c_b->_tag);
  
  if (caut_ord_eq != _c_o) {
    return _c_o;
  }
  
  switch(_c_a->_tag) {
{{#ctdFields}}
{{#CNamedRef}}
  case {{ctName}}_tag_{{cnrName}}:
    return order_{{cnrRefName}}(&_c_a->{{cnrName}}, &_c_b->{{cnrName}});
    break;
{{/CNamedRef}}
{{#CNamedEmpty}}
  case {{ctName}}_tag_{{cneName}}:
    /* No data for field `{{cneName}}`. */
    return caut_ord_eq;
    break;
{{/CNamedEmpty}}
{{/ctdFields}}
  default:
    return caut_ord_eq;
  }
  
  return caut_ord_eq;
}
{{/CEnum}}
{{#CSet}}
{{/CSet}}
{{#CPad}}
{{/CPad}}
{{/ctDetails}}
{{/cLibTypes}}
