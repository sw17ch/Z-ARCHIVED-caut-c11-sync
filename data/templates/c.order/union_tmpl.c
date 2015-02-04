{{#CUnion}}
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
{{/CUnion}}
