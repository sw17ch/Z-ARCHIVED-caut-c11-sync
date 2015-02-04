{{#CCombination}}
enum caut_ord order_{{ctName}}(struct {{ctName}} const * const _c_a, struct {{ctName}} const * const _c_b) {
{{#ctdFields}}
{{#CNamedRef}}
  if (FLAG_SET(_c_a, {{cnrIndex}}) && FLAG_SET(_c_b, {{cnrIndex}})) {
    enum caut_ord _c_o;
  
    _c_o = order_{{cnrRefName}}(&_c_a->{{cnrName}}, &_c_b->{{cnrName}});
    if (caut_ord_eq != _c_o) {
      return _c_o;
    }
  } else if (FLAG_SET(_c_b, {{cnrIndex}})) {
    return caut_ord_lt;
  } else if (FLAG_SET(_c_a, {{cnrIndex}})) {
    return caut_ord_gt;
  }
{{/CNamedRef}}
{{#CNamedEmpty}}
  if (FLAG_SET(_c_a, {{cneIndex}}) && !FLAG_SET(_c_b, {{cneIndex}})) {
    return caut_ord_gt;
  } else if (!FLAG_SET(_c_a, {{cneIndex}}) && FLAG_SET(_c_b, {{cneIndex}})) {
    return caut_ord_lt;
  }
{{/CNamedEmpty}}
{{/ctdFields}}

  return caut_ord_eq;
}
{{/CCombination}}
