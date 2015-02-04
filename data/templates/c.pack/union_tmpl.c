{{#CUnion}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, struct {{ctName}} const * const _c_obj) {
  STATUS_CHECK(pack_{{ctdUnionTagReprName}}(_c_iter, &_c_obj->_tag));
  
  switch(_c_obj->_tag) {
{{#ctdFields}}
{{#CNamedRef}}
  case {{ctName}}_tag_{{cnrName}}:
    STATUS_CHECK(pack_{{cnrRefName}}(_c_iter, &_c_obj->{{cnrName}}));
    break;
{{/CNamedRef}}
{{#CNamedEmpty}}
  case {{ctName}}_tag_{{cneName}}:
    /* Nothing to pack for empty field {{ctName}}. */
    break;
{{/CNamedEmpty}}
{{/ctdFields}}
  default:
    return caut_status_invalid_tag;
  }
  
  return caut_status_ok;
}
{{/CUnion}}
