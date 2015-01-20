{{#CEnum}}
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, struct {{ctName}} * const _c_obj) {
  STATUS_CHECK(unpack_{{ctdEnumTagReprName}}(_c_iter, &_c_obj->_tag));

  switch(_c_obj->_tag) {
{{#ctdFields}}
{{#CNamedRef}}
  case {{ctName}}_tag_{{cnrName}}:
    STATUS_CHECK(unpack_{{cnrRefName}}(_c_iter, &_c_obj->{{cnrName}}));
    break;
{{/CNamedRef}}
{{#CNamedEmpty}}
  case {{ctName}}_tag_{{cneName}}:
    /* Field `{{cneName}}` is empty and has no size. */
    break;
{{/CNamedEmpty}}
{{/ctdFields}}
  default:
    return caut_status_invalid_tag;
  }

  return caut_status_ok;
}
{{/CEnum}}
