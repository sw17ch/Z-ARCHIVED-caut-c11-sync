{{#CEnum}}
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj) {
  size_t _c_size = sizeof(_c_obj->_tag);
  
  switch (_c_obj->_tag) {
{{#ctdFields}}
{{#CNamedRef}}
  case {{ctName}}_tag_{{cnrName}}:
    _c_size += packed_size_{{cnrRefName}}(&_c_obj->{{cnrName}});
    break;
{{/CNamedRef}}
{{#CNamedEmpty}}
  case {{ctName}}_tag_{{cneName}}:
    /* No data for field `{{cneName}}`. */
    break;
{{/CNamedEmpty}}
{{/ctdFields}}
  default:
    return SIZE_MAX;
  }
  
  return _c_size;
}
{{/CEnum}}
