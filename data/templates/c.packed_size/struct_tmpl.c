{{#CStruct}}
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj) {
  size_t _c_size = 0;
  
{{#ctdFields}}
{{#CNamedRef}}
  _c_size += packed_size_{{cnrRefName}}(&_c_obj->{{cnrName}});
{{/CNamedRef}}
{{#CNamedEmpty}}
  case u_tag_a:
    /* No data for field `{{cneName}}`. */
    break;
{{/CNamedEmpty}}
{{/ctdFields}}
  
  return _c_size;
}
{{/CStruct}}
