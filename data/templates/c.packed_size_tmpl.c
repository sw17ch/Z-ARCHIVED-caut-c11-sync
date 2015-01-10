{{#cLibTypes}}
{{#ctDetails}}
{{#CBuiltIn}}
size_t packed_size_{{ctName}}({{ctName}} const * const _c_obj) {
  (void) _c_obj;
  return MAX_SIZE_{{cLibName}}_{{ctName}};
{{/CBuiltIn}}
{{#CConst}}
size_t packed_size_{{ctName}}({{ctName}} const * const _c_obj) {
  (void) _c_obj;
  return MAX_SIZE_{{cLibName}}_{{ctName}};
{{/CConst}}
{{#CArray}}
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj) {
  size_t _c_size = 0;
  
  for (size_t _c_i = 0; _c_i < ARR_LEN(_c_obj->elems); _c_i++) {
    _c_size += packed_size_{{ctdReprName}}(&_c_obj->elems[_c_i]);
  }
  
  return _c_size;
{{/CArray}}
{{#CVector}}
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj) {
  size_t _c_size = sizeof(_c_obj->_length);
  
  for (size_t _c_i = 0; _c_i < _c_obj->_length; _c_i++) {
    _c_size += packed_size_{{ctdReprName}}(&_c_obj->elems[_c_i]);
  }
  
  return _c_size;
{{/CVector}}
{{#CScalar}}
size_t packed_size_{{ctName}}({{ctName}} const * const _c_obj) {
  (void) _c_obj;
  return MAX_SIZE_{{cLibName}}_{{ctName}};
{{/CScalar}}
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
{{/CStruct}}
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
{{/CEnum}}
{{#CSet}}
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj) {
  size_t _c_size = sizeof(_c_obj->_flags);

{{#ctdFields}}
{{#CNamedRef}}
  if (_c_obj->_flags & (1 << {{cnrIndex}})) {
    _c_size += packed_size_{{cnrRefName}}(&_c_obj->{{cnrName}});
{{/CNamedRef}}
{{#CNamedEmpty}}
  if (_c_obj->_flags & (1 << {{cneIndex}})) {
    /* No data for field `{{cneName}}`. */
{{/CNamedEmpty}}
  }
{{/ctdFields}}
  
  return _c_size;
{{/CSet}}
{{#CPad}}
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj) {
  (void) _c_obj;
  return MAX_SIZE_{{cLibName}}_{{ctName}};
{{/CPad}}
}

{{/ctDetails}}
{{/cLibTypes}}
