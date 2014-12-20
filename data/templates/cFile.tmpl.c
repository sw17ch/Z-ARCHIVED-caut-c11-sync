#include "{{cLibName}}.h"

hashtype_t const SCHEMA_HASH_{{cLibName}} = 
  { {{cLibHashStr}} };

{{#cLibTypes}}
hashtype_t const TYPE_HASH_{{cLibName}}_{{ctName}} =
  { {{cLibHashStr}} };
{{/cLibTypes}}

{{#cLibTypes}}
{{#ctDetails}}
size_t packed_size_{{ctName}}(u8 const * const _c_obj) {
{{#CBuiltIn}}
  return MAX_SIZE_{{cLibName}}_{{ctName}};
{{/CBuiltIn}}
{{#CConst}}
  return MAX_SIZE_{{cLibName}}_{{ctName}};
{{/CConst}}
{{#CArray}}
  size_t _c_size = 0;
  
  for (size_t _c_i = 0; _c_i < ARR_LEN(_c_obj->elems); _c_i++) {
    _c_size += packed_size_{{ctdReprName}}(&_c_obj->elems[_c_i]);
  }
  
  return _c_size;
{{/CArray}}
{{#CVector}}
  size_t _c_size = sizeof(_c_obj->_length);
  
  for (size_t _c_i = 0; _c_i < _c_obj->_length; _c_i++) {
    _c_size += packed_size_{{ctdReprName}}(&_c_obj->elems[_c_i]);
  }
  
  return _c_size;
{{/CVector}}
{{#CScalar}}
  return MAX_SIZE_{{cLibName}}_{{ctName}};
{{/CScalar}}
{{#CStruct}}
  size_t _c_size = 0;
  
{{#ctdFields}}
  _c_size += packed_size_{{cnrRefName}}(&_c_obj->{{cnrName}});
{{/ctdFields}}
  
  return _c_size;
{{/CStruct}}
{{#CEnum}}
  size_t _c_size = sizeof(_c_obj->_tag);
  
  switch (_c_obj->_tag) {
{{#ctdFields}}
{{#CNamedRef}}
  case u_tag_a:
    _c_size += packed_size_{{cnrRefName}}(&_c_obj->{{cnrName}});
    break;
{{/CNamedRef}}
{{#CNamedEmpty}}
  case u_tag_a:
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
  !!!
{{/CSet}}
{{#CPad}}
  !!!
{{/CPad}}
}

{{/ctDetails}}
{{/cLibTypes}}
