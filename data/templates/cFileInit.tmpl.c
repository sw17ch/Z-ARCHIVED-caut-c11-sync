{{#cLibTypes}}
{{#ctDetails}}
{{#CBuiltIn}}
void init_{{ctName}}({{ctName}} * _c_obj) {
  *_c_obj = 0;
}
{{/CBuiltIn}}
{{#CConst}}
void init_{{ctName}}({{ctdReprName}} * _c_obj) {
  *_c_obj = {{ctdConstVal}};
}
{{/CConst}}
{{#CArray}}
void init_{{ctName}}(struct {{ctName}} * _c_obj) {
  for (size_t _c_i = 0; _c_i < ARR_LEN(_c_obj->elems); _c_i++) {
    init_{{ctdReprName}}(&_c_obj->elems[_c_i]);
  }
}
{{/CArray}}
{{#CVector}}
void init_{{ctName}}(struct {{ctName}} * _c_obj) {
  _c_obj->_length = 0;
}
{{/CVector}}
{{#CScalar}}
void init_{{ctName}}({{ctName}} * _c_obj) {
  *_c_obj = 0;
}
{{/CScalar}}
{{#CStruct}}
void init_{{ctName}}(struct {{ctName}} * _c_obj) {
{{#ctdFields}}
{{#CNamedRef}}
  init_{{cnrRefName}}(&_c_obj->{{cnrName}});
{{/CNamedRef}}
{{#CNamedEmpty}}
  /* No data for field `{{cnrName}}`. */
{{/CNamedEmpty}}
{{/ctdFields}}
}
{{/CStruct}}
{{#CEnum}}
void init_{{ctName}}(struct {{ctName}} * _c_obj) {
{{#ctdFields.0}}
{{#CNamedRef}}
  _c_obj->_tag = ({{ctdEnumTagReprName}}) {{ctName}}_tag_{{cnrName}};
  init_bool(&_c_obj->a);
{{/CNamedRef}}
{{#CNamedEmpty}}
  _c_obj->_tag = ({{ctdEnumTagReprName}}) {{ctName}}_tag_{{cneName}};
  /* No data associated with field `{{cnrName}}`. */
{{/CNamedEmpty}}
{{/ctdFields.0}}
}
{{/CEnum}}
{{#CSet}}
void init_{{ctName}}(struct {{ctName}} * _c_obj) {
  _c_obj->_flags = 0;
}
{{/CSet}}
{{#CPad}}
void init_{{ctName}}(struct {{ctName}} * _c_obj) {
  for (size_t _c_i = 0; _c_i < sizeof(_c_obj->pad); _c_i++) {
    _c_obj->pad[_c_i] = 0;
  }
}
{{/CPad}}
{{/ctDetails}}

{{/cLibTypes}}
