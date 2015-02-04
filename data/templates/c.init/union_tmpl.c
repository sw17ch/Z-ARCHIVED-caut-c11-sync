{{#CUnion}}
void init_{{ctName}}(struct {{ctName}} * _c_obj) {
{{#ctdFields.0}}
{{#CNamedRef}}
  _c_obj->_tag = ({{ctdUnionTagReprDecl}}) {{ctName}}_tag_{{cnrName}};
  init_{{cnrRefName}}(&_c_obj->a);
{{/CNamedRef}}
{{#CNamedEmpty}}
  _c_obj->_tag = ({{ctdUnionTagReprDecl}}) {{ctName}}_tag_{{cneName}};
  /* No data associated with field `{{cnrName}}`. */
{{/CNamedEmpty}}
{{/ctdFields.0}}
}
{{/CUnion}}
