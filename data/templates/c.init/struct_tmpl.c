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
