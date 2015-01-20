{{#CStruct}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, struct {{ctName}} const * const _c_obj) {
{{#ctdFields}}
{{#CNamedRef}}
  STATUS_CHECK(pack_{{cnrRefName}}(_c_iter, &_c_obj->{{cnrName}}));
{{/CNamedRef}}
{{#CNamedEmpty}}
  /* No data for field {{cneName}}. */
{{/CNamedEmpty}}
{{/ctdFields}}
  
  return caut_status_ok;
}
{{/CStruct}}
