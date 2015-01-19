{{#CStruct}}
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, struct {{ctName}} * const _c_obj) {
{{#ctdFields}}
{{#CNamedRef}}
  STATUS_CHECK(unpack_{{cnrRefName}}(_c_iter, &_c_obj->{{cnrName}}));
{{/CNamedRef}}
{{#CNamedEmpty}}
  /* No data for field {{cneName}}. */
{{/CNamedEmpty}}
{{/ctdFields}}

  return caut_status_ok;
}
{{/CStruct}}
