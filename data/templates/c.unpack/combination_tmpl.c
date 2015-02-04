{{#CCombination}}
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, struct {{ctName}} * const _c_obj) {
  STATUS_CHECK(unpack_{{ctdCombinationFlagsReprName}}(_c_iter, &_c_obj->_flags));

{{#ctdFields}}
{{#CNamedRef}}
  if (_c_obj->_flags & (1 << {{cnrIndex}})) {
    STATUS_CHECK(unpack_{{cnrRefName}}(_c_iter, &_c_obj->{{cnrName}}));
  }
{{/CNamedRef}}
{{#CNamedEmpty}}
  /* Nothin gto unpack for empty field {{cneName}}. */
{{/CNamedEmpty}}
{{/ctdFields}}

  return caut_status_ok;
}

{{/CCombination}}
