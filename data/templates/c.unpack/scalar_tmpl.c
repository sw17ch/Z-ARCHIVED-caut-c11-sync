{{#CScalar}}
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, {{ctName}} * const _c_obj) {
  return __caut_unpack_{{ctdReprName}}(_c_iter, ({{ctdReprName}} *)_c_obj);
}
{{/CScalar}}
