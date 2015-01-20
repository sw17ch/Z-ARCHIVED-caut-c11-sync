{{#CScalar}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, {{ctName}} const * const _c_obj) {
  return __caut_pack_{{ctdReprName}}(_c_iter, ({{ctdReprName}} *)_c_obj);
}
{{/CScalar}}
