{{#CBuiltIn}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, {{ctName}} const * const _c_obj) {
  return __caut_pack_{{ctName}}(_c_iter, _c_obj);
}
{{/CBuiltIn}}
