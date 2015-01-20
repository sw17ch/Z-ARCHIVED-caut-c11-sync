{{#CPad}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, struct {{ctName}} const * const _c_obj) {
  return __caut_pack_null_bytes(_c_iter, sizeof(*_c_obj));
}
{{/CPad}}
