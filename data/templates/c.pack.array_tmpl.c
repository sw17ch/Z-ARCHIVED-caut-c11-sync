{{#CArray}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, struct {{ctName}} const * const _c_obj) {
  for (size_t _c_i = 0; _c_i < ARR_LEN(_c_obj->elems); _c_i++) {
    STATUS_CHECK(pack_{{ctdReprName}}(_c_iter, &_c_obj->elems[_c_i]));
  }
  
  return caut_status_ok;
}
{{/CArray}}
