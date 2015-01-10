{{#CArray}}
void init_{{ctName}}(struct {{ctName}} * _c_obj) {
  for (size_t _c_i = 0; _c_i < ARR_LEN(_c_obj->elems); _c_i++) {
    init_{{ctdReprName}}(&_c_obj->elems[_c_i]);
  }
}
{{/CArray}}
