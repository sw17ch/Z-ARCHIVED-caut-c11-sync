{{#CArray}}
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj) {
  size_t _c_size = 0;
  
  for (size_t _c_i = 0; _c_i < ARR_LEN(_c_obj->elems); _c_i++) {
    _c_size += packed_size_{{ctdReprName}}(&_c_obj->elems[_c_i]);
  }
  
  return _c_size;
}
{{/CArray}}
