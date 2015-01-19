{{#CVector}}
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, struct {{ctName}} * const _c_obj) {
  STATUS_CHECK(unpack_{{ctdVectorMaxLenReprName}}(_c_iter, &_c_obj->_length)); 

  for (size_t _c_i = 0; _c_i < _c_obj->_length; _c_i++) {
    STATUS_CHECK(unpack_{{ctdReprName}}(_c_iter, &_c_obj->elems[_c_i]));
  }

  return caut_status_ok;
}
{{/CVector}}
