{{#CVector}}
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj) {
  size_t _c_size = sizeof(_c_obj->_length);
  
  for (size_t _c_i = 0; _c_i < _c_obj->_length; _c_i++) {
    _c_size += packed_size_{{ctdReprName}}(&_c_obj->elems[_c_i]);
  }
  
  return _c_size;
}
{{/CVector}}
