{{#CPad}}
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, struct {{ctName}} * const _c_obj) {
  enum caut_status _c_s = __caut_unpack_and_ignore_bytes(_c_iter, sizeof(*_c_obj)); 

  if (caut_status_ok == _c_s) {
    for (size_t _c_i = 0; _c_i < sizeof(_c_obj->pad); _c_i++) {
      _c_obj->pad[_c_i] = 0;
    }
  }

  return _c_s;
}
{{/CPad}}
