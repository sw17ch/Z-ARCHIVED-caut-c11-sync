{{#CPad}}
void init_{{ctName}}(struct {{ctName}} * _c_obj) {
  for (size_t _c_i = 0; _c_i < sizeof(_c_obj->pad); _c_i++) {
    _c_obj->pad[_c_i] = 0;
  }
}
{{/CPad}}
