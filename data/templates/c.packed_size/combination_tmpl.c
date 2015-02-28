{{#CCombination}}
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj) {
  size_t _c_size = sizeof(_c_obj->_flags);

{{#ctdFields}}
{{#CNamedRef}}
  if (_c_obj->_flags & (1ull << {{cnrIndex}})) {
    _c_size += packed_size_{{cnrRefName}}(&_c_obj->{{cnrName}});
  }
{{/CNamedRef}}
{{#CNamedEmpty}}
  if (_c_obj->_flags & (1ull << {{cneIndex}})) {
    /* No data for field `{{cneName}}`. */
  }
{{/CNamedEmpty}}
{{/ctdFields}}

  return _c_size;
}
{{/CCombination}}
