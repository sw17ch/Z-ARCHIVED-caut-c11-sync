{{#CSynonym}}
size_t packed_size_{{ctName}}({{ctName}} const * const _c_obj) {
  (void) _c_obj;
  return MAX_SIZE_{{cLibName}}_{{ctName}};
}
{{/CSynonym}}
