{{#CPad}}
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj) {
  (void) _c_obj;
  return MAX_SIZE_{{cLibName}}_{{ctName}};
}
{{/CPad}}
