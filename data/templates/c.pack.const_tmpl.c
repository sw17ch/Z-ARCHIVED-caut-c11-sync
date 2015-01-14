{{#CConst}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, {{ctName}} const * const _c_obj) {
  if (CONST_{{cLibName}}_{{ctName}}_VALUE == *_c_obj) {
    return __caut_pack_{{ctdReprName}}(_c_iter, _c_obj);
  } else {
    return caut_status_invalid_constant;
  }
}
{{/CConst}}
