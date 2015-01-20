{{#CConst}}
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, {{ctName}} * const _c_obj) {
  STATUS_CHECK(__caut_unpack_{{ctdReprName}}(_c_iter, _c_obj));

  if (CONST_{{cLibName}}_{{ctName}}_VALUE != *_c_obj) {
    return caut_status_invalid_constant;
  }

  return caut_status_ok;
}

{{/CConst}}
