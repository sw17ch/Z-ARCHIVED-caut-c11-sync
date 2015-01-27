{{#specInfo}}
{{#metaInfo}}
#include "{{cLibName}}_meta.h"
#include <string.h>

#define CHECKED_PACK(TYPE_NAME) \
  do { \
    /* Pack the tag. */ \
    for (size_t _i = 0; _i < sizeof(TYPE_TAG_##TYPE_NAME); _i++) { \
      STATUS_CHECK(__caut_pack_u8(_iter, &TYPE_TAG_##TYPE_NAME[_i])); \
    } \
    _data_position = _iter->position; \
\
    /* Pack the object. */ \
    STATUS_CHECK(pack_##TYPE_NAME(_iter, &_obj->msg_##TYPE_NAME)); \
\
    /* Now, figure out what the length is and pack that. */ \
    _encode_len = _iter->position - _data_position; \
    memmove(_len_pointer, &_encode_len, sizeof(_encode_len)); \
  } while(0)

#define CHECKED_UNPACK(TYPE_NAME) \
  do { \
    if (0 == memcmp(_header->tag, TYPE_TAG_##TYPE_NAME, sizeof(_header->tag))) { \
      _obj->_tag = {{cLibName}}_meta_tag_##TYPE_NAME; \
      STATUS_CHECK(unpack_##TYPE_NAME(_iter, &_obj->msg_##TYPE_NAME)); \
      return caut_status_ok; \
    } \
  } while(0)

{{#metaTypes}}
{{cLibName}}_meta_tag_t const TYPE_TAG_{{cMetaTypeName}} = { {{cMetaPrefix}} };
{{/metaTypes}}

enum caut_status pack_{{cLibName}}_meta(struct caut_pack_iter * const _iter, struct {{cLibName}}_meta const * const _obj) {
  {{metaDataLengthDecl}} _encode_len = 0;
  size_t _data_position = 0;
  void * _len_pointer = NULL;

  /* Remember where the length should go. */
  _len_pointer = &_iter->buffer[_iter->position];

  /* Dummy pack the length so that we know that the length *CAN* be stored. */
  STATUS_CHECK(__caut_pack_{{metaDataLengthBuiltInRepr}}(_iter, &_encode_len));

  switch(_obj->_tag) {
{{#cLibTypes}}
{{#ctDetails}}
  case {{cLibName}}_meta_tag_{{ctName}}: CHECKED_PACK({{ctName}}); break;
{{/ctDetails}}
{{/cLibTypes}}
  default:
    return caut_status_invalid_tag;
  }

  return caut_status_ok;
}

enum caut_status unpack_header_{{cLibName}}_meta(struct caut_unpack_iter * _iter, struct {{cLibName}}_meta_header * _header) {
  STATUS_CHECK(__caut_unpack_{{metaDataLengthBuiltInRepr}}(_iter, &_header->length));

  for (size_t _i = 0; _i < sizeof(_header->tag); _i++) {
    STATUS_CHECK(__caut_unpack_u8(_iter, &_header->tag[_i]));
  }

  return caut_status_ok;
}

enum caut_status unpack_{{cLibName}}_meta(struct caut_unpack_iter * const _iter, struct {{cLibName}}_meta_header * _header, struct {{cLibName}}_meta * const _obj) {
{{#cLibTypes}}
{{#ctDetails}}
  CHECKED_UNPACK({{ctName}});
{{/ctDetails}}
{{/cLibTypes}}

  return caut_status_invalid_tag;
}

{{/metaInfo}}
{{/specInfo}}
