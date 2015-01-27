{{#specInfo}}
{{#metaInfo}}
#ifndef _CAUTERIZE_CAUT2C11_{{cLibName}}_meta_
#define _CAUTERIZE_CAUT2C11_{{cLibName}}_meta_

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "cauterize.h"
#include "{{cLibName}}.h"

/* static field overhead + tag field overhead + length field overhead */
#define MESSAGE_OVERHEAD_{{cLibName}}_meta ({{metaTypeLength}} + {{metaDataLength}})
#define MESSAGE_MAX_SIZE_{{cLibName}}_meta (MESSAGE_OVERHEAD_{{cLibName}}_meta + MAX_SIZE_{{cLibName}})
#define MESSAGE_MIN_SIZE_{{cLibName}}_meta (MESSAGE_OVERHEAD_{{cLibName}}_meta + MIN_SIZE_{{cLibName}})

typedef uint8_t {{cLibName}}_meta_tag_t[{{metaTypeLength}}];

{{#cLibTypes}}
{{#ctDetails}}
extern {{cLibName}}_meta_tag_t const TYPE_TAG_{{ctName}};
{{/ctDetails}}
{{/cLibTypes}}

struct {{cLibName}}_meta_header {
  {{metaDataLengthDecl}} length;
  {{cLibName}}_meta_tag_t tag;
};

struct {{cLibName}}_meta {
  enum {{cLibName}}_meta_tag {
{{#cLibTypes}}
{{#ctDetails}}
    {{cLibName}}_meta_tag_{{ctName}},
{{/ctDetails}}
{{/cLibTypes}}
  } _tag;

  union {
{{#cLibTypes}}
{{#ctDetails}}
    {{ctdDecl}} msg_{{ctName}};
{{/ctDetails}}
{{/cLibTypes}}
  };
};

enum caut_status pack_{{cLibName}}_meta(
  struct caut_pack_iter * const _iter,
  struct {{cLibName}}_meta const * const _obj);
enum caut_status unpack_header_{{cLibName}}_meta(
  struct caut_unpack_iter * _iter,
  struct {{cLibName}}_meta_header * _header);
enum caut_status unpack_{{cLibName}}_meta(
  struct caut_unpack_iter * const _iter,
  struct {{cLibName}}_meta_header * _header,
  struct {{cLibName}}_meta * const _obj);

#endif /* _CAUTERIZE_CAUT2C11_{{cLibName}}_meta_ */
{{/metaInfo}}
{{/specInfo}}
