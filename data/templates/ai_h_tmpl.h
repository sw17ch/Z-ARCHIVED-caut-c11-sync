{{#specInfo}}
{{#aiInfo}}
#ifndef _CAUTERIZE_CAUT2C11_{{cLibName}}_ai_
#define _CAUTERIZE_CAUT2C11_{{cLibName}}_ai_

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "cauterize.h"
#include "{{cLibName}}.h"

/* static field overhead + tag field overhead + length field overhead */
#define MESSAGE_OVERHEAD_{{cLibName}}_ai ({{aiTypeLength}} + {{aiDataLength}})
#define MESSAGE_MAX_SIZE_{{cLibName}}_ai (MESSAGE_OVERHEAD_{{cLibName}}_ai + MAX_SIZE_{{cLibName}})
#define MESSAGE_MIN_SIZE_{{cLibName}}_ai (MESSAGE_OVERHEAD_{{cLibName}}_ai + MIN_SIZE_{{cLibName}})

typedef uint8_t {{cLibName}}_ai_tag_t[{{aiTypeLength}}];

{{#cLibTypes}}
{{#ctDetails}}
extern {{cLibName}}_ai_tag_t const TYPE_TAG_{{ctName}};
{{/ctDetails}}
{{/cLibTypes}}

struct {{cLibName}}_ai_header {
  {{aiDataLengthDecl}} length;
  {{cLibName}}_ai_tag_t tag;
};

struct libayy_ai {
  enum libayy_ai_tag {
{{#cLibTypes}}
{{#ctDetails}}
    {{cLibName}}_ai_tag_{{ctName}},
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

enum caut_status pack_{{cLibName}}_ai(
  struct caut_pack_iter * const _iter,
  struct {{cLibName}}_ai const * const _obj);
enum caut_status unpack_header_{{cLibName}}_ai(
  struct caut_unpack_iter * _iter,
  struct {{cLibName}}_ai_header * _header);
enum caut_status unpack_{{cLibName}}_ai(
  struct caut_unpack_iter * const _iter,
  struct {{cLibName}}_ai_header * _header,
  struct {{cLibName}}_ai * const _obj);

#endif /* _CAUTERIZE_CAUT2C11_{{cLibName}}_ai_ */
{{/aiInfo}}
{{/specInfo}}
