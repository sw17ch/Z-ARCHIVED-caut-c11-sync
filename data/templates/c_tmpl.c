#include "{{cLibName}}.h"

hashtype_t const SCHEMA_HASH_{{cLibName}} = 
  { {{cLibHashStr}} };

{{#cLibTypes}}
hashtype_t const TYPE_HASH_{{cLibName}}_{{ctName}} =
  { {{cLibHashStr}} };
{{/cLibTypes}}

{{> c.pack_tmpl.c}}
{{> c.unpack_tmpl.c}}
{{> c.packed_size_tmpl.c}}
{{> c.init_tmpl.c}}
{{> c.order_tmpl.c}}
