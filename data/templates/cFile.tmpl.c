#include "{{cLibName}}.h"

hashtype_t const SCHEMA_HASH_{{cLibName}} = 
  { {{cLibHashStr}} };

{{#cLibTypes}}
hashtype_t const TYPE_HASH_{{cLibName}}_{{ctName}} =
  { {{cLibHashStr}} };
{{/cLibTypes}}

{{> cFilePackedSize.tmpl.c}}
{{> cFileInit.tmpl.c}}
