#ifndef _CAUTERIZE_C11SYNC_{{cLibName}}_
#define _CAUTERIZE_C11SYNC_{{cLibName}}_

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "cauterize.h"

#define NAME_{{cLibName}} "{{cLibName}}"
#define VERSION_{{cLibName}} "{{cLibVersion}}"
#define MIN_SIZE_{{cLibName}} ({{cLibMinSize}})
#define MAX_SIZE_{{cLibName}} ({{cLibMaxSize}})

extern hashtype_t const SCHEMA_HASH_{{cLibName}};

{{! Emit type hash externs. }}
{{#cLibTypes}}
extern hashtype_t const TYPE_HASH_{{cLibName}}_{{ctName}};
{{/cLibTypes}}

/* Type size information. */
{{#cLibTypes}}
#define MIN_SIZE_{{cLibName}}_{{ctName}} ({{ctMinSize}})
#define MAX_SIZE_{{cLibName}}_{{ctName}} ({{ctMaxSize}})

{{/cLibTypes}}

/* Array and vector lengths. */
{{#cLibTypes}}
{{#ctDetails}}
{{#CArray}}
#define CONST_{{cLibName}}_{{ctName}}_LENGTH ({{ctdArrayLen}})
{{/CArray}}
{{#CVector}}
#define CONST_{{cLibName}}_{{ctName}}_MAX_LENGTH ({{ctdVectorMaxLen}})
{{/CVector}}
{{/ctDetails}}
{{/cLibTypes}}

/* Forward delcarations for types. */
{{#cLibTypes}}{{#ctDetails}}
{{#CBuiltIn}}
typedef {{ctdStdType}} {{ctName}};
{{/CBuiltIn}}
{{#CConst}}
typedef {{ctdReprName}} {{ctName}};
{{/CConst}}
{{#CArray}}
struct {{ctName}};
{{/CArray}}
{{#CVector}}
struct {{ctName}};
{{/CVector}}
{{#CScalar}}
typedef {{ctdReprName}} {{ctName}};
{{/CScalar}}
{{#CStruct}}
struct {{ctName}};
{{/CStruct}}
{{#CEnum}}
struct {{ctName}};
{{/CEnum}}
{{#CSet}}
struct {{ctName}};
{{/CSet}}
{{#CPad}}
struct {{ctName}};
{{/CPad}}
{{/ctDetails}}{{/cLibTypes}}

/* Function prototypes. */
{{#cLibTypes}}
{{#ctDetails}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, {{ctName}} const * const _c_obj);
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, {{ctName}} * const _c_obj);
size_t packed_size_{{ctName}}({{ctName}} const * const _c_obj);
void init_{{ctName}}({{ctName}} * _c_obj);
enum caut_ord order_{{ctName}}({{ctName}} const * const _c_a, {{ctName}} const * const _c_b);

{{/ctDetails}}
{{/cLibTypes}}

#endif /* _CAUTERIZE_C11SYNC_{{cLibName}}_ */
