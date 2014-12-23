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
{{#needTypeDef}}
typedef {{ctdDecl}} {{ctName}};
{{/needTypeDef}}
{{/CBuiltIn}}
{{#CConst}}
typedef {{ctdReprDecl}} {{ctName}};
{{/CConst}}
{{#CArray}}
struct {{ctName}};
{{/CArray}}
{{#CVector}}
struct {{ctName}};
{{/CVector}}
{{#CScalar}}
typedef {{ctdReprDecl}} {{ctName}};
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
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, {{ctdDecl}} const * const _c_obj);
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, {{ctdDecl}} * const _c_obj);
size_t packed_size_{{ctName}}({{ctdDecl}} const * const _c_obj);
void init_{{ctName}}({{ctdDecl}} * _c_obj);
enum caut_ord order_{{ctName}}({{ctdDecl}} const * const _c_a, {{ctdDecl}} const * const _c_b);

{{/ctDetails}}
{{/cLibTypes}}

/* Type delcarations. */
{{#cLibTypes}}
{{#ctDetails}}
{{#CArray}}
{{ctdDecl}} {
  {{ctdReprDecl}} elems[CONST_{{cLibName}}_{{ctName}}_LENGTH];
};

{{/CArray}}
{{#CVector}}
{{ctdDecl}} {
  {{ctdVectorMaxLenReprDecl}} _length;
  {{ctdReprDecl}} elems[CONST_{{cLibName}}_{{ctName}}_MAX_LENGTH];
};

{{/CVector}}
{{#CStruct}}
{{ctdDecl}} {
{{#ctdFields}}
{{#CNamedRef}}
  {{cnrRefDecl}} {{cnrName}};
{{/CNamedRef}}
{{#CNamedEmpty}}
  /* No data for field `{{cneName}}`. */
{{/CNamedEmpty}}
{{/ctdFields}}
};

{{/CStruct}}
{{#CEnum}}
{{ctdDecl}} {
  enum {{ctName}}_tag {
{{#ctdFields}}
{{#CNamedRef}}
    {{ctName}}_tag_{{cnrName}} = {{cnrIndex}},
{{/CNamedRef}}
{{#CNamedEmpty}}
    {{ctName}}_tag_{{cneName}} = {{cneIndex}},
{{/CNamedEmpty}}
{{/ctdFields}}
  };

  {{ctdEnumTagReprDecl}} _tag;

  union {
{{#ctdFields}}
{{#CNamedRef}}
    {{cnrRefDecl}} {{cnrName}};
{{/CNamedRef}}
{{#CNamedEmpty}}
    /* No data for field `{{cneName}}`. */
{{/CNamedEmpty}}
{{/ctdFields}}
  };
};

{{/CEnum}}
{{#CSet}}
{{ctdDecl}} {
  {{ctdSetFlagsReprDecl}} _flags;

{{#ctdFields}}
{{#CNamedRef}}
  {{cnrRefDecl}} {{cnrName}};
{{/CNamedRef}}
{{#CNamedEmpty}}
  /* No data for field `{{cnrName}}`. */
{{/CNamedEmpty}}
{{/ctdFields}}
};

{{/CSet}}
{{#CPad}}
{{ctdDecl}} {
  uint8_t pad[MAX_SIZE_{{cLibName}}_{{ctName}}];
};

{{/CPad}}
{{/ctDetails}}
{{/cLibTypes}}

#endif /* _CAUTERIZE_C11SYNC_{{cLibName}}_ */
