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
{{#CBuiltIn}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, {{ctName}} const * const _c_obj);
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, {{ctName}} * const _c_obj);
size_t packed_size_{{ctName}}({{ctName}} const * const _c_obj);
void init_{{ctName}}({{ctName}} * _c_obj);
enum caut_ord order_{{ctName}}({{ctName}} const * const _c_a, {{ctName}} const * const _c_b);
{{/CBuiltIn}}
{{#CConst}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, {{ctName}} const * const _c_obj);
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, {{ctName}} * const _c_obj);
size_t packed_size_{{ctName}}({{ctName}} const * const _c_obj);
void init_{{ctName}}({{ctName}} * _c_obj);
enum caut_ord order_{{ctName}}({{ctName}} const * const _c_a, {{ctName}} const * const _c_b);
{{/CConst}}
{{#CArray}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, struct {{ctName}} const * const _c_obj);
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, struct {{ctName}} * const _c_obj);
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj);
void init_{{ctName}}(struct {{ctName}} * _c_obj);
enum caut_ord order_{{ctName}}(struct {{ctName}} const * const _c_a, struct {{ctName}} const * const _c_b);
{{/CArray}}
{{#CVector}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, struct {{ctName}} const * const _c_obj);
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, struct {{ctName}} * const _c_obj);
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj);
void init_{{ctName}}(struct {{ctName}} * _c_obj);
enum caut_ord order_{{ctName}}(struct {{ctName}} const * const _c_a, struct {{ctName}} const * const _c_b);
{{/CVector}}
{{#CScalar}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, {{ctName}} const * const _c_obj);
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, {{ctName}} * const _c_obj);
size_t packed_size_{{ctName}}({{ctName}} const * const _c_obj);
void init_{{ctName}}({{ctName}} * _c_obj);
enum caut_ord order_{{ctName}}({{ctName}} const * const _c_a, {{ctName}} const * const _c_b);
{{/CScalar}}
{{#CStruct}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, struct {{ctName}} const * const _c_obj);
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, struct {{ctName}} * const _c_obj);
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj);
void init_{{ctName}}(struct {{ctName}} * _c_obj);
enum caut_ord order_{{ctName}}(struct {{ctName}} const * const _c_a, struct {{ctName}} const * const _c_b);
{{/CStruct}}
{{#CEnum}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, struct {{ctName}} const * const _c_obj);
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, struct {{ctName}} * const _c_obj);
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj);
void init_{{ctName}}(struct {{ctName}} * _c_obj);
enum caut_ord order_{{ctName}}(struct {{ctName}} const * const _c_a, struct {{ctName}} const * const _c_b);
{{/CEnum}}
{{#CSet}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, struct {{ctName}} const * const _c_obj);
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, struct {{ctName}} * const _c_obj);
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj);
void init_{{ctName}}(struct {{ctName}} * _c_obj);
enum caut_ord order_{{ctName}}(struct {{ctName}} const * const _c_a, struct {{ctName}} const * const _c_b);
{{/CSet}}
{{#CPad}}
enum caut_status pack_{{ctName}}(struct caut_pack_iter * const _c_iter, struct {{ctName}} const * const _c_obj);
enum caut_status unpack_{{ctName}}(struct caut_unpack_iter * const _c_iter, struct {{ctName}} * const _c_obj);
size_t packed_size_{{ctName}}(struct {{ctName}} const * const _c_obj);
void init_{{ctName}}(struct {{ctName}} * _c_obj);
enum caut_ord order_{{ctName}}(struct {{ctName}} const * const _c_a, struct {{ctName}} const * const _c_b);
{{/CPad}}
{{/ctDetails}}
{{/cLibTypes}}

/* Type delcarations. */
{{#cLibTypes}}
{{#ctDetails}}
{{#CArray}}
struct {{ctName}} {
  struct {{ctName}} elems[CONST_{{cLibName}}_{{ctName}}_LENGTH];
};

{{/CArray}}
{{#CVector}}
struct {{ctName}} {
  {{ctdVectorMaxLenReprName}} _length;
  {{ctdReprName}} elems[CONST_{{cLibName}}_{{ctName}}_MAX_LENGTH];
};

{{/CVector}}
{{#CStruct}}
struct {{ctName}} {
{{#ctdFields}}
{{#CNamedRef}}
  {{cnrRefName}} {{cnrName}};
{{/CNamedRef}}
{{#CNamedEmpty}}
  /* No data for field `{{cneName}}`. */
{{/CNamedEmpty}}
{{/ctdFields}}
};

{{/CStruct}}
{{#CEnum}}
struct {{ctName}} {
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

  {{ctdEnumTagReprName}} _tag;

  union {
{{#ctdFields}}
{{#CNamedRef}}
    {{cnrReprName}} {{cnrName}};
{{/CNamedRef}}
{{#CNamedEmpty}}
    /* No data for field `{{cneName}}`. */
{{/CNamedEmpty}}
{{/ctdFields}}
  };
};

{{/CEnum}}
{{#CSet}}
struct {{ctName}} {
  {{ctdSetFlagsReprName}} _flags;

{{#ctdFields}}
{{#CNamedRef}}
  {{cnrRefName}} {{cnrName}};
{{/CNamedRef}}
{{#CNamedEmpty}}
  /* No data for field `{{cnrName}}`. */
{{/CNamedEmpty}}
};

{{/CSet}}
{{#CPad}}
struct {{ctName}} {
  uint8_t pad[MAX_SIZE_{{cLibTypes}}_{{ctName}}];
};

{{/CPad}}
{{/ctDetails}}
{{/cLibTypes}}

#endif /* _CAUTERIZE_C11SYNC_{{cLibName}}_ */
