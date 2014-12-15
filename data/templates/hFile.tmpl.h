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

{{! Emit type size information. }}
{{#cLibTypes}}
#define MIN_SIZE_{{cLibName}}_{{ctName}} ({{ctMinSize}})
#define MAX_SIZE_{{cLibName}}_{{ctName}} ({{ctMaxSize}})

{{/cLibTypes}}

{{! Emit type forward declarations (if any). }}
{{#cLibTypes}}
{{#ctDetails}}
{{#CBuiltIn}}typedef {{ctdStdType}} {{ctName}};{{/CBuiltIn}}
{{#CConst}}typedef {{ctdReprName}} {{ctName}};{{/CConst}}
{{#CArray}}struct {{ctName}};{{/CArray}}
{{#CVector}}struct {{ctName}};{{/CVector}}
{{#CScalar}}typedef {{ctdReprName}} {{ctName}};{{/CScalar}}
{{#CStruct}}struct {{ctName}};{{/CStruct}}
{{#CEnum}}struct {{ctName}};{{/CEnum}}
{{#CSet}}struct {{ctName}};{{/CSet}}
{{#CPad}}struct {{ctName}};{{/CPad}}
{{/ctDetails}}
{{/cLibTypes}}
