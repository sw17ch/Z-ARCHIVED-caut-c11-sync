{{#cLibTypes}}
{{#ctDetails}}
{{> cFileInit.builtin.tmpl.c}}
{{> cFileInit.const.tmpl.c}}
{{> cFileInit.array.tmpl.c}}
{{> cFileInit.vector.tmpl.c}}
{{> cFileInit.scalar.tmpl.c}}
{{> cFileInit.struct.tmpl.c}}
{{> cFileInit.enum.tmpl.c}}
{{> cFileInit.set.tmpl.c}}
{{> cFileInit.pad.tmpl.c}}
{{/ctDetails}}

{{/cLibTypes}}
