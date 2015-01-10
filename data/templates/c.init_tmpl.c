{{#cLibTypes}}
{{#ctDetails}}
{{> c.init.builtin_tmpl.c}}
{{> c.init.const_tmpl.c}}
{{> c.init.array_tmpl.c}}
{{> c.init.vector_tmpl.c}}
{{> c.init.scalar_tmpl.c}}
{{> c.init.struct_tmpl.c}}
{{> c.init.enum_tmpl.c}}
{{> c.init.set_tmpl.c}}
{{> c.init.pad_tmpl.c}}
{{/ctDetails}}

{{/cLibTypes}}
