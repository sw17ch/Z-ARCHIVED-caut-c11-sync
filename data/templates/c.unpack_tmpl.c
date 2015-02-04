{{#cLibTypes}}
{{#ctDetails}}
{{> c.unpack/builtin_tmpl.c}}
{{> c.unpack/array_tmpl.c}}
{{> c.unpack/vector_tmpl.c}}
{{> c.unpack/synonym_tmpl.c}}
{{> c.unpack/record_tmpl.c}}
{{> c.unpack/union_tmpl.c}}
{{> c.unpack/combination_tmpl.c}}
{{/ctDetails}}
{{/cLibTypes}}
