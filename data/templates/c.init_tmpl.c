{{#cLibTypes}}
{{#ctDetails}}
{{> c.init/builtin_tmpl.c}}
{{> c.init/array_tmpl.c}}
{{> c.init/vector_tmpl.c}}
{{> c.init/synonym_tmpl.c}}
{{> c.init/record_tmpl.c}}
{{> c.init/union_tmpl.c}}
{{> c.init/combination_tmpl.c}}
{{/ctDetails}}

{{/cLibTypes}}
