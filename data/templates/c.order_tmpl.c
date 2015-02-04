{{#cLibTypes}}
{{#ctDetails}}
{{> c.order/builtin_tmpl.c}}
{{> c.order/array_tmpl.c}}
{{> c.order/vector_tmpl.c}}
{{> c.order/synonym_tmpl.c}}
{{> c.order/record_tmpl.c}}
{{> c.order/union_tmpl.c}}
{{> c.order/combination_tmpl.c}}
{{/ctDetails}}
{{/cLibTypes}}
