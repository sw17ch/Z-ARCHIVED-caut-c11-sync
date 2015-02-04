{{#cLibTypes}}
{{#ctDetails}}
{{> c.packed_size/builtin_tmpl.c}}
{{> c.packed_size/array_tmpl.c}}
{{> c.packed_size/vector_tmpl.c}}
{{> c.packed_size/synonym_tmpl.c}}
{{> c.packed_size/record_tmpl.c}}
{{> c.packed_size/union_tmpl.c}}
{{> c.packed_size/combination_tmpl.c}}
{{/ctDetails}}
{{/cLibTypes}}
