{{#cLibTypes}}
{{#ctDetails}}
{{> c.packed_size/builtin_tmpl.c}}
{{> c.packed_size/const_tmpl.c}}
{{> c.packed_size/array_tmpl.c}}
{{> c.packed_size/vector_tmpl.c}}
{{> c.packed_size/scalar_tmpl.c}}
{{> c.packed_size/struct_tmpl.c}}
{{> c.packed_size/enum_tmpl.c}}
{{> c.packed_size/set_tmpl.c}}
{{> c.packed_size/pad_tmpl.c}}
{{/ctDetails}}
{{/cLibTypes}}
