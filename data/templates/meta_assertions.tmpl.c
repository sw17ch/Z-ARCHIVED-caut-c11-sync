{{#specInfo}}
{{#metaInfo}}
#include "{{cLibName}}.h"
#include "{{cLibName}}_meta.h"
#include "greatest.h"

struct {{cLibName}}_meta * meta_x = NULL;
struct {{cLibName}}_meta_header * meta_hy = NULL;
struct {{cLibName}}_meta * meta_y = NULL;
uint8_t * meta_buffer = NULL;

{{#cLibTypes}}
{{#ctDetails}}
TEST assertions_{{ctName}}(void) {
  struct caut_pack_iter pack;
  struct caut_unpack_iter unpack;

  caut_pack_iter_init(&pack, meta_buffer, MESSAGE_MAX_SIZE_{{cLibName}}_meta);
  caut_unpack_iter_init(&unpack, meta_buffer, MESSAGE_MAX_SIZE_{{cLibName}}_meta);

  meta_x->_tag = {{cLibName}}_meta_tag_{{ctName}};
  init_{{ctName}}(&meta_x->msg_{{ctName}});
  memset(meta_y, 'X', sizeof(*meta_y));

  ASSERT(MESSAGE_MAX_SIZE_{{cLibName}}_meta <= sizeof(struct {{cLibName}}_meta));
  ASSERT_EQ(caut_status_ok, pack_{{cLibName}}_meta(&pack, meta_x));

  ASSERT_EQ(caut_status_ok, unpack_header_{{cLibName}}_meta(&unpack, meta_hy));
  ASSERT_EQ(caut_status_ok, unpack_{{cLibName}}_meta(&unpack, meta_hy, meta_y));

  ASSERT_EQ(caut_ord_eq, order_{{ctName}}(&meta_x->msg_{{ctName}}, &meta_y->msg_{{ctName}}));

  PASS();
}
{{/ctDetails}}
{{/cLibTypes}}

SUITE({{cLibName}}_suite) {
{{#cLibTypes}}
{{#ctDetails}}
    RUN_TEST(assertions_{{ctName}});
{{/ctDetails}}
{{/cLibTypes}}
}

GREATEST_MAIN_DEFS();

int main(int argc, char * argv[]) {
  (void)argc;
  (void)argv;

  meta_x = malloc(sizeof(*meta_x));

  meta_hy = malloc(sizeof(*meta_hy));
  meta_y = malloc(sizeof(*meta_y));
  meta_buffer = calloc(1, MESSAGE_MAX_SIZE_{{cLibName}}_meta);

  GREATEST_MAIN_BEGIN();
  RUN_SUITE({{cLibName}}_suite);
  GREATEST_MAIN_END();

  free(meta_x);
  free(meta_y);
  free(meta_hy);
  free(meta_buffer);

  return 0;
}
{{/metaInfo}}
{{/specInfo}}
