{{#specInfo}}
{{#aiInfo}}
#include "{{cLibName}}.h"
#include "{{cLibName}}_ai.h"
#include "greatest.h"

struct {{cLibName}}_ai * ai_x = NULL;
struct {{cLibName}}_ai_header * ai_hy = NULL;
struct {{cLibName}}_ai * ai_y = NULL;
uint8_t * ai_buffer = NULL;

{{#cLibTypes}}
{{#ctDetails}}
TEST assertions_{{ctName}}(void) {
  struct caut_pack_iter pack;
  struct caut_unpack_iter unpack;

  caut_pack_iter_init(&pack, ai_buffer, MESSAGE_MAX_SIZE_{{cLibName}}_ai);
  caut_unpack_iter_init(&unpack, ai_buffer, MESSAGE_MAX_SIZE_{{cLibName}}_ai);

  ai_x->_tag = {{cLibName}}_ai_tag_{{ctName}};
  init_{{ctName}}(&ai_x->msg_{{ctName}});
  memset(ai_y, 'X', sizeof(*ai_y));

  ASSERT(MESSAGE_MAX_SIZE_{{cLibName}}_ai <= sizeof(struct {{cLibName}}_ai));
  ASSERT_EQ(caut_status_ok, pack_{{cLibName}}_ai(&pack, ai_x));

  ASSERT_EQ(caut_status_ok, unpack_header_{{cLibName}}_ai(&unpack, ai_hy));
  ASSERT_EQ(caut_status_ok, unpack_{{cLibName}}_ai(&unpack, ai_hy, ai_y));

  ASSERT_EQ(caut_ord_eq, order_{{ctName}}(&ai_x->msg_{{ctName}}, &ai_y->msg_{{ctName}}));

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

  ai_x = malloc(sizeof(*ai_x));

  ai_hy = malloc(sizeof(*ai_hy));
  ai_y = malloc(sizeof(*ai_y));
  ai_buffer = calloc(1, MESSAGE_MAX_SIZE_{{cLibName}}_ai);

  GREATEST_MAIN_BEGIN();
  RUN_SUITE({{cLibName}}_suite);
  GREATEST_MAIN_END();

  free(ai_x);
  free(ai_y);
  free(ai_hy);
  free(ai_buffer);

  return 0;
}
{{/aiInfo}}
{{/specInfo}}
