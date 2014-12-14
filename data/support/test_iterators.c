#include "greatest.h"
#include "lib/cauterize.h"

TEST pack_iter_init__can_be_initialized(void) {
  struct caut_pack_iter iter;
  uint8_t buffer[128];

  caut_pack_iter_init(&iter, buffer, ARR_LEN(buffer));

  ASSERT_EQ(buffer, iter.buffer);
  ASSERT_EQ(ARR_LEN(buffer), iter.length);
  ASSERT_EQ(0, iter.position);

  PASS();
}

TEST pack_iter_remaining__is_correct(void) {
  struct caut_pack_iter iter;
  uint8_t buffer[128];
  uint8_t x = 5;

  caut_pack_iter_init(&iter, buffer, ARR_LEN(buffer));
  ASSERT_EQ(sizeof(buffer), caut_pack_iter_remaining(&iter));

  ASSERT_EQ(caut_status_ok, __caut_pack_u8(&iter, &x));
  ASSERT_EQ(sizeof(buffer) - 1, caut_pack_iter_remaining(&iter));

  PASS();
}

TEST pack_iter_buffer__is_the_buffer(void) {
  struct caut_pack_iter iter;
  uint8_t buffer[128];

  caut_pack_iter_init(&iter, buffer, ARR_LEN(buffer));

  ASSERT_EQ(buffer, caut_pack_iter_buffer(&iter));

  PASS();
}

TEST unpack_iter_init__can_be_initialized(void) {
  struct caut_unpack_iter iter;
  uint8_t buffer[128];

  caut_unpack_iter_init(&iter, buffer, ARR_LEN(buffer));

  ASSERT_EQ(buffer, iter.buffer);
  ASSERT_EQ(ARR_LEN(buffer), iter.length);
  ASSERT_EQ(0, iter.position);

  PASS();
}

TEST unpack_iter_remaining__is_correct(void) {
  struct caut_unpack_iter iter;
  uint8_t buffer[128];
  uint8_t x;

  caut_unpack_iter_init(&iter, buffer, ARR_LEN(buffer));
  ASSERT_EQ(sizeof(buffer), caut_unpack_iter_remaining(&iter));

  ASSERT_EQ(caut_status_ok, __caut_unpack_u8(&iter, &x));
  ASSERT_EQ(sizeof(buffer) - 1, caut_unpack_iter_remaining(&iter));
  
  PASS();
}

TEST unpack_iter_buffer__is_the_buffer(void) {
  struct caut_unpack_iter iter;
  uint8_t buffer[128];

  caut_unpack_iter_init(&iter, buffer, ARR_LEN(buffer));
  ASSERT_EQ(buffer, caut_unpack_iter_buffer(&iter));

  PASS();
}


SUITE(iterator_suite) {
  RUN_TEST(pack_iter_init__can_be_initialized);
  RUN_TEST(pack_iter_remaining__is_correct);
  RUN_TEST(pack_iter_buffer__is_the_buffer);

  RUN_TEST(unpack_iter_init__can_be_initialized);
  RUN_TEST(unpack_iter_remaining__is_correct);
  RUN_TEST(unpack_iter_buffer__is_the_buffer);
}
