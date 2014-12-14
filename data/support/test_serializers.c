#include "greatest.h"
#include "lib/cauterize.h"

TEST identity(void) {
  uint8_t buffer[64];
  struct caut_pack_iter pack;
  struct caut_unpack_iter unpack;

#define ID_TYPE(TYPE, REPR, INIT) \
  { \
    caut_pack_iter_init(&pack, buffer, sizeof(buffer)); \
    caut_unpack_iter_init(&unpack, buffer, sizeof(buffer)); \
    REPR _x = (INIT); \
    ASSERT_EQ(caut_status_ok, __caut_pack_##TYPE(&pack, &_x)); \
    memset(&_x, 0, sizeof(_x)); \
    ASSERT_EQ(caut_status_ok, __caut_unpack_##TYPE(&unpack, &_x)); \
    ASSERT_EQm(#TYPE " isn't ID.", (INIT), _x); \
  }

  ID_TYPE(s8,  int8_t,  10);
  ID_TYPE(s16, int16_t, 11);
  ID_TYPE(s32, int32_t, 12);
  ID_TYPE(s64, int64_t, 13);

  ID_TYPE(u8,  uint8_t,  10);
  ID_TYPE(u16, uint16_t, 11);
  ID_TYPE(u32, uint32_t, 12);
  ID_TYPE(u64, uint64_t, 13);

  ID_TYPE(ieee754s, float, 599.0);
  ID_TYPE(ieee754d, double, 601.2);

  ID_TYPE(bool, uint8_t, true);

#undef ID_TYPE

  PASS();
}

TEST iteration(void) {
  uint8_t buffer[64];
  struct caut_pack_iter pack;
  struct caut_unpack_iter unpack;

  /* Check that packing advances the iterator. */
#define PACK_ITER_TYPE(TYPE, REPR) \
  { \
    caut_pack_iter_init(&pack, buffer, sizeof(buffer)); \
    REPR _x; \
    ASSERT_EQ(sizeof(buffer), caut_pack_iter_remaining(&pack)); \
    ASSERT_EQ(caut_status_ok, __caut_pack_##TYPE(&pack, &_x)); \
    ASSERT_EQ(sizeof(buffer) - sizeof(_x), caut_pack_iter_remaining(&pack)); \
  }

  PACK_ITER_TYPE(s8,  int8_t);
  PACK_ITER_TYPE(s16, int16_t);
  PACK_ITER_TYPE(s32, int32_t);
  PACK_ITER_TYPE(s64, int64_t);

  PACK_ITER_TYPE(u8,  uint8_t);
  PACK_ITER_TYPE(u16, uint16_t);
  PACK_ITER_TYPE(u32, uint32_t);
  PACK_ITER_TYPE(u64, uint64_t);

  PACK_ITER_TYPE(ieee754s, float);
  PACK_ITER_TYPE(ieee754d, double);

  PACK_ITER_TYPE(bool, uint8_t);

#undef PACK_ITER_TYPE

  /* Check that unpacking advances the iterator. */
#define UNPACK_ITER_TYPE(TYPE, REPR) \
  { \
    caut_unpack_iter_init(&unpack, buffer, sizeof(buffer)); \
    REPR _x; \
    ASSERT_EQ(sizeof(buffer), caut_unpack_iter_remaining(&unpack)); \
    ASSERT_EQ(caut_status_ok, __caut_unpack_##TYPE(&unpack, &_x)); \
    ASSERT_EQ(sizeof(buffer) - sizeof(_x), caut_unpack_iter_remaining(&unpack)); \
  }

  UNPACK_ITER_TYPE(s8,  int8_t);
  UNPACK_ITER_TYPE(s16, int16_t);
  UNPACK_ITER_TYPE(s32, int32_t);
  UNPACK_ITER_TYPE(s64, int64_t);

  UNPACK_ITER_TYPE(u8,  uint8_t);
  UNPACK_ITER_TYPE(u16, uint16_t);
  UNPACK_ITER_TYPE(u32, uint32_t);
  UNPACK_ITER_TYPE(u64, uint64_t);

  UNPACK_ITER_TYPE(ieee754s, float);
  UNPACK_ITER_TYPE(ieee754d, double);

  UNPACK_ITER_TYPE(bool, uint8_t);

#undef UNPACK_ITER_TYPE


  PASS();
}

TEST overflow(void) {
  struct caut_pack_iter pack;

#define PACK_OVERFLOW(TYPE, REPR) \
  { \
    uint8_t buffer[sizeof(REPR) - 1]; \
    caut_pack_iter_init(&pack, buffer, sizeof(buffer)); \
    REPR _x; \
    ASSERT_EQ(caut_status_would_overflow, __caut_pack_##TYPE(&pack, &_x)); \
    ASSERT_EQ(sizeof(buffer), caut_pack_iter_remaining(&pack)); \
  }

  PACK_OVERFLOW(s8,  int8_t);
  PACK_OVERFLOW(s16, int16_t);
  PACK_OVERFLOW(s32, int32_t);
  PACK_OVERFLOW(s64, int64_t);

  PACK_OVERFLOW(u8,  uint8_t);
  PACK_OVERFLOW(u16, uint16_t);
  PACK_OVERFLOW(u32, uint32_t);
  PACK_OVERFLOW(u64, uint64_t);

  PACK_OVERFLOW(ieee754s, float);
  PACK_OVERFLOW(ieee754d, double);

  PACK_OVERFLOW(bool, uint8_t);

#undef PACK_OVERFLOW

  /* Ensure exact sizing still works. */
#define PACK_EXACT(TYPE, REPR) \
  { \
    uint8_t buffer[sizeof(REPR)]; \
    caut_pack_iter_init(&pack, buffer, sizeof(buffer)); \
    REPR _x; \
    ASSERT_EQ(caut_status_ok, __caut_pack_##TYPE(&pack, &_x)); \
    ASSERT_EQ(0, caut_pack_iter_remaining(&pack)); \
  }

  PACK_EXACT(s8,  int8_t);
  PACK_EXACT(s16, int16_t);
  PACK_EXACT(s32, int32_t);
  PACK_EXACT(s64, int64_t);

  PACK_EXACT(u8,  uint8_t);
  PACK_EXACT(u16, uint16_t);
  PACK_EXACT(u32, uint32_t);
  PACK_EXACT(u64, uint64_t);

  PACK_EXACT(ieee754s, float);
  PACK_EXACT(ieee754d, double);

  PACK_EXACT(bool, uint8_t);

#undef PACK_EXACT

  PASS();
}

TEST underflow(void) {
  struct caut_unpack_iter unpack;

#define UNPACK_UNDERFLOW(TYPE, REPR) \
  { \
    uint8_t buffer[sizeof(REPR) - 1]; \
    caut_unpack_iter_init(&unpack, buffer, sizeof(buffer)); \
    REPR _x; \
    ASSERT_EQ(caut_status_would_underflow, __caut_unpack_##TYPE(&unpack, &_x)); \
    ASSERT_EQ(sizeof(buffer), caut_unpack_iter_remaining(&unpack)); \
  }

  UNPACK_UNDERFLOW(s8,  int8_t);
  UNPACK_UNDERFLOW(s16, int16_t);
  UNPACK_UNDERFLOW(s32, int32_t);
  UNPACK_UNDERFLOW(s64, int64_t);

  UNPACK_UNDERFLOW(u8,  uint8_t);
  UNPACK_UNDERFLOW(u16, uint16_t);
  UNPACK_UNDERFLOW(u32, uint32_t);
  UNPACK_UNDERFLOW(u64, uint64_t);

  UNPACK_UNDERFLOW(ieee754s, float);
  UNPACK_UNDERFLOW(ieee754d, double);

  UNPACK_UNDERFLOW(bool, uint8_t);

#undef UNPACK_UNDERFLOW

  /* Ensure exact sizing still works. */
#define UNPACK_EXACT(TYPE, REPR) \
  { \
    uint8_t buffer[sizeof(REPR)]; \
    caut_unpack_iter_init(&unpack, buffer, sizeof(buffer)); \
    REPR _x; \
    ASSERT_EQ(caut_status_ok, __caut_unpack_##TYPE(&unpack, &_x)); \
    ASSERT_EQ(0, caut_unpack_iter_remaining(&unpack)); \
  }

  UNPACK_EXACT(s8,  int8_t);
  UNPACK_EXACT(s16, int16_t);
  UNPACK_EXACT(s32, int32_t);
  UNPACK_EXACT(s64, int64_t);

  UNPACK_EXACT(u8,  uint8_t);
  UNPACK_EXACT(u16, uint16_t);
  UNPACK_EXACT(u32, uint32_t);
  UNPACK_EXACT(u64, uint64_t);

  UNPACK_EXACT(ieee754s, float);
  UNPACK_EXACT(ieee754d, double);

  UNPACK_EXACT(bool, uint8_t);

#undef UNPACK_EXACT

  PASS();
}

TEST null_bytes(void) {
  struct caut_pack_iter pack;
  struct caut_unpack_iter unpack;
  uint8_t buffer[81];

  caut_pack_iter_init(&pack, buffer, sizeof(buffer));
  caut_unpack_iter_init(&unpack, buffer, sizeof(buffer));

  /* Pack */
  ASSERT_EQ(caut_status_ok, __caut_pack_null_bytes(&pack, 40));
  ASSERT_EQ(sizeof(buffer) - 40, caut_pack_iter_remaining(&pack));
  ASSERT_EQ(caut_status_ok, __caut_pack_null_bytes(&pack, 40));
  ASSERT_EQ(sizeof(buffer) - 40 - 40, caut_pack_iter_remaining(&pack));

  ASSERT_EQ(caut_status_would_overflow, __caut_pack_null_bytes(&pack, 2));
  ASSERT_EQ(sizeof(buffer) - 40 - 40, caut_pack_iter_remaining(&pack));

  /* Unpack */
  ASSERT_EQ(caut_status_ok, __caut_unpack_and_ignore_bytes(&unpack, 40));
  ASSERT_EQ(sizeof(buffer) - 40, caut_unpack_iter_remaining(&unpack));
  ASSERT_EQ(caut_status_ok, __caut_unpack_and_ignore_bytes(&unpack, 40));
  ASSERT_EQ(sizeof(buffer) - 40 - 40, caut_unpack_iter_remaining(&unpack));

  ASSERT_EQ(caut_status_would_underflow, __caut_unpack_and_ignore_bytes(&unpack, 2));
  ASSERT_EQ(sizeof(buffer) - 40 - 40, caut_unpack_iter_remaining(&unpack));

  PASS();
}


SUITE(serializer_suite) {
  RUN_TEST(identity);
  RUN_TEST(iteration);
  RUN_TEST(overflow);
  RUN_TEST(underflow);
  RUN_TEST(null_bytes);
}
