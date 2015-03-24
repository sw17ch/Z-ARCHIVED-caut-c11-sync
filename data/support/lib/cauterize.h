#ifndef _CAUTERIZE_H_
#define _CAUTERIZE_H_

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

enum caut_status {
  caut_status_ok,
  caut_status_invalid_constant,
  caut_status_invalid_tag,
  caut_status_would_overflow,
  caut_status_would_underflow,
};

enum caut_ord {
  caut_ord_lt = -1,
  caut_ord_eq = 0,
  caut_ord_gt = 1,
};

/* Array type for a SHA1 hash. */
typedef uint8_t hashtype_t[20];

/* An alias for bool. Must be 1 byte. */
typedef bool caut_bool;

/* Returns the status if it's not `caut_status_ok`. */
#define STATUS_CHECK(S) \
  do { \
    enum caut_status __check_stat = (S); \
    if (caut_status_ok != __check_stat) { \
      return __check_stat; \
    } \
  } while (0)

#define ARR_LEN(A) \
  (sizeof(A) / sizeof(A[0]))

#define CAUT_ASSERT(EXP) \
  do { \
    if (!(EXP)) { \
      printf("ASSERT FAIL on %s:%u\n\t" #EXP, __FILE__, __LINE__); \
      exit(__LINE__); \
    } \
  } while(0)

#define CAUT_ORDER(A,B) \
  ((A) < (B) ? caut_ord_lt : ((A) > (B) ? caut_ord_gt : caut_ord_eq))

#define FLAG_SET(OBJ,IDX) \
  (!!((OBJ)->_flags & (1ull << (IDX))))

struct caut_pack_iter {
  uint8_t * buffer;
  size_t length;
  size_t position;
};

struct caut_unpack_iter {
  uint8_t * buffer;
  size_t length;
  size_t position;
};

void caut_pack_iter_init(struct caut_pack_iter * const iter, void * const buffer, size_t const length);
void * caut_pack_iter_buffer(struct caut_pack_iter * const iter);
size_t caut_pack_iter_remaining(struct caut_pack_iter * const iter);


void caut_unpack_iter_init(struct caut_unpack_iter * const iter, void * const buffer, size_t const length);
void * caut_unpack_iter_buffer(struct caut_unpack_iter * const iter);
size_t caut_unpack_iter_remaining(struct caut_unpack_iter * const iter);




enum caut_status __caut_pack_s8(struct caut_pack_iter * const iter, int8_t const * const obj);
enum caut_status __caut_pack_s16(struct caut_pack_iter * const iter, int16_t const * const obj);
enum caut_status __caut_pack_s32(struct caut_pack_iter * const iter, int32_t const * const obj);
enum caut_status __caut_pack_s64(struct caut_pack_iter * const iter, int64_t const * const obj);

enum caut_status __caut_pack_u8(struct caut_pack_iter * const iter, uint8_t const * const obj);
enum caut_status __caut_pack_u16(struct caut_pack_iter * const iter, uint16_t const * const obj);
enum caut_status __caut_pack_u32(struct caut_pack_iter * const iter, uint32_t const * const obj);
enum caut_status __caut_pack_u64(struct caut_pack_iter * const iter, uint64_t const * const obj);

enum caut_status __caut_pack_f32(struct caut_pack_iter * const iter, float const * const obj);
enum caut_status __caut_pack_f64(struct caut_pack_iter * const iter, double const * const obj);

enum caut_status __caut_pack_bool(struct caut_pack_iter * const iter, bool const * const obj);

enum caut_status __caut_pack_null_bytes(struct caut_pack_iter * const iter, size_t count);




enum caut_status __caut_unpack_s8(struct caut_unpack_iter * const iter, int8_t * const obj);
enum caut_status __caut_unpack_s16(struct caut_unpack_iter * const iter, int16_t * const obj);
enum caut_status __caut_unpack_s32(struct caut_unpack_iter * const iter, int32_t * const obj);
enum caut_status __caut_unpack_s64(struct caut_unpack_iter * const iter, int64_t * const obj);

enum caut_status __caut_unpack_u8(struct caut_unpack_iter * const iter, uint8_t * const obj);
enum caut_status __caut_unpack_u16(struct caut_unpack_iter * const iter, uint16_t * const obj);
enum caut_status __caut_unpack_u32(struct caut_unpack_iter * const iter, uint32_t * const obj);
enum caut_status __caut_unpack_u64(struct caut_unpack_iter * const iter, uint64_t * const obj);

enum caut_status __caut_unpack_f32(struct caut_unpack_iter * const iter, float * const obj);
enum caut_status __caut_unpack_f64(struct caut_unpack_iter * const iter, double * const obj);

enum caut_status __caut_unpack_bool(struct caut_unpack_iter * const iter, bool * const obj);

enum caut_status __caut_unpack_and_ignore_bytes(struct caut_unpack_iter * const iter, size_t count);

#endif /* _CAUTERIZE_H_ */
