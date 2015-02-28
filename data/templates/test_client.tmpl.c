{{#specInfo}}
{{#metaInfo}}
#include "{{cLibName}}_meta.h"

#include <unistd.h>
#include <string.h>
#include <stdio.h>

bool read_exactly(FILE * fd, void * buf, size_t nbyte, size_t * rbyte);
int run_client(FILE * si, FILE * so);

int main(int argc, char * argv[]) {
  (void)argc;
  (void)argv;

  return run_client(stdin, stdout);
}

int run_client(FILE * si, FILE * so) {
  struct {{cLibName}}_meta_header * h = malloc(sizeof(*h));
  struct {{cLibName}}_meta * d = malloc(sizeof(*d));
  void * buffer = malloc(MESSAGE_MAX_SIZE_{{cLibName}}_meta);

  struct caut_unpack_iter upack;
  struct caut_pack_iter pack;

  size_t rlen = 0;

  enum caut_status status;

  /* Read and decode header. */
  if (!read_exactly(si, buffer, MESSAGE_OVERHEAD_{{cLibName}}_meta, &rlen)) {
    fprintf(stderr, "Not enough data available for header: expected %d bytes but only got %lu bytes.",
      MESSAGE_OVERHEAD_{{cLibName}}_meta,
      rlen);
    return 1;
  }
  caut_unpack_iter_init(&upack, buffer, rlen);
  if (caut_status_ok != (status = unpack_header_{{cLibName}}_meta(&upack, h))) {
    fprintf(stderr, "Failed to unpack the header: %d.", status);
    return 2;
  }

  /* Read the remaining data as described by the header. */
  if (!read_exactly(si, buffer, h->length, &rlen)) {
    fprintf(stderr, "Not enough data available: expected %d bytes but only got %lu bytes.",
      h->length,
      rlen);
    return 3;
  }
  caut_unpack_iter_init(&upack, buffer, rlen);
  if (caut_status_ok != (status = unpack_{{cLibName}}_meta(&upack, h, d))) {
    fprintf(stderr, "Unable to unpack payload: %d.", status);
    return 4;
  }

  memset(buffer, 0, MAX_SIZE_{{cLibName}});

  caut_pack_iter_init(&pack, buffer, MESSAGE_MAX_SIZE_{{cLibName}}_meta);

  if (caut_status_ok != (status = pack_{{cLibName}}_meta(&pack, d))) {
    fprintf(stderr, "Unable to pack response: %d.", status);
    return 5;
  }

  fwrite(buffer, 1, pack.position, so);

  return 0;
}

/*
 * read_exactly
 *
 *    Automatically re-reads a file descriptor until an error occurs or the
 *    expected number of bytes has been read.
 *
 *    fd - the file descriptor to read from.
 *    buf - the buffer to read into.
 *    nbyte - the number of bytes to read into the buffer.
 *    rbyte - the actual number of bytes read into the buffer. Will always
 *    equal nbyte if all reads were successful.
 *
 *    Returns true when no errors occurred and the proper number of bytes was
 *    read. Returns false otherwise.
 */
bool read_exactly(FILE * fd, void * buf, size_t nbyte, size_t * rbyte) {
  uint8_t * bbuf = buf;
  size_t r = 0;

  while(r < nbyte) {
    uint8_t * next_pos = &(bbuf[r]);
    int l = fread(next_pos, 1, nbyte - r, fd);

    if (l <= 0) {
      break;
    } else {
      r += l;
    }
  }

  if (rbyte) {
    *rbyte = r;
  }

  return (r == nbyte);
}
{{/metaInfo}}
{{/specInfo}}
