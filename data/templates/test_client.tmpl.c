{{#specInfo}}
{{#metaInfo}}
#include "{{cLibName}}_meta.h"
#include "socket99.h"

#include <unistd.h>
#include <string.h>

bool read_exactly(int fd, void * buf, size_t nbyte, size_t * rbyte);
int run_client(int sock);

int main(int argc, char * argv[]) {
  if (argc < 3) {
    printf("Usage: %s [server] [port]\n", argv[0]);
    return -10;
  }

  socket99_config cfg = {
    .host = argv[1],
    .port = atoi(argv[2]),
  };

  socket99_result res;

  if (socket99_open(&cfg, &res)) {
    int r = run_client(res.fd);
    close(res.fd);
    return r;
  } else {
    printf("Unable to open socket. (%d)\n", res.status);
    return 1;
  }
}

int run_client(int sock) {
  struct {{cLibName}}_meta_header * h = malloc(sizeof(*h));
  struct {{cLibName}}_meta * d = malloc(sizeof(*d));
  void * buffer = malloc(MAX_SIZE_{{cLibName}});

  struct caut_unpack_iter upack;
  struct caut_pack_iter pack;

  write(sock, SCHEMA_HASH_{{cLibName}}, sizeof(SCHEMA_HASH_{{cLibName}}));

  size_t rlen = 0;

  /* Read and decode header. */
  if (!read_exactly(sock, buffer, MESSAGE_OVERHEAD_{{cLibName}}_meta, &rlen)) { return -1; }
  caut_unpack_iter_init(&upack, buffer, rlen);
  if (caut_status_ok != unpack_header_{{cLibName}}_meta(&upack, h)) { return -2; }

  /* Read the remaining data as described by the header. */
  if (!read_exactly(sock, buffer, h->length, &rlen)) { return -3; }
  caut_unpack_iter_init(&upack, buffer, rlen);
  if (caut_status_ok != unpack_{{cLibName}}_meta(&upack, h, d)) { return -4; }

  memset(buffer, 0, MAX_SIZE_{{cLibName}});

  caut_pack_iter_init(&pack, buffer, MAX_SIZE_{{cLibName}});

  if (caut_status_ok != pack_{{cLibName}}_meta(&pack, d)) { return -5; }

  write(sock, buffer, pack.position);

  printf("OK.\n");

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
bool read_exactly(int fd, void * buf, size_t nbyte, size_t * rbyte) {
  size_t r = 0;

  while(r < nbyte) {
    int l = read(fd, buf, nbyte - r);
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
