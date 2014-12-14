#include "greatest.h"
#include "lib/cauterize.h"

extern SUITE(iterator_suite);
extern SUITE(serializer_suite);

GREATEST_MAIN_DEFS();

int main(int argc, char * argv[]) {
  (void)argc;
  (void)argv;

  GREATEST_MAIN_BEGIN();
  RUN_SUITE(iterator_suite);
  RUN_SUITE(serializer_suite);
  GREATEST_MAIN_END();

  return 0;
}
