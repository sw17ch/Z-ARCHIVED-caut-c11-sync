#!/bin/sh
clang -Wall -Wextra *.c lib/cauterize.c -o test && ./test
