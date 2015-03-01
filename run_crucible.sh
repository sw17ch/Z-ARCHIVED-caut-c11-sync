#!/bin/sh

# This is what John uses to test the caut-c11-sync generator locally. You may
# need to tweak this to fit your local configuration.

./.cabal-sandbox/bin/cauterize-test \
  crucible --build-cmd="../../.cabal-sandbox/bin/caut-c11-sync-gen --spec=%s --output=c11" \
           --build-cmd="../../.cabal-sandbox/bin/caut-c11-sync-gen-meta --spec=%s --meta=%m --output=c11" \
           --build-cmd="make -C c11" \
           --run-cmd="./c11/test_client" \
           --schema-count=1 \
           --instance-count=1000 \
           --type-count=20 \
           --enc-size=1024
