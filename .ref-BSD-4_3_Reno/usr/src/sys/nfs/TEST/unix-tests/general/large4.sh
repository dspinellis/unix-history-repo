:
#! /bin/sh
#
#       @(#)large4.sh	1.4 90/01/09 NFS Rev 2 testsuite
#      @(#)large4.sh	1.4 Lachman ONC Test Suite  source

CFLAGS=-O $UTS

cc $CFLAGS -o large large.c&
cc $CFLAGS -o large1 large1.c&
cc $CFLAGS -o large2 large2.c&
cc $CFLAGS -o large3 large3.c&
wait
rm large large1 large2 large3 
