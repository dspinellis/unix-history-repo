# $Header: /b/source/CVS/src/usr.bin/tn3270/Makefile.inc,v 1.3 1993/06/18 05:41:32 cgd Exp $

CFLAGS += -DTERMCAP -DSRCRT -DKLUDGELINEMODE -DUSE_TERMIO -DTN3270
KBD = unix.kbd

.include "${.CURDIR}/../../Makefile.inc"
