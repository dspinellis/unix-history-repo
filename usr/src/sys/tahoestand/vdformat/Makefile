#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	1.11 (Berkeley) 6/22/90
#

DESTDIR=
COPTS=	-DSTANDALONE -I../../sys -I../..
CFLAGS=	-O ${COPTS}
RELOC=	800
LDTOPT= -T ${RELOC} -e _entry -x
SRCS=	cmd.c cons.c config.c correct.c delete.c disklabel.c exercise.c help.c \
	format.c info.c io.c list.c maps.c proc_cmd.c profile.c relocate.c \
	reset.c smd.c smd_e.c start.c status.c util.c vdfmt.c verify.c
OBJS=	cmd.o cons.o config.o correct.o delete.o disklabel.o exercise.o help.o \
	format.o info.o io.o list.o maps.o proc_cmd.o profile.o relocate.o \
	reset.o smd.o smd_e.o start.o status.o util.o vdfmt.o verify.o
ALL=	vdformat
MAN=	vdformat.0

all: ${ALL}

vdformat: ${OBJS} ../srt0.o ../libsa.a
	ld -o vdformat ${LDTOPT} ../srt0.o ${OBJS} ../libsa.a -lc

clean:
	rm -f ${OBJS} ${ALL} a.out core t.c errs

cleandir: clean
	rm -f ${MAN} tags .depend

../srt0.o: 
	@(cd ..; make srt0.o)

../libsa.a: 
	@(cd ..; make libsa.a)

install: ${MAN}
	install -o bin -g bin -m 644 vdformat ${DESTDIR}/stand/vdformat
	install -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat8/tahoe

depend: ${SRCS}
	mkdep ${COPTS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
