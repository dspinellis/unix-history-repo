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
#	@(#)Makefile	1.8 (Berkeley) %G%
#

DESTDIR=
COPTS=	-DSTANDALONE -I../../h -I../..
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

all: ${ALL}

vdformat: ${OBJS} ../srt0.o ../libsa.a
	ld -o vdformat ${LDTOPT} ../srt0.o ${OBJS} ../libsa.a -lc

clean:
	rm -f ${OBJS} ${ALL} a.out core t.c errs

../srt0.o: 
	@(cd ..; make srt0.o)

../libsa.a: 
	@(cd ..; make libsa.a)

install:
	install -o bin -g bin -m 644 vdformat ${DESTDIR}/stand/vdformat

depend:
	mkdep ${COPTS} ${SRCS}

tags:
	ctags ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

cmd.o: cmd.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h ../../h/param.h
cmd.o: /usr/include/sys/types.h ../../h/signal.h ../../machine/trap.h
cmd.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
cmd.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
cmd.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h cmd.h
cons.o: cons.c ../../h/../machine/mtpr.h ../../h/param.h
cons.o: /usr/include/sys/types.h ../../h/signal.h ../../machine/trap.h
cons.o: ../../machine/machparam.h ../../h/../tahoe/cp.h
config.o: config.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
config.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
config.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
config.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
config.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
correct.o: correct.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
correct.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
correct.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
correct.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
correct.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
correct.o: cmd.h
delete.o: delete.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
delete.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
delete.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
delete.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
delete.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
delete.o: cmd.h
disklabel.o: disklabel.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
disklabel.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
disklabel.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
disklabel.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
disklabel.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
disklabel.o: cmd.h
exercise.o: exercise.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
exercise.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
exercise.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
exercise.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
exercise.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
help.o: help.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h ../../h/param.h
help.o: /usr/include/sys/types.h ../../h/signal.h ../../machine/trap.h
help.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
help.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
help.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h cmd.h
format.o: format.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
format.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
format.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
format.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
format.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
info.o: info.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h ../../h/param.h
info.o: /usr/include/sys/types.h ../../h/signal.h ../../machine/trap.h
info.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
info.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
info.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
io.o: io.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h ../../h/param.h
io.o: /usr/include/sys/types.h ../../h/signal.h ../../machine/trap.h
io.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
io.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
io.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h cmd.h
list.o: list.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h ../../h/param.h
list.o: /usr/include/sys/types.h ../../h/signal.h ../../machine/trap.h
list.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
list.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
list.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
maps.o: maps.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h ../../h/param.h
maps.o: /usr/include/sys/types.h ../../h/signal.h ../../machine/trap.h
maps.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
maps.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
maps.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
proc_cmd.o: proc_cmd.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
proc_cmd.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
proc_cmd.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
proc_cmd.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
proc_cmd.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
proc_cmd.o: cmd.h
profile.o: profile.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
profile.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
profile.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
profile.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
profile.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
relocate.o: relocate.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
relocate.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
relocate.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
relocate.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
relocate.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
relocate.o: cmd.h
reset.o: reset.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
reset.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
reset.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
reset.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
reset.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
smd.o: smd.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h ../../h/param.h
smd.o: /usr/include/sys/types.h ../../h/signal.h ../../machine/trap.h
smd.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
smd.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
smd.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
smd_e.o: smd_e.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
smd_e.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
smd_e.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
smd_e.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
smd_e.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
start.o: start.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
start.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
start.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
start.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
start.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
status.o: status.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
status.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
status.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
status.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
status.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
strpbrk.o: strpbrk.c
strspn.o: strspn.c
strtok.o: strtok.c
util.o: util.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h ../../h/param.h
util.o: /usr/include/sys/types.h ../../h/signal.h ../../machine/trap.h
util.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
util.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
util.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h cmd.h
vdfmt.o: vdfmt.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
vdfmt.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
vdfmt.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
vdfmt.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
vdfmt.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
verify.o: verify.c vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
verify.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
verify.o: ../../machine/trap.h ../../machine/machparam.h ../../h/buf.h
verify.o: ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
verify.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
