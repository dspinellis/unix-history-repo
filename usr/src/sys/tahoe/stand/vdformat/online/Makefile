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
#	%W% (Berkeley) %G%
#

DESTDIR=
COPTS=	-I/sys/h -I/sys
CFLAGS=	-O ${COPTS}
SRCS=	cmd.c config.c correct.c delete.c disklabel.c exercise.c help.c \
	format.c info.c io.c list.c maps.c proc_cmd.c profile.c relocate.c \
	reset.c smd.c smd_e.c start.c status.c strpbrk.c strspn.c strtok.c \
	util.c vdfmt.c verify.c
OBJS=	cmd.o config.o correct.o delete.o disklabel.o exercise.o help.o \
	format.o info.o io.o list.o maps.o proc_cmd.o profile.o relocate.o \
	reset.o smd.o smd_e.o start.o status.o strpbrk.o strspn.o strtok.o \
	util.o vdfmt.o verify.o
ALL=	vdformat

all: ${ALL}

vdformat: ${OBJS}
	cc -o vdformat ${OBJS}

clean:
	rm -f ${OBJS} ${ALL} a.out core t.c errs tags


depend:
	mkdep ${COPTS} ${SRCS}

tags:
	ctags ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

cmd.o: cmd.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h /sys/h/param.h
cmd.o: /usr/include/sys/types.h /sys/h/signal.h /sys/machine/trap.h
cmd.o: /sys/machine/machparam.h /sys/machine/endian.h /sys/h/buf.h
cmd.o: /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h /sys/tahoevba/vbaparam.h
cmd.o: /sys/tahoevba/vdreg.h /sys/tahoe/cp.h cmd.h
cons.o: cons.c /sys/h/../machine/mtpr.h /sys/h/param.h /usr/include/sys/types.h
cons.o: /sys/h/signal.h /sys/machine/trap.h /sys/machine/machparam.h
cons.o: /sys/machine/endian.h /sys/h/../tahoe/cp.h
config.o: config.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h
config.o: /sys/h/param.h /usr/include/sys/types.h /sys/h/signal.h
config.o: /sys/machine/trap.h /sys/machine/machparam.h /sys/machine/endian.h
config.o: /sys/h/buf.h /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h
config.o: /sys/tahoevba/vbaparam.h /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
correct.o: correct.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h
correct.o: /sys/h/param.h /usr/include/sys/types.h /sys/h/signal.h
correct.o: /sys/machine/trap.h /sys/machine/machparam.h /sys/machine/endian.h
correct.o: /sys/h/buf.h /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h
correct.o: /sys/tahoevba/vbaparam.h /sys/tahoevba/vdreg.h /sys/tahoe/cp.h cmd.h
delete.o: delete.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h
delete.o: /sys/h/param.h /usr/include/sys/types.h /sys/h/signal.h
delete.o: /sys/machine/trap.h /sys/machine/machparam.h /sys/machine/endian.h
delete.o: /sys/h/buf.h /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h
delete.o: /sys/tahoevba/vbaparam.h /sys/tahoevba/vdreg.h /sys/tahoe/cp.h cmd.h
disklabel.o: disklabel.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h
disklabel.o: /sys/h/param.h /usr/include/sys/types.h /sys/h/signal.h
disklabel.o: /sys/machine/trap.h /sys/machine/machparam.h /sys/machine/endian.h
disklabel.o: /sys/h/buf.h /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h
disklabel.o: /sys/tahoevba/vbaparam.h /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
disklabel.o: cmd.h
exercise.o: exercise.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h
exercise.o: /sys/h/param.h /usr/include/sys/types.h /sys/h/signal.h
exercise.o: /sys/machine/trap.h /sys/machine/machparam.h /sys/machine/endian.h
exercise.o: /sys/h/buf.h /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h
exercise.o: /sys/tahoevba/vbaparam.h /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
help.o: help.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h /sys/h/param.h
help.o: /usr/include/sys/types.h /sys/h/signal.h /sys/machine/trap.h
help.o: /sys/machine/machparam.h /sys/machine/endian.h /sys/h/buf.h
help.o: /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h /sys/tahoevba/vbaparam.h
help.o: /sys/tahoevba/vdreg.h /sys/tahoe/cp.h cmd.h
format.o: format.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h
format.o: /sys/h/param.h /usr/include/sys/types.h /sys/h/signal.h
format.o: /sys/machine/trap.h /sys/machine/machparam.h /sys/machine/endian.h
format.o: /sys/h/buf.h /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h
format.o: /sys/tahoevba/vbaparam.h /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
info.o: info.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h /sys/h/param.h
info.o: /usr/include/sys/types.h /sys/h/signal.h /sys/machine/trap.h
info.o: /sys/machine/machparam.h /sys/machine/endian.h /sys/h/buf.h
info.o: /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h /sys/tahoevba/vbaparam.h
info.o: /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
io.o: io.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h /sys/h/param.h
io.o: /usr/include/sys/types.h /sys/h/signal.h /sys/machine/trap.h
io.o: /sys/machine/machparam.h /sys/machine/endian.h /sys/h/buf.h
io.o: /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h /sys/tahoevba/vbaparam.h
io.o: /sys/tahoevba/vdreg.h /sys/tahoe/cp.h cmd.h
list.o: list.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h /sys/h/param.h
list.o: /usr/include/sys/types.h /sys/h/signal.h /sys/machine/trap.h
list.o: /sys/machine/machparam.h /sys/machine/endian.h /sys/h/buf.h
list.o: /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h /sys/tahoevba/vbaparam.h
list.o: /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
maps.o: maps.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h /sys/h/param.h
maps.o: /usr/include/sys/types.h /sys/h/signal.h /sys/machine/trap.h
maps.o: /sys/machine/machparam.h /sys/machine/endian.h /sys/h/buf.h
maps.o: /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h /sys/tahoevba/vbaparam.h
maps.o: /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
proc_cmd.o: proc_cmd.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h
proc_cmd.o: /sys/h/param.h /usr/include/sys/types.h /sys/h/signal.h
proc_cmd.o: /sys/machine/trap.h /sys/machine/machparam.h /sys/machine/endian.h
proc_cmd.o: /sys/h/buf.h /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h
proc_cmd.o: /sys/tahoevba/vbaparam.h /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
proc_cmd.o: cmd.h
profile.o: profile.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h
profile.o: /sys/h/param.h /usr/include/sys/types.h /sys/h/signal.h
profile.o: /sys/machine/trap.h /sys/machine/machparam.h /sys/machine/endian.h
profile.o: /sys/h/buf.h /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h
profile.o: /sys/tahoevba/vbaparam.h /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
relocate.o: relocate.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h
relocate.o: /sys/h/param.h /usr/include/sys/types.h /sys/h/signal.h
relocate.o: /sys/machine/trap.h /sys/machine/machparam.h /sys/machine/endian.h
relocate.o: /sys/h/buf.h /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h
relocate.o: /sys/tahoevba/vbaparam.h /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
relocate.o: cmd.h
reset.o: reset.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h /sys/h/param.h
reset.o: /usr/include/sys/types.h /sys/h/signal.h /sys/machine/trap.h
reset.o: /sys/machine/machparam.h /sys/machine/endian.h /sys/h/buf.h
reset.o: /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h /sys/tahoevba/vbaparam.h
reset.o: /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
smd.o: smd.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h /sys/h/param.h
smd.o: /usr/include/sys/types.h /sys/h/signal.h /sys/machine/trap.h
smd.o: /sys/machine/machparam.h /sys/machine/endian.h /sys/h/buf.h
smd.o: /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h /sys/tahoevba/vbaparam.h
smd.o: /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
smd_e.o: smd_e.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h /sys/h/param.h
smd_e.o: /usr/include/sys/types.h /sys/h/signal.h /sys/machine/trap.h
smd_e.o: /sys/machine/machparam.h /sys/machine/endian.h /sys/h/buf.h
smd_e.o: /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h /sys/tahoevba/vbaparam.h
smd_e.o: /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
start.o: start.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h /sys/h/param.h
start.o: /usr/include/sys/types.h /sys/h/signal.h /sys/machine/trap.h
start.o: /sys/machine/machparam.h /sys/machine/endian.h /sys/h/buf.h
start.o: /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h /sys/tahoevba/vbaparam.h
start.o: /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
status.o: status.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h
status.o: /sys/h/param.h /usr/include/sys/types.h /sys/h/signal.h
status.o: /sys/machine/trap.h /sys/machine/machparam.h /sys/machine/endian.h
status.o: /sys/h/buf.h /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h
status.o: /sys/tahoevba/vbaparam.h /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
strpbrk.o: strpbrk.c
strspn.o: strspn.c
strtok.o: strtok.c
util.o: util.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h /sys/h/param.h
util.o: /usr/include/sys/types.h /sys/h/signal.h /sys/machine/trap.h
util.o: /sys/machine/machparam.h /sys/machine/endian.h /sys/h/buf.h
util.o: /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h /sys/tahoevba/vbaparam.h
util.o: /sys/tahoevba/vdreg.h /sys/tahoe/cp.h cmd.h
vdfmt.o: vdfmt.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h /sys/h/param.h
vdfmt.o: /usr/include/sys/types.h /sys/h/signal.h /sys/machine/trap.h
vdfmt.o: /sys/machine/machparam.h /sys/machine/endian.h /sys/h/buf.h
vdfmt.o: /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h /sys/tahoevba/vbaparam.h
vdfmt.o: /sys/tahoevba/vdreg.h /sys/tahoe/cp.h
verify.o: verify.c vdfmt.h /usr/include/setjmp.h /sys/tahoe/mtpr.h
verify.o: /sys/h/param.h /usr/include/sys/types.h /sys/h/signal.h
verify.o: /sys/machine/trap.h /sys/machine/machparam.h /sys/machine/endian.h
verify.o: /sys/h/buf.h /sys/h/disklabel.h /sys/h/inode.h /sys/h/fs.h
verify.o: /sys/tahoevba/vbaparam.h /sys/tahoevba/vdreg.h /sys/tahoe/cp.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
