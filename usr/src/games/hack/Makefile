#
# Copyright (c) 1988 The Regents of the University of California.
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
#	@(#)Makefile	5.7 (Berkeley) %G%
#
# Hack or Quest Makefile.

CFLAGS=	-O
SRCS=	alloc.c hack.Decl.c hack.apply.c hack.bones.c hack.c hack.cmd.c \
	hack.do.c hack.do_name.c hack.do_wear.c hack.dog.c hack.eat.c \
	hack.end.c hack.engrave.c hack.fight.c hack.invent.c hack.ioctl.c \
	hack.lev.c hack.main.c hack.makemon.c hack.mhitu.c hack.mklev.c \
	hack.mkmaze.c hack.mkobj.c hack.mkshop.c hack.mon.c hack.monst.c \
	hack.o_init.c hack.objnam.c hack.options.c hack.pager.c hack.potion.c \
	hack.pri.c hack.read.c hack.rip.c hack.rumors.c hack.save.c \
	hack.search.c hack.shk.c hack.shknam.c hack.steal.c hack.termcap.c \
	hack.timeout.c hack.topl.c hack.track.c hack.trap.c hack.tty.c \
	hack.u_init.c hack.unix.c hack.vault.c hack.version.c hack.wield.c \
	hack.wizard.c hack.worm.c hack.worn.c hack.zap.c rnd.c
OBJS=	alloc.o hack.Decl.o hack.apply.o hack.bones.o hack.o hack.cmd.o \
	hack.do.o hack.do_name.o hack.do_wear.o hack.dog.o hack.eat.o \
	hack.end.o hack.engrave.o hack.fight.o hack.invent.o hack.ioctl.o \
	hack.lev.o hack.main.o hack.makemon.o hack.mhitu.o hack.mklev.o \
	hack.mkmaze.o hack.mkobj.o hack.mkshop.o hack.mon.o hack.monst.o \
	hack.o_init.o hack.objnam.o hack.options.o hack.pager.o hack.potion.o \
	hack.pri.o hack.read.o hack.rip.o hack.rumors.o hack.save.o \
	hack.search.o hack.shk.o hack.shknam.o hack.steal.o hack.termcap.o \
	hack.timeout.o hack.topl.o hack.track.o hack.trap.o hack.tty.o \
	hack.u_init.o hack.unix.o hack.vault.o hack.version.o hack.wield.o \
	hack.wizard.o hack.worm.o hack.worn.o hack.zap.o rnd.o
MAN=	hack.0

all: hack

hack: ${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS} -ltermcap

makedefs: makedefs.c
	${CC} ${CFLAGS} -o $@ $@.c

hack.onames.h: makedefs def.objects.h
	makedefs > hack.onames.h

install: ${MAN}
	install -s -o games -g bin -m 4700 hack ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f hack; ln -s dm hack; chown games.bin hack)
	-[ -d ${DESTDIR}/usr/games/lib/hackdir ] || mkdir ${DESTDIR}/usr/games/lib/hackdir
	chmod 700 ${DESTDIR}/usr/games/lib/hackdir
	chown games.bin ${DESTDIR}/usr/games/lib/hackdir
	install -c -o games -g bin -m 666 /dev/null ${DESTDIR}/usr/games/lib/hackdir/perm
	install -c -o games -g bin -m 666 /dev/null ${DESTDIR}/usr/games/lib/hackdir/record
	install -c -o games -g bin -m 666 hh ${DESTDIR}/usr/games/lib/hackdir/hh
	rm -f ${DESTDIR}/usr/games/lib/hackdir/bones*
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6

clean:
	rm -f ${OBJS} hack makedefs

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS} makedefs.c

lint: ${SRCS}
	lint ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
