#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation, advertising
# materials, and other materials related to such redistribution and
# use acknowledge that the software was developed by the University
# of California, Berkeley.  The name of the University may not be
# used to endorse or promote products derived from this software
# without specific prior written permission.  THIS SOFTWARE IS PROVIDED
# ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
# WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND
# FITNESS FOR A PARTICULAR PURPOSE.
#
# @(#)Makefile	5.1 (Berkeley) %G%
#

DEFS=	-DHELPFILE=\"/usr/lib/less.help\" -DEDIT_PGM=\"/usr/ucb/vi\" -DRECOMP
CFLAGS=	-O ${DEFS}
LIBC=	/lib/libc.a
SRCS=	ch.c command.c decode.c help.c input.c line.c linenum.c main.c \
	option.c os.c output.c position.c prim.c prompt.c screen.c \
	signal.c tags.c ttyin.c version.c
OBJS=	ch.o command.o decode.o help.o input.o line.o linenum.o main.o \
	option.o os.o output.o position.o prim.o prompt.o screen.o \
	signal.o tags.o ttyin.o version.o

.SUFFIXES: .1 .0

.1.0:
	${DESTDIR}/usr/man/manroff $*.1 > $*.0

all: less lesskey

less: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} -ltermcap

lesskey: lesskey.o ${LIBC}
	${CC} -o $@ ${CFLAGS} lesskey.o 

#funcs.h: ${SRCS}
funcs.h:
	awk -f mkfuncs.awk ${SRCS} > funcs.h

clean: FRC
	rm -f ${OBJS} core less lesskey less.0 lesskey.0

depend: ${SRCS} funcs.h FRC
	mkdep ${CFLAGS} ${SRCS}

install: less.0 lesskey.0 FRC
	install -s -o bin -g bin -m 755 less ${DESTDIR}/usr/ucb/less
	install -s -o bin -g bin -m 755 lesskey ${DESTDIR}/usr/ucb/lesskey
	install -o bin -g bin -m 444 less.0 ${DESTDIR}/usr/man/cat1/less.0
	install -o bin -g bin -m 444 lesskey.0 ${DESTDIR}/usr/man/cat1/lesskey.0
	install -c -o bin -g bin -m 444 less.help ${DESTDIR}/usr/lib/less.help

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

ch.o: ch.c less.h /usr/include/sys/types.h /usr/include/sys/file.h funcs.h
command.o: command.c less.h /usr/include/sys/types.h /usr/include/sys/file.h
command.o: funcs.h position.h cmd.h
decode.o: decode.c less.h /usr/include/sys/types.h /usr/include/sys/file.h
decode.o: funcs.h cmd.h
help.o: help.c less.h /usr/include/sys/types.h /usr/include/sys/file.h funcs.h
input.o: input.c less.h /usr/include/sys/types.h /usr/include/sys/file.h
input.o: funcs.h
line.o: line.c less.h /usr/include/sys/types.h /usr/include/sys/file.h funcs.h
linenum.o: linenum.c less.h /usr/include/sys/types.h /usr/include/sys/file.h
linenum.o: funcs.h position.h
main.o: main.c less.h /usr/include/sys/types.h /usr/include/sys/file.h funcs.h
main.o: position.h
option.o: option.c less.h /usr/include/sys/types.h /usr/include/sys/file.h
option.o: funcs.h
os.o: os.c /usr/include/stdio.h /usr/include/signal.h
os.o: /usr/include/machine/trap.h /usr/include/setjmp.h less.h
os.o: /usr/include/sys/types.h /usr/include/sys/file.h funcs.h
os.o: /usr/include/sys/types.h /usr/include/sys/stat.h
output.o: output.c less.h /usr/include/sys/types.h /usr/include/sys/file.h
output.o: funcs.h
position.o: position.c less.h /usr/include/sys/types.h /usr/include/sys/file.h
position.o: funcs.h position.h
prim.o: prim.c less.h /usr/include/sys/types.h /usr/include/sys/file.h funcs.h
prim.o: position.h
prompt.o: prompt.c less.h /usr/include/sys/types.h /usr/include/sys/file.h
prompt.o: funcs.h position.h
screen.o: screen.c less.h /usr/include/sys/types.h /usr/include/sys/file.h
screen.o: funcs.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
screen.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
screen.o: /usr/include/sys/ioctl.h
signal.o: signal.c less.h /usr/include/sys/types.h /usr/include/sys/file.h
signal.o: funcs.h /usr/include/signal.h /usr/include/machine/trap.h
tags.o: tags.c /usr/include/stdio.h less.h /usr/include/sys/types.h
tags.o: /usr/include/sys/file.h funcs.h
ttyin.o: ttyin.c less.h /usr/include/sys/types.h /usr/include/sys/file.h
ttyin.o: funcs.h
version.o: version.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
