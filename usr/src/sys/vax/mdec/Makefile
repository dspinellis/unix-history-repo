#
# Copyright (c) 1980, 1986, 1987 Regents of the University of California.
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
#	@(#)Makefile	7.6 (Berkeley) %G%
#

CFLAGS=	-DLOCORE

# source files that live in the current directory
SRCS=	hkboot.c hpboot.c htboot.c mtboot.c raboot.c rdboot.c rlboot.c \
	upboot.c utboot.c tmboot.c tsboot.c tuboot.c httoggle.s mttoggle.s \
	tmtoggle.s tstoggle.s uttoggle.s

# boot blocks that are preprocessed, then assembled
BOOTS=	hkboot hpboot htboot mtboot raboot rdboot rlboot upboot utboot \
	tmboot tmscpboot tsboot tuboot

# toggle files that are assembled, but not installed
TOGGLES=httoggle mttoggle tmtoggle tstoggle uttoggle

all: ${BOOTS} ${TOGGLES} noboot

${BOOTS}:
	${CC} -E ${CFLAGS} $@.c | as
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=b.out
	dd if=b.out of=$@ conv=sync
	rm b.out

${TOGGLES}:
	as $@.s
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=$@

noboot:
	echo | dd of=noboot conv=sync

clean:
	rm -f a.out b.out ${BOOTS} ${TOGGLES} noboot

install:
	install -o bin -g bin -m 444 ${BOOTS} ${DESTDIR}/usr/mdec
	rm -f ${DESTDIR}/usr/mdec/mboot ${DESTDIR}/usr/mdec/uboot
	ln ${DESTDIR}/usr/mdec/htboot ${DESTDIR}/usr/mdec/mboot
	ln ${DESTDIR}/usr/mdec/hpboot ${DESTDIR}/usr/mdec/uboot

depend:
	mkdep -p ${CFLAGS} ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

hkboot: hkboot.c /usr/include/sys/disklabel.h
hpboot: hpboot.c /usr/include/sys/disklabel.h
htboot: htboot.c
mtboot: mtboot.c
raboot: raboot.c /usr/include/sys/disklabel.h
rlboot: rlboot.c /usr/include/sys/disklabel.h
upboot: upboot.c /usr/include/sys/disklabel.h
utboot: utboot.c
tmboot: tmboot.c
tsboot: tsboot.c
tuboot: tuboot.c
httoggle: httoggle.s
mttoggle: mttoggle.s
tmtoggle: tmtoggle.s
tstoggle: tstoggle.s
uttoggle: uttoggle.s

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
