#
# Copyright (c) 1980, 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.2 (Berkeley) %G%
#
ALL=	hkboot hpboot htboot mtboot noboot raboot rlboot \
	upboot utboot tmboot tsboot tuboot \
	httoggle mttoggle tmtoggle tstoggle uttoggle

CFLAGS= -DLOCORE
INCLUDE= /usr/include

all: ${ALL}

hkboot: hkboot.c ${INCLUDE}/sys/disklabel.h
	cc -E ${CFLAGS} hkboot.c | as
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=b.out
	dd if=b.out of=hkboot conv=sync
	rm b.out

hpboot: hpboot.c ${INCLUDE}/sys/disklabel.h
	cc -E ${CFLAGS} hpboot.c | as
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=b.out
	dd if=b.out of=hpboot conv=sync
	rm b.out

htboot: htboot.c
	cc -E ${CFLAGS} htboot.c | as
	nm -u a.out
	strip a.out
	dd if=a.out of=b.out bs=32 skip=1
	dd if=b.out of=htboot conv=sync
	rm b.out

mtboot: mtboot.c
	cc -E ${CFLAGS} mtboot.c | as
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=b.out
	dd if=b.out of=mtboot conv=sync
	rm b.out

raboot: raboot.c ${INCLUDE}/sys/disklabel.h
	cc -E ${CFLAGS} raboot.c | as
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=b.out
	dd if=b.out of=raboot conv=sync
	rm b.out

rlboot: rlboot.c ${INCLUDE}/sys/disklabel.h
	cc -E ${CFLAGS} rlboot.c | as
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=b.out
	dd if=b.out of=rlboot conv=sync
	rm b.out

tmboot: tmboot.c
	cc -E ${CFLAGS} tmboot.c | as
	nm -u a.out
	strip a.out
	dd if=a.out of=b.out bs=32 skip=1
	dd if=b.out of=tmboot conv=sync
	rm b.out

tsboot: tsboot.c
	cc -E ${CFLAGS} tsboot.c | as
	nm -u a.out
	strip a.out
	dd if=a.out of=b.out bs=32 skip=1
	dd if=b.out of=tsboot conv=sync
	rm b.out

upboot: upboot.c ${INCLUDE}/sys/disklabel.h
	cc -E ${CFLAGS} upboot.c | as
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=b.out
	dd if=b.out of=upboot conv=sync
	rm b.out

tuboot: tuboot.c
	cc -E ${CFLAGS} tuboot.c | as
	nm -u a.out
	strip a.out
	dd if=a.out of=b.out bs=32 skip=1
	dd if=b.out of=tuboot conv=sync
	rm b.out

utboot: utboot.c
	cc -E ${CFLAGS} utboot.c | as
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=b.out
	dd if=b.out of=utboot conv=sync
	rm b.out

httoggle: httoggle.s
	as httoggle.s
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=httoggle

mttoggle: mttoggle.s
	as mttoggle.s
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=mttoggle

tmtoggle: tmtoggle.s
	as tmtoggle.s
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=tmtoggle

tstoggle: tstoggle.s
	as tstoggle.s
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=tstoggle

uttoggle: uttoggle.s
	as uttoggle.s
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=uttoggle

noboot:
	echo | dd of=noboot conv=sync

clean:
	rm -f a.out b.out ${ALL}

install:
	cp *boot ${DESTDIR}/usr/mdec
	rm -f ${DESTDIR}/usr/mdec/mboot
	ln ${DESTDIR}/usr/mdec/htboot ${DESTDIR}/usr/mdec/mboot
	rm -f ${DESTDIR}/usr/mdec/uboot
	ln ${DESTDIR}/usr/mdec/hpboot ${DESTDIR}/usr/mdec/uboot
