#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.4	(Berkeley)	%G%
#
COMMON=	../common_source
CFLAGS=	-O -DNROFF -DVMUNIX -I${COMMON}
VPATH=	${COMMON}
LIBC=	/lib/libc.a
SRCS=	ni.c nii.c n1.c n2.c n3.c n4.c n5.c n6.c n7.c n8.c n9.c n10.c \
	hytab.c ntab.c suftab.c
OBJS=	ni.o nii.o n1.o n2.o n3.o n4.o n5.o n6.o n7.o n8.o n9.o n10.o \
	hytab.o ntab.o suftab.o

all: nroff

nroff: ${OBJS} ${LIBC}
	$(CC) -o $@ ${OBJS}

hytab.o: ${COMMON}/hytab.c
	${CC} -R -c ${COMMON}/hytab.c

suftab.o: ${COMMON}/suftab.c
	${CC} -R -c ${COMMON}/suftab.c

ntab.o: ntab.c
	${CC} -R -c ntab.c

clean: FRC
	rm -f ${OBJS} core nroff

depend: ${SRCS} FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 nroff ${DESTDIR}/usr/bin/nroff

lint: ${SRCS} FRC
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS} FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

ni.o: ../common_source/ni.c ../common_source/tdef.h /usr/include/sys/param.h
ni.o: /usr/include/sys/types.h /usr/include/signal.h
ni.o: /usr/include/machine/machparam.h /usr/include/signal.h
nii.o: ../common_source/nii.c ../common_source/tdef.h /usr/include/sys/param.h
nii.o: /usr/include/sys/types.h /usr/include/signal.h
nii.o: /usr/include/machine/machparam.h /usr/include/signal.h
nii.o: ../common_source/tw.h ../common_source/sdef.h ../common_source/d.h
nii.o: ../common_source/v.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
nii.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
n1.o: ../common_source/n1.c ../common_source/tdef.h /usr/include/sys/param.h
n1.o: /usr/include/sys/types.h /usr/include/signal.h
n1.o: /usr/include/machine/machparam.h /usr/include/signal.h
n1.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/time.h
n1.o: ../common_source/d.h ../common_source/v.h ../common_source/tw.h
n1.o: ../common_source/sdef.h /usr/include/setjmp.h /usr/include/sgtty.h
n1.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
n1.o: /usr/include/sys/ttydev.h
n2.o: ../common_source/n2.c ../common_source/tdef.h /usr/include/sys/param.h
n2.o: /usr/include/sys/types.h /usr/include/signal.h
n2.o: /usr/include/machine/machparam.h /usr/include/signal.h
n2.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
n2.o: /usr/include/sys/ttydev.h ../common_source/d.h ../common_source/v.h
n2.o: ../common_source/tw.h ../common_source/sdef.h /usr/include/setjmp.h
n3.o: ../common_source/n3.c ../common_source/tdef.h /usr/include/sys/param.h
n3.o: /usr/include/sys/types.h /usr/include/signal.h
n3.o: /usr/include/machine/machparam.h /usr/include/signal.h
n3.o: ../common_source/d.h ../common_source/v.h ../common_source/tw.h
n3.o: ../common_source/sdef.h
n4.o: ../common_source/n4.c ../common_source/tdef.h /usr/include/sys/param.h
n4.o: /usr/include/sys/types.h /usr/include/signal.h
n4.o: /usr/include/machine/machparam.h /usr/include/signal.h
n4.o: ../common_source/d.h ../common_source/v.h ../common_source/tw.h
n4.o: ../common_source/sdef.h
n5.o: ../common_source/n5.c ../common_source/tdef.h /usr/include/sys/param.h
n5.o: /usr/include/sys/types.h /usr/include/signal.h
n5.o: /usr/include/machine/machparam.h /usr/include/signal.h
n5.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
n5.o: /usr/include/sys/ttydev.h ../common_source/d.h ../common_source/v.h
n5.o: ../common_source/sdef.h
n6.o: n6.c ../common_source/tdef.h /usr/include/sys/param.h
n6.o: /usr/include/sys/types.h /usr/include/signal.h
n6.o: /usr/include/machine/machparam.h /usr/include/signal.h
n6.o: ../common_source/d.h ../common_source/v.h ../common_source/tw.h
n7.o: ../common_source/n7.c ../common_source/tdef.h /usr/include/sys/param.h
n7.o: /usr/include/sys/types.h /usr/include/signal.h
n7.o: /usr/include/machine/machparam.h /usr/include/signal.h
n7.o: ../common_source/d.h ../common_source/v.h ../common_source/tw.h
n7.o: ../common_source/sdef.h
n8.o: ../common_source/n8.c ../common_source/tdef.h /usr/include/sys/param.h
n8.o: /usr/include/sys/types.h /usr/include/signal.h
n8.o: /usr/include/machine/machparam.h /usr/include/signal.h
n9.o: ../common_source/n9.c ../common_source/tdef.h /usr/include/sys/param.h
n9.o: /usr/include/sys/types.h /usr/include/signal.h
n9.o: /usr/include/machine/machparam.h /usr/include/signal.h
n9.o: ../common_source/d.h ../common_source/v.h ../common_source/tw.h
n10.o: n10.c ../common_source/tdef.h /usr/include/sys/param.h
n10.o: /usr/include/sys/types.h /usr/include/signal.h
n10.o: /usr/include/machine/machparam.h /usr/include/signal.h
n10.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
n10.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
n10.o: ../common_source/d.h ../common_source/v.h ../common_source/tw.h
hytab.o: ../common_source/hytab.c
ntab.o: ntab.c
suftab.o: ../common_source/suftab.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
