#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5 (Berkeley) %G%
#
CFLAGS=	-O ${DEFS}
STDSRCS=ftime.c gtty.c nice.c pause.c rand.c stty.c tell.c \
	times.c utime.c vlimit.c vtimes.c
STD=	ftime.o gtty.o nice.o pause.o rand.o stty.o tell.o \
	times.o utime.o vlimit.o vtimes.o
# these should just be pitched, but in the interest of compatibility...
TRASHSRC=getpw.c
TRASH=	getpw.o 
SRCS=	${STDSRCS} ${TRASHSRCS}
OBJS=	${STD} ${TRASH}
TAGSFILE=tags

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

compat-4.1lib compat-4.1lib_p: ${OBJS}
	@echo "building profiled compat-4.1lib"
	@cd profiled; ar cru ../compat-4.1lib_p ${OBJS}
	@echo "buiding normal compat-4.1lib"
	@ar cru compat-4.1lib ${OBJS}

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f compat-4.1lib compat-4.1lib_p *.o profiled/*.o tags

depend:
	mkdep ${CFLAGS} ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

ftime.o: ftime.c /usr/include/sys/types.h /usr/include/sys/time.h
ftime.o: /usr/include/time.h
gtty.o: gtty.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
gtty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
nice.o: nice.c /usr/include/sys/time.h /usr/include/time.h
nice.o: /usr/include/sys/resource.h
pause.o: pause.c
rand.o: rand.c
stty.o: stty.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
stty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tell.o: tell.c
times.o: times.c /usr/include/sys/time.h /usr/include/time.h
times.o: /usr/include/sys/resource.h
utime.o: utime.c /usr/include/sys/time.h /usr/include/time.h
vlimit.o: vlimit.c /usr/include/sys/time.h /usr/include/time.h
vlimit.o: /usr/include/sys/resource.h /usr/include/errno.h
vtimes.o: vtimes.c /usr/include/sys/time.h /usr/include/time.h
vtimes.o: /usr/include/sys/resource.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
