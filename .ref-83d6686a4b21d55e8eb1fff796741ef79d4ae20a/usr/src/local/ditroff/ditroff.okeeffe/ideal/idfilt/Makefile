#
#	Makefile	(CWI)	1.2	87/04/01
#
# BINDIR is the place where ideal will be installed
BINDIR = /usr/local
# DESTDIR is the destination where we want to move the thing (temporarily)
DESTDIR =
# LIBDIR is the place where we expect the the things we need and is
# is therefore hardwired in. Since it is all local, we use BINDIR as well
LIBDIR = ${BINDIR}/lib/ideal/lib
# pfilt is ideal filter for plot(1)
# tfilt is filter for ditroff
# idsort is general sort for ideal
OBJECTS = tfilt pfilt idsort vfilt 4filt

all:	${OBJECTS}

idfilt.o	:idfilt.c idfilt.h

4014.o	:4014.c idfilt.h

4filt	:idfilt.o 4014.o
	cc -o 4filt 4014.o idfilt.o -l4014 -lm

pout.o	:pout.c

pfilt	:idfilt.o pout.o
	cc -o pfilt pout.o idfilt.o -lplot -lm

202.o	:202.c idfilt.h

202filt	:idfilt.o 202.o
	cc -o tfilt 202.o idfilt.o

har.o	:har.c idfilt.h

ver.o	:ver.c idfilt.h

vfilt	:idfilt.o ver.o
	cc -o vfilt ver.o idfilt.o

tfilt	:idfilt.o har.o
	cc -o tfilt har.o idfilt.o

aps.o	:aps.c

apsfilt	:aps.o idfilt.o
	cc -o apsfilt aps.o idfilt.o

idsort	:idsort.c
	cc -o idsort idsort.c

install: all
	install -s tfilt ${DESTDIR}${LIBDIR}/tfilt
	install -s 4filt ${DESTDIR}${LIBDIR}/4filt
	install -s pfilt ${DESTDIR}${LIBDIR}/pfilt
	install -s vfilt ${DESTDIR}${LIBDIR}/vfilt
	install -s idsort ${DESTDIR}${LIBDIR}/idsort

clean:
	rm -f *.o ${OBJECTS}
