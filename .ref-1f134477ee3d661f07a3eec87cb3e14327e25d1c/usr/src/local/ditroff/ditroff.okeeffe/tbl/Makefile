#	@(#)Makefile	1.2 (CWI) 85/10/02
#
DESTDIR=
BINDIR=/usr/local
CFLAGS = -O
HFILES = ext.h defs.h
SOURCES = checkuse.c delimfields.c getstop.c globals.c horlines.c \
	intersect.c io.c main.c maktab.c misc.c numfields.c options.c \
	over200.c rdata.c savrstor.c specs.c subr.c textblocks.c \
	vertlines.c write1line.c writecntl.c
OBJECTS = checkuse.o delimfields.o getstop.o globals.o horlines.o \
	intersect.o io.o main.o maktab.o misc.o numfields.o options.o \
	over200.o rdata.o savrstor.o specs.o subr.o textblocks.o \
	vertlines.o write1line.o writecntl.o
#
#
#
all: tbl

install: all
	install -s tbl ${DESTDIR}${BINDIR}/tbl

tbl: ${OBJECTS}
	cc ${CFLAGS} -o tbl ${OBJECTS}

checkuse.o delimfields.o getstop.o globals.o horlines.o:	ext.h defs.h
intersect.o io.o main.o maktab.o misc.o numfields.o options.o:	ext.h defs.h
over200.o rdata.o savrstor.o specs.o subr.o textblocks.o:	ext.h defs.h
vertlines.o write1line.o writecntl.o:				ext.h defs.h

lint:
	lint -ahbcp ${SOURCES}

print:	${HFILES} ${SOURCES}
	print ${HFILES} ${SOURCES}

clean:
	rm -f *.o tbl

tblfiles:
	echo ${SOURCES} ${HFILES} > tblfiles
