#	@(#)Makefile	4.3	%G%
#
DIFF=	/bin/diff
DIFFH=	/usr/lib/diffh
PR=	/bin/pr
CFLAGS=	-O -DDIFF='"${DIFF}"' -DDIFFH='"${DIFFH}"' -DPR='"${PR}"'
DESTDIR=

SRCS=	diff.c diffdir.c diffreg.c
HDRS=	diff.h
OBJS=	diff.o diffdir.o diffreg.o

all: diff diffh

diff: ${OBJS}
	cc ${CFLAGS} -o diff ${OBJS} ${LDFLAGS}

diffh: diffh.o
	cc -o diffh diffh.o ${LDFLAGS}

depend:

clean:
	rm -f diff diffh ${OBJS} diffh.o

install:
	install -s diff ${DESTDIR}/bin/diff
	install -s diffh ${DESTDIR}/usr/lib/diffh
