SCCSID = "@(#)makefile 4.1 10/10/80"

CFLAGS = -O -d2 -DVAX -DOBJ
INSTALLDIR =$(DESTDIR)/usr/ucb
CC = cc
AS = as
RM = rm -f

PXRUN =	int.c perror.c stats.c utilities.c

PXHDR =	00case.h OPnames.h h00vars.h objfmt.h

PXINT =	01int.s 02relset.s 03rel.s 04as.s 05index.s 06add.s 07sub.s \
	08call.s 10mul.s 12div.s 13mod.s 14neg.s 15bool.s 16dvd.s \
	17ind.s 18rv.s 20con.s 21rang.s 23case.s 24pxp.s 25set.s \
	26for.s 27conv.s 28fun.s 30read.s 31write.s 32iostat.s \
	33iofile.s 35iosubs.s

PXUTL =	malloc.c opc.c opoff.c pic.c version.c \
	make.ed1 make.ed2 errdata

px: Version.c int.o perror.o stats.o utilities.o malloc.o px.o
	${CC} -O -o px Version.c int.o px.o utilities.o \
		perror.o stats.o malloc.o -lm

Version.c: version.c
	${CC} -o version version.c
	./version >Version.c
	${RM}  version
int.o: h00vars.h objfmt.h
perror.o stats.o: h00vars.h h01errs.h
utilities.o: h00vars.h h01errs.h h02opcs.h
malloc.o: h01errs.h
	${CC} -O -c -d2 -Ddebug malloc.c
px.o: 00case.s 34err.s ${PXINT}
	cat 00case.s 34err.s ${PXINT} | ${AS} -d2 -o px.o
00case.s: OPnames.h 00case.h opoff.c
	${CC} opoff.c -o opoff
	./opoff >optab
	cat 00case.h optab >00case.s
	${RM} opoff optab
h01errs.h 34err.s: errdata make.ed1
	ed - <make.ed1
	${RM}  E.s
h02opcs.h: OPnames.h opc.c
	${CC} opc.c -o opc
	./opc >h02opcs.h
	${RM} opc

install: px
	cp px ${INSTALLDIR}/px

clean:
	${RM}  *.o px version 00case.s optab opoff opc pic errs h02opcs.h

print: 00case.s 34err.s OPnames.h make.ed2
	@pr READ_ME makefile
	@ls -l | pr
	@${CC} -o pic pic.c
	@pic | pr
	@pr h00vars.h Version.c int.c
	@cat [0-3][0-9]*.s >tmp
	@ed - tmp <make.ed2
	@cat -n <tmp | pr
	@pr perror.c stats.c utilities.c
	@${RM}  tmp pic*
