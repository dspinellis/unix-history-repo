#	makefile	1.5	83/12/13
#
CFLAGS=	-O -w -DVMUNIX
LFLAGS=
LIBS=	-lm -lc
DESTDIR=
LIBDIR=	/usr/new/lib/apl
LD=	ld
ALL=	xed apl aplcvt cata

OBJ1 = a0.o a1.o a3.o a4.o a5.o a6.o a7.o a8.o a9.o 
OBJ2 = aa.o ab.o ac.o ad.o ae.o af.o ag.o ah.o ai.o aj.o 
OBJ3 = ak.o al.o an.o ao.o aq.o at.o aw.o ax.o ay.o az.o
OBJ4 = y.tab.o a2.o am.o
XEDOBJ = xed.o
CVTOBJ = aplcvt.o
CATOBJ = cata.o

all:	${ALL}

apl:	$(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) gamma.o
	$(LD) $(LFLAGS) /lib/crt0.o -o apl \
		$(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) gamma.o $(LIBS)


$(OBJ1) $(OBJ2) $(OBJ3): apl.h

a0.o:	a0.c
	${CC} -c ${CFLAGS} -DLIBDIR=\"${LIBDIR}\" a0.c

a2.o:	a2.c apl.h aplmap.h

am.o:	am.c apl.h

y.tab.o: y.tab.c lex.c tab.c apl.h

y.tab.c: apl.y
	$(YACC) apl.y

rebuild:
	$(YACC) apl.y
	$(CC) $(CFLAGS) -c a?.c y.tab.c gamma.c
	$(LD) $(LFLAGS) /lib/crt0.o -o apl *.o $(LIBS)
	: APL rebuild complete

xed:
	cc ${CFLAGS} -o xed xed.c

aplcvt:
	cc ${CFLAGS} -o aplcvt aplcvt.c

cata:
	cc ${CFLAGS} -o cata cata.c

install: apl aplcvt xed cata
	install -s xed ${DESTDIR}/usr/new
	install -s aplcvt ${DESTDIR}/usr/new
	install -s cata ${DESTDIR}/usr/new
	install -s apl ${DESTDIR}/usr/new

clean:
	rm -f $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) $(XEDOBJ) $(CVTOBJ) $(CATOBJ)
	rm -f xed apl aplcvt cata gamma.o y.tab.c

apl2:	FRC
	rm -f *.o y.tab.c
	$(YACC) apl.y
	$(CC) $(CFLAGS) -DAPL2 -c a?.c y.tab.c gamma.c
	$(LD) $(LFLAGS) /lib/crt0.o -o apl2 *.o $(LIBS)
	rm -f *.o y.tab.c
	: APL2 build complete

FRC:
