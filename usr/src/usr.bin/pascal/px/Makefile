SCCSID = "@(#)Makefile 1.1 %G%"

CFLAGS = -O -DVAX -DOBJ
DESTDIR =/usr/ucb
LIBDIR =/usr/lib
PASCALDIR = /usr/src/cmd/pascal
LIBPCDIR = /usr/src/lib/libpc
CC = cc
AS = as
RM = rm -f

PXHDR =	${PASCALDIR}/OPnames.h ${PASCALDIR}/objfmt.h\
	${LIBPCDIR}/libpc.h ${LIBPCDIR}/h01errs.h\
	newvars.h machdep.h

PXSRC =	int.c interp.c except.c utilities.c

PXOBJ =	int.o interp.o except.o utilities.o

PXUTL =	${PASCALDIR}/opc.c ${PASCALDIR}/pic.c ${PASCALDIR}/version.c\
	make.ed1 panics interp.sed clean.sed

sources: ${PXHDR} ${PXSRC} ${PXUTL}
${PXHDR} ${PXUTL}:
	cp $@ .

px: Version.c ${PXOBJ}
	${CC} -O -o px Version.c ${PXOBJ} /usr/src/lib/libpc/pclib -lm
Version.c: ${PASCALDIR}/version.c
	${CC} -o version ${PASCALDIR}/version.c
	./version >Version.c
	${RM}  version
interp.o interp.s: interp.c
	${CC} -S interp.c
	sed -f interp.sed <interp.s >tmp
	mv tmp interp.s
	as -o interp.o interp.s
panics.h: panics make.ed1
	ex - <make.ed1
h02opcs.h: ${PASCALDIR}/OPnames.h ${PASCALDIR}/opc.c
	${CC} ${PASCALDIR}/opc.c -o opc
	./opc >h02opcs.h
	${RM} opc

install: px
	cp px ${DESTDIR}/px

clean:
	${RM}  *.o *.s px version.* opc* pic* OPnames.h objfmt.h libpc.h\
		panics.h h02opcs.h errs lpr core tmp

prt: interp.s
	sed -f clean.sed <interp.s

grind: sources
	@vpr READ_ME makefile
	@${CC} -o pic pic.c
	@./pic | vpr
	@/usr/ucb/vgrind newvars.h objfmt.h machdep.h
	@/usr/ucb/vgrind ${PXSRC}
	@${RM} pic*

depend:	sources
	/bin/grep '^#[ 	]*include' *.c \
		| sed '/<.*>/d' \
		| sed 's/:[^"]*"\([^"]*\)".*/: \1/' \
		| sed 's/\.c/.o/' >makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp makefile makefile.bak
	ed - makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> makefile
	echo '# see make depend above' >> makefile

# DO NOT DELETE THIS LINE -- make depend uses it
# DEPENDENCIES MUST END AT END OF FILE
except.o: panics.h
int.o: newvars.h
int.o: objfmt.h
interp.o: newvars.h
interp.o: panics.h
interp.o: h02opcs.h
interp.o: machdep.h
interp.o: libpc.h
opc.o: OPnames.h
panic.o: newvars.h
panic.o: panics.h
pic.o: OPnames.h
stats.o: newvars.h
utilities.o: newvars.h
utilities.o: panics.h
utilities.o: h02opcs.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
