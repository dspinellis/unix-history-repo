#	Makefile	4.13	86/01/09
#
SRCS=	_setjmp.s abs.s alloca.s atof.s bcmp.s bcopy.s bzero.s disktab.c \
	ecvt.c fabs.s ffs.s frexp.s index.s insque.s ldexp.s modf.s nargs.s \
	remque.s rindex.s setjmp.s setjmperr.c strcat.s strcmp.s strcpy.s \
	strlen.s strncat.s strncmp.s strncpy.s udiv.s urem.s
OBJS=	_setjmp.o abs.o alloca.o atof.o bcmp.o bcopy.o bzero.o disktab.o \
	ecvt.o fabs.o ffs.o frexp.o index.o insque.o ldexp.o modf.o nargs.o \
	remque.o rindex.o setjmp.o setjmperr.o strcat.o strcmp.o strcpy.o \
	strlen.o strncat.o strncmp.o strncpy.o udiv.o urem.o
CFLAGS=	-O ${DEFS}
TAGSFILE=tags

.s.o:
	/lib/cpp -E ${DEFS} -DPROF $*.s | ${AS} -o $*.o
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	/lib/cpp -E ${DEFS} $*.s | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out $*.o

.c.o:
	cc -p -c ${CFLAGS} $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	cc -c ${CFLAGS} $*.c
	-ld -x -r $*.o
	mv a.out $*.o

genlib genlib_p: ${OBJS}
	@echo "building profiled genlib"
	@cd profiled; ar cru ../genlib_p ${OBJS}
	@echo "building normal genlib"
	@ar cru genlib ${OBJS}

tags:
	cwd=`pwd`; \
	for i in disktab.c ecvt.c; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f *.o profiled/*.o errs a.out core genlib genlib_p \
		tags Makefile.bak

depend:
	for i in ${SRCS}; do \
	    cc -M ${CFLAGS} $$i | awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

_setjmp.o: _setjmp.s ./DEFS.h
abs.o: abs.s ./DEFS.h
alloca.o: alloca.s ./DEFS.h
atof.o: atof.s ./DEFS.h
bcmp.o: bcmp.s ./DEFS.h
bcopy.o: bcopy.s ./DEFS.h
bzero.o: bzero.s ./DEFS.h
disktab.o: disktab.c /usr/include/disktab.h /usr/include/stdio.h
disktab.o: /usr/include/ctype.h
ecvt.o: ecvt.c
fabs.o: fabs.s ./DEFS.h
ffs.o: ffs.s ./DEFS.h
frexp.o: frexp.s ./DEFS.h
index.o: index.s ./DEFS.h
insque.o: insque.s ./DEFS.h
ldexp.o: ldexp.s ./DEFS.h /usr/include/errno.h
modf.o: modf.s ./DEFS.h
nargs.o: nargs.s ./DEFS.h
remque.o: remque.s ./DEFS.h
rindex.o: rindex.s ./DEFS.h
setjmp.o: setjmp.s ./DEFS.h /usr/include/vax/machparam.h
strcat.o: strcat.s ./DEFS.h
strcmp.o: strcmp.s ./DEFS.h
strcpy.o: strcpy.s ./DEFS.h
strlen.o: strlen.s ./DEFS.h
strncat.o: strncat.s ./DEFS.h
strncmp.o: strncmp.s ./DEFS.h
strncpy.o: strncpy.s ./DEFS.h
udiv.o: udiv.s ./DEFS.h ./DEFS.h
urem.o: urem.s ./DEFS.h ./DEFS.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
