#-
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# William Jolitz.
#
#
# Copying or redistribution in any form is explicitly forbidden
# unless prior written permission is obtained from William Jolitz or an
# authorized representative of the University of California, Berkeley.
#
# Freely redistributable copies of this code will be available in
# the near future; for more information contact William Jolitz or
# the Computer Systems Research Group at the University of California,
# Berkeley.
#
# The name of the University may not be used to endorse or promote
# products derived from this software without specific prior written
# permission.  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE.
#	@(#)Makefile	5.2 (Berkeley) %G%
AS=as
#
#	crt0	Normal C run time startoff
#	mcrt0	C run time start off for profiling, ``prof'' conventions
#	gcrt0	C run time start off for profiling, ``gprof'' conventions
#
DESTDIR=
SRCS=	crt0.c # mon.c gmon.c
OBJS=	crt0.o # mcrt0.o gcrt0.o mon.o gmon.o
TAGSFILE=tags

all: ${OBJS}

install: ${OBJS}
	mv crt0.o ${DESTDIR}/lib/crt0.o
	# mv mcrt0.o ${DESTDIR}/lib/mcrt0.o
	# mv gcrt0.o ${DESTDIR}/usr/lib/gcrt0.o

crt0.o:	crt0.c
	${CC} -S ${DFLAGS} -DCRT0 crt0.c
	grep -v "^#[ _]" <crt0.s >x.s 
	ed x.s <:fix
	mv x.s crt0.s
	/lib/cpp -traditional crt0.s > x.s
	${AS} -o x.o x.s
	ld -x -r -o crt0.o x.o
	rm -f x.s x.o crt0.s

moncrt0.o: crt0.c
	${CC} -S ${DFLAGS} -DMCRT0 crt0.c
	grep -v "^#[ _]" <crt0.s >x.s 
	mv x.s crt0.s
	/lib/cpp -traditional crt0.s > x.s
	${AS} -o x.o x.s
	ld -x -r -o moncrt0.o x.o
	rm -f x.s x.o crt0.s

gcrt0.o: moncrt0.o gmon.o
	ld -x -r -o gcrt0.o moncrt0.o gmon.o

mcrt0.o: moncrt0.o mon.o
	ld -x -r -o mcrt0.o moncrt0.o mon.o

mon.o: mon.c mon.ex
	${CC} -S ${DEFS} ${DFLAGS} mon.c
	ex - mon.s < mon.ex
	${AS} -o x.o mon.s
	ld -x -r -o mon.o x.o
	rm -f x.o mon.s

gmon.o: gmon.c gmon.h gmon.ex
	${CC} -S ${DEFS} ${DFLAGS} gmon.c
	ex - gmon.s < gmon.ex
	${AS} -o x.o gmon.s
	ld -x -r -o gmon.o x.o
	rm -f x.o gmon.s

tags:	
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f ${OBJS} *.o *.s core errs tags Makefile.bak

lint:	crt0.c
	lint crt0.c -DMCRT0
	lint crt0.c -DGCRT0
	lint crt0.c -DCRT0

depend:
	for i in ${SRCS}; do \
	    ${CC} -M ${DEFS} ${DFLAGS} $$i | awk ' { if ($$1 != prev) \
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

crt0.o: crt0.c
mon.o: mon.c
gmon.o: gmon.c ./gmon.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
