#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)makefile	4.3 (Berkeley) %G%
#
DEBUG_FLAGS = -g

CFLAGS = -I obj ${DEBUG_FLAGS}

PRINT = lpr -p

KBD	= 3270pc.kbd
KBD	= unix.kbd

ALLC = apilib.c api_bsd.c api_exch.c asc_ebc.c astosc.c dctype.c \
		disp_asc.c ebc_disp.c

ALLH = apilib.h api_exch.h asc_ebc.h astosc.h disp_asc.h dctype.h ebc_disp.h


ALLHC=	${ALLH} ${ALLC}
ALLPRINT =	${ALLHC}

ALLSOURCE = ${ALLPRINT} makefile makefile.mak

ALLO = apilib.o api_bsd.o api_exch.o astosc.o asc_ebc.o dctype.o \
		disp_asc.o ebc_disp.o

#.c.obj:
#	${CC} ${CFLAGS} -c $<

apilib.a: ${ALLO}
	-rm $@
	${AR} ${ARFLAGS} $@ ${ALLO}
	${RANLIB} $@

.DEFAULT:
	sccs get $<

clean:
	-rm ${ALLO} errs apilib.a makefile.bak \
		disp_out asc_disp.out astosc.out disp_asc.out \
		astosc.OUT disp_asc.OUT \
		test* test.o t1* t1.o t2* t2.o

sccsclean:
	-sccs clean
	-sccs get makefile

action:
	${ACTION}

test:	apilib.a test.o
	${CC} ${CFLAGS} -o $@ test.o apilib.a

t1:	apilib.a t1.o
	${CC} ${CFLAGS} -o $@ t1.o apilib.a

t2:	apilib.a t2.o
	${CC} ${CFLAGS} -o $@ t2.o apilib.a

print:
	${PRINT} ${ALLPRINT}

clist:	${ALLC}
	@for i in ${ALLC} ; \
		do (echo ${DIRPATH}$$i); done

hclist:	${ALLHC}
	@for i in ${ALLHC} ; \
		do (echo ${DIRPATH}$$i); done

sourcelist:	${ALLSOURCE}
	@for i in ${ALLSOURCE}; \
		do (echo ${DIRPATH}$$i); done

astosc.OUT:	../ctlr/function.h ../ctlr/hostctlr.h ../ctlr/$(KBD)
	(cd ${.CURDIR}/../tools; make mkastosc )
	-rm $@ astosc.out
	../tools/mkastosc ${.CURDIR}/../ctlr/hostctlr.h \
		${.CURDIR}/../ctlr/function.h < \
				${.CURDIR}/../ctlr/$(KBD) > $@
	ln $@ astosc.out

asc_disp.out:	ebc_disp.o
	(cd ${.CURDIR}/../tools; make mkastods )
	-rm $@
	../tools/mkastods > $@

disp_asc.OUT:	ebc_disp.o
	(cd ${.CURDIR}/../tools; make mkdstoas )
	-rm $@ disp_asc.out
	../tools/mkdstoas > $@
	ln $@ disp_asc.out

depend:
	grep '^#include' ${ALLC} | grep -v '<' | \
	sed -e 's/:[^"]*"\([^"]*\)".*/: \1/' \
	    -e 's/\.c/$.o/' | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '$$r makedep' >>eddep
	echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	-rm -f makefile.bak
	cp makefile makefile.bak
	ed - makefile < eddep
	-rm eddep makedep

# DO NOT DELETE THIS LINE

apilib.o: ../ctlr/api.h apilib.h
api_bsd.o: ../ctlr/api.h api_exch.h
api_exch.o: ../general/general.h api_exch.h
asc_ebc.o: asc_ebc.h
astosc.o: ../general/general.h ../ctlr/function.h astosc.h astosc.OUT
dctype.o: dctype.h
disp_asc.o: disp_asc.h asc_disp.out disp_asc.OUT
