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
#	@(#)makefile	4.1 (Berkeley) %G%
# msdos versus unix defines
O	= .o
#PC_O	= .obj

X	=
#PC_X	= .exe

L	=
#PC_L	= -link

CC	= cc
#PC_CC	= cl

MV	= mv
#PC_MV	= rename

RM	= rm -f
#PC_RM= erase

LINT_ARGS =
#PC_LINT_ARGS = -DLINT_ARGS

DEBUG_FLAGS = -g
#PC_DEBUG_FLAGS = -Zi -Od

AR	= ar
AR1	= cr
AR2	=
AR3	=
#PC_AR	= lib
#PC_AR1	=
#PC_AR2	= +
#PC_AR3	= ";"

RANLIB	= ranlib
#PC_RANLIB = echo "Done with "

CFLAGS = ${DEBUG_FLAGS}

PRINT = lpr -p

KBD	= 3270pc.kbd
KBD	= unix.kbd

ALLC = apilib.c api_bsd.c api_exch.c asc_ebc.c astosc.c dctype.c \
		disp_asc.c ebc_disp.c

ALLH = apilib.h api_exch.h asc_ebc.h astosc.h disp_asc.h dctype.h ebc_disp.h


ALLHC=	${ALLH} ${ALLC}
ALLPRINT =	${ALLHC}

ALLSOURCE = ${ALLPRINT} makefile makefile.mak

ALLO = apilib$O api_bsd$O api_exch$O astosc$O asc_ebc$O dctype$O \
		disp_asc$O ebc_disp$O

.c.obj:
	${CC} ${CFLAGS} -c $<

apilib.a: ${ALLO}
	${RM} $@
	for i in ${ALLO}; do (${AR} ${AR1} $@ ${AR2} $$i${AR3}); done
	${RANLIB} $@

.DEFAULT:
	sccs get $<

clean:
	for i in ${ALLO} errs apilib.a makefile.bak \
		disp_out asc_disp.out astosc.out disp_asc.out \
		test* test$O t1* t1$O t2* t2$O; \
			do (${RM} $$i); done

sccsclean:
	-sccs clean
	-sccs get makefile

action:
	${ACTION}

test:	apilib.a test$O
	${CC} ${CFLAGS} -o $@ test$O apilib.a

t1:	apilib.a t1$O
	${CC} ${CFLAGS} -o $@ t1$O apilib.a

t2:	apilib.a t2$O
	${CC} ${CFLAGS} -o $@ t2$O apilib.a

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

astosc.out:	../ctlr/function.h ../ctlr/hostctlr.h ../ctlr/$(KBD)
	(cd ../tools; make mkastosc$X )
	${RM} $@
	../tools/mkastosc < ../ctlr/$(KBD) > $@

asc_disp.out:	ebc_disp$O
	(cd ../tools; make mkastods$X )
	${RM} $@
	../tools/mkastods > $@

disp_asc.out:	ebc_disp$O
	(cd ../tools; make mkdstoas$X )
	${RM} $@
	../tools/mkdstoas > $@

depend:
	grep '^#include' ${ALLC} | grep -v '<' | \
	sed -e 's/:[^"]*"\([^"]*\)".*/: \1/' \
	    -e 's/\.c/$$O/' | \
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
	rm eddep makedep

# DO NOT DELETE THIS LINE

apilib$O: ../ctlr/api.h apilib.h
api_bsd$O: ../ctlr/api.h api_exch.h
api_exch$O: ../general/general.h api_exch.h
asc_ebc$O: asc_ebc.h
astosc$O: ../general/general.h ../ctlr/function.h astosc.h astosc.out
dctype$O: dctype.h
disp_asc$O: disp_asc.h asc_disp.out disp_asc.out
