# variables...
O =	.obj
X =	.exe
L =	-link
CC =	cl
MV =	rename
RM =	erase
LINT_ARGS =	-DLINT_ARGS
DEBUG_FLAGS =	-Zi -Od
AR =	lib
AR1 =	
AR2 =	+
AR3 =	";"
RANLIB =	echo "Done with "
DEFINES =	${LINT_ARGS}
CFLAGS =	${DEBUG_FLAGS} -I..
PRINT =	lpr -p
ALLC =	tnrecv.c
ALLH =	tncomp.h
ALLPRINT =	${ALLH} ${ALLC}
ALLSOURCE =	${ALLPRINT} makefile makefile.mak
ALLO =	tnrecv${O}


#targets...

.c.obj:	 
	${CC} ${CFLAGS} -c $<

all:	tnrecv${X} 
	

tnrecv${X}:	tnrecv${O} 
	${CC} ${CFLAGS} -o $@ tnrecv${O} ${L} ..\api\api.lib

clean:	 
	${RM} makefile.bak
	cd ${CWD}
	${RM} tnrecv.obj
	cd ${CWD}
	${RM} errs
	cd ${CWD}
	${RM} tnrecv.exe
	cd ${CWD}
	
	

.DEFAULT:	 
	sccs get $<

sccsclean:	 
	-sccs clean
	-sccs get makefile

action:	 
	${ACTION}

print:	 
	${PRINT} ${ALLPRINT}

sourcelist:	${ALLSOURCE} tarread.exe 
	echo ${DIRPATH}tncomp.h
	cd ${CWD}
	echo ${DIRPATH}tnrecv.c
	cd ${CWD}
	echo ${DIRPATH}makefile
	cd ${CWD}
	echo ${DIRPATH}makefile.mak
	cd ${CWD}
	echo ${DIRPATH}tarread.exe
	cd ${CWD}
	
	

depend:	 
	grep '^#include' ${ALLC} | grep -v '<' | sed -e 's\:[^"]*"\([^"]*\)".*\: \1\' -e 's\\.c\$$O\' | awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '$$r makedep' >>eddep
	echo '\^# DO NOT DELETE THIS LINE\+1,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	-rm -f makefile.bak
	cp makefile makefile.bak
	ed - makefile < eddep
	rm eddep makedep
