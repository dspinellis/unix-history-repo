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
PRINT =	lpr -p
ALLC =	
ALLH =	telnet.h
ALLPRINT =	${ALLH} ${ALLC}
ALLSOURCE =	${ALLPRINT} makefile makefile.mak


#targets...

clean:	 
	

sccsclean:	 
	-sccs clean
	-sccs get makefile

action:	 
	${ACTION}

print:	 
	${PRINT} ${ALLPRINT}

sourcelist:	${ALLSOURCE} 
	echo ${DIRPATH}telnet.h
	cd ${CWD}
	echo ${DIRPATH}
	cd ${CWD}
	echo ${DIRPATH}makefile
	cd ${CWD}
	echo ${DIRPATH}makefile.mak
	cd ${CWD}
	
	

.DEFAULT:	 
	sccs get $<
