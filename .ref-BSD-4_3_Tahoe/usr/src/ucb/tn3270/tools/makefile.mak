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
PRINT =	print
DEFINES =	${LINT_ARGS}
INCLUDES =	-I.
OPTIMIZE =	${DEBUG_FLAGS}
CFLAGS =	${OPTIMIZE} ${INCLUDES} ${DEFINES}
LINTFLAGS =	-hbxaz
ALLH =	dohits.h ectype.h
ALLY =	mkmake.y
ALLC =	dohits.c ectype.c mkastods.c mkastosc.c mkdctype.c mkdstoas.c mkhits.c prt3270.c
ALLO =	prt3270${O} mkdctype${O} ectype${O} mkastods${O} mkdstoas${O} mkhits${O} mkmake${O} dohits${O} mkastosc${O}
ALLPRINT =	${ALLH} ${ALLC} ${ALLY}
ALLSOURCE =	${ALLPRINT} makefile makefile.mak
MKMAKE =	
ALLEXE =	mkastods${X} mkastosc${X} mkdctype${X} mkdstoas${X} mkhits${X} ${MKMAKE} prt3270${X}


#targets...

.c.obj:	 
	${CC} ${CFLAGS} -c $<

tools:	 
	@echo Need to specify WHICH tool ...

all:	${ALLEXE} 
	@echo done.

prt3270${X}:	prt3270${O} ..\general\globals${O} ..\api\asc_ebc${O} ..\api\astosc${O} ..\ctlr\kbd.out 
	${CC} ${CFLAGS} -o prt3270 prt3270${O} ..\general\globals${O} ..\api\asc_ebc${O} ..\api\astosc${O}

mkastosc${X}:	mkastosc${O} dohits${O} ..\api\asc_ebc${O} ..\api\ebc_disp${O} 
	${CC} ${CFLAGS} -o mkastosc mkastosc${O} dohits${O} ..\api\asc_ebc${O} ..\api\ebc_disp${O}

mkastods${X}:	mkastods${O} ..\api\asc_ebc${O} ..\api\ebc_disp${O} 
	${CC} ${CFLAGS} -o mkastods mkastods${O} ..\api\asc_ebc${O} ..\api\ebc_disp${O}

mkdstoas${X}:	mkdstoas${O} ..\api\asc_ebc${O} ..\api\ebc_disp${O} 
	${CC} ${CFLAGS} -o mkdstoas mkdstoas${O} ..\api\asc_ebc${O} ..\api\ebc_disp${O}

mkhits${X}:	mkhits${O} dohits${O} ..\api\asc_ebc${O} ..\api\ebc_disp${O} 
	${CC} ${CFLAGS} -o mkhits mkhits${O} dohits${O} ..\api\asc_ebc${O} ..\api\ebc_disp${O}

mkmake:	mkmake${O} 
	${CC} ${CFLAGS} -o mkmake mkmake${O}

mkdctype${X}:	mkdctype${O} ..\api\ebc_disp${O} ectype${O} 
	${CC} ${CFLAGS} -o mkdctype mkdctype${O} ..\api\ebc_disp${O} ectype${O}

..\api\astosc${O}:	 
	cd ..\api
	 make astosc${O}
	cd ${CWD}
	

..\api\asc_ebc${O}:	 
	cd ..\api
	 make asc_ebc${O}
	cd ${CWD}
	

..\ctlr\kbd.out:	 
	cd ..\ctlr
	 make kbd.out
	cd ${CWD}
	

..\api\ebc_disp${O}:	 
	cd ..\api
	 make ebc_disp${O}
	cd ${CWD}
	

..\general\globals${O}:	 
	cd ..\general
	 make globals${O}
	cd ${CWD}
	

sourcelist:	${ALLSOURCE} 
	echo ${DIRPATH}dohits.h
	cd ${CWD}
	echo ${DIRPATH}ectype.h
	cd ${CWD}
	echo ${DIRPATH}dohits.c
	cd ${CWD}
	echo ${DIRPATH}ectype.c
	cd ${CWD}
	echo ${DIRPATH}mkastods.c
	cd ${CWD}
	echo ${DIRPATH}mkastosc.c
	cd ${CWD}
	echo ${DIRPATH}mkdctype.c
	cd ${CWD}
	echo ${DIRPATH}mkdstoas.c
	cd ${CWD}
	echo ${DIRPATH}mkhits.c
	cd ${CWD}
	echo ${DIRPATH}prt3270.c
	cd ${CWD}
	echo ${DIRPATH}mkmake.y
	cd ${CWD}
	echo ${DIRPATH}makefile
	cd ${CWD}
	echo ${DIRPATH}makefile.mak
	cd ${CWD}
	
	

print:	 
	${PRINT} ${ALLPRINT}

tags:	${ALLC} ${ALLH} 
	ctags -t ${ALLC} ${ALLH}

action:	 
	${ACTION}

clean:	 
	${RM} errs
	cd ${CWD}
	${RM} makefile.bak
	cd ${CWD}
	${RM} y.tab.c
	cd ${CWD}
	${RM} y.output
	cd ${CWD}
	${RM} prt3270.obj
	cd ${CWD}
	${RM} mkdctype.obj
	cd ${CWD}
	${RM} ectype.obj
	cd ${CWD}
	${RM} mkastods.obj
	cd ${CWD}
	${RM} mkdstoas.obj
	cd ${CWD}
	${RM} mkhits.obj
	cd ${CWD}
	${RM} mkmake.obj
	cd ${CWD}
	${RM} dohits.obj
	cd ${CWD}
	${RM} mkastosc.obj
	cd ${CWD}
	${RM} mkastods.exe
	cd ${CWD}
	${RM} mkastosc.exe
	cd ${CWD}
	${RM} mkdctype.exe
	cd ${CWD}
	${RM} mkdstoas.exe
	cd ${CWD}
	${RM} mkhits.exe
	cd ${CWD}
	${RM} 
	cd ${CWD}
	${RM} prt3270.exe
	cd ${CWD}
	
	

sccsclean:	 
	-sccs clean
	-sccs get makefile

lint:	 
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} -DTN3270 ${TNMAIN} ${MOSTC} -lcurses
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} ${MSMAIN} map3270.c -lcurses

.DEFAULT:	 
	sccs get $<

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

dohits${O}:	..\general\general.h ..\api\asc_ebc.h ..\api\ebc_disp.h ..\ctlr\function.h dohits.h 
	

ectype${O}:	ectype.h 
	

mkastods${O}:	..\api\asc_ebc.h ..\api\ebc_disp.h 
	

mkastosc${O}:	..\general\general.h ..\ctlr\function.h dohits.h 
	

mkdctype${O}:	..\api\ebc_disp.h ectype.h 
	

mkdstoas${O}:	..\api\asc_ebc.h ..\api\ebc_disp.h 
	

mkhits${O}:	..\ctlr\function.h dohits.h 
	

prt3270${O}:	..\general\general.h ..\api\asc_ebc.h ..\ctlr\hostctlr.h ..\ctlr\screen.h ..\ctlr\function.h ..\api\astosc.h ..\general\globals.h ..\ctlr\kbd.out 
	
