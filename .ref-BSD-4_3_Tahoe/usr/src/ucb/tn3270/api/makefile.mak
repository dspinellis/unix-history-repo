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
CFLAGS =	${DEBUG_FLAGS}
PRINT =	lpr -p
KBD =	unix.kbd
ALLC =	apilib.c api_bsd.c api_exch.c asc_ebc.c astosc.c dctype.c disp_asc.c ebc_disp.c
ALLH =	apilib.h api_exch.h asc_ebc.h astosc.h disp_asc.h dctype.h ebc_disp.h
ALLPRINT =	${ALLH} ${ALLC}
ALLSOURCE =	${ALLPRINT} makefile makefile.mak
ALLO =	apilib${O} api_bsd${O} api_exch${O} astosc${O} asc_ebc${O} dctype${O} disp_asc${O} ebc_disp${O}


#targets...

.c.obj:	 
	${CC} ${CFLAGS} -c $<

api.lib:	${ALLO} 
	${RM} $@
	${AR} ${AR1} $@ ${AR2} apilib.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} api_bsd.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} api_exch.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} astosc.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} asc_ebc.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} dctype.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} disp_asc.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} ebc_disp.obj${AR3}
	cd ${CWD}
	
	
	${RANLIB} $@

.DEFAULT:	 
	sccs get $<

clean:	 
	${RM} apilib.obj
	cd ${CWD}
	${RM} api_bsd.obj
	cd ${CWD}
	${RM} api_exch.obj
	cd ${CWD}
	${RM} astosc.obj
	cd ${CWD}
	${RM} asc_ebc.obj
	cd ${CWD}
	${RM} dctype.obj
	cd ${CWD}
	${RM} disp_asc.obj
	cd ${CWD}
	${RM} ebc_disp.obj
	cd ${CWD}
	${RM} errs
	cd ${CWD}
	${RM} api.lib
	cd ${CWD}
	${RM} makefile.bak
	cd ${CWD}
	${RM} disp_out
	cd ${CWD}
	${RM} asc_disp.out
	cd ${CWD}
	${RM} astosc.out
	cd ${CWD}
	${RM} disp_asc.out
	cd ${CWD}
	${RM} test*
	cd ${CWD}
	${RM} test.obj
	cd ${CWD}
	${RM} t1*
	cd ${CWD}
	${RM} t1.obj
	cd ${CWD}
	${RM} t2*
	cd ${CWD}
	${RM} t2.obj
	cd ${CWD}
	
	

sccsclean:	 
	-sccs clean
	-sccs get makefile

action:	 
	${ACTION}

test:	api.lib test${O} 
	${CC} ${CFLAGS} -o $@ test${O} api.lib

t1:	api.lib t1${O} 
	${CC} ${CFLAGS} -o $@ t1${O} api.lib

t2:	api.lib t2${O} 
	${CC} ${CFLAGS} -o $@ t2${O} api.lib

print:	 
	${PRINT} ${ALLPRINT}

sourcelist:	${ALLSOURCE} 
	echo ${DIRPATH}apilib.h
	cd ${CWD}
	echo ${DIRPATH}api_exch.h
	cd ${CWD}
	echo ${DIRPATH}asc_ebc.h
	cd ${CWD}
	echo ${DIRPATH}astosc.h
	cd ${CWD}
	echo ${DIRPATH}disp_asc.h
	cd ${CWD}
	echo ${DIRPATH}dctype.h
	cd ${CWD}
	echo ${DIRPATH}ebc_disp.h
	cd ${CWD}
	echo ${DIRPATH}apilib.c
	cd ${CWD}
	echo ${DIRPATH}api_bsd.c
	cd ${CWD}
	echo ${DIRPATH}api_exch.c
	cd ${CWD}
	echo ${DIRPATH}asc_ebc.c
	cd ${CWD}
	echo ${DIRPATH}astosc.c
	cd ${CWD}
	echo ${DIRPATH}dctype.c
	cd ${CWD}
	echo ${DIRPATH}disp_asc.c
	cd ${CWD}
	echo ${DIRPATH}ebc_disp.c
	cd ${CWD}
	echo ${DIRPATH}makefile
	cd ${CWD}
	echo ${DIRPATH}makefile.mak
	cd ${CWD}
	
	

astosc.out:	..\ctlr\function.h ..\ctlr\hostctlr.h ..\ctlr\${KBD} 
	cd ..\tools
	 make mkastosc${X}
	cd ${CWD}
	
	${RM} $@
	..\tools\mkastosc < ..\ctlr\${KBD} > $@

asc_disp.out:	ebc_disp${O} 
	cd ..\tools
	 make mkastods${X}
	cd ${CWD}
	
	${RM} $@
	..\tools\mkastods > $@

disp_asc.out:	ebc_disp${O} 
	cd ..\tools
	 make mkdstoas${X}
	cd ${CWD}
	
	${RM} $@
	..\tools\mkdstoas > $@

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

apilib${O}:	..\ctlr\api.h apilib.h 
	

api_bsd${O}:	..\ctlr\api.h api_exch.h 
	

api_exch${O}:	..\general\general.h api_exch.h 
	

asc_ebc${O}:	asc_ebc.h 
	

astosc${O}:	..\general\general.h ..\ctlr\function.h astosc.h astosc.out 
	

dctype${O}:	dctype.h 
	

disp_asc${O}:	disp_asc.h asc_disp.out disp_asc.out 
	
