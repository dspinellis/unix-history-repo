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
KBD =	unix.kbd
ALLH =	map3270.ext state.h termin.ext
ALLC =	map3270.c mset.c termin.c
ALLO =	map3270${O} termin${O}
ALLPRINT =	default.map ${ALLH} ${ALLC}
ALLSOURCE =	${ALLPRINT} makefile makefile.mak


#targets...

.s.o:	 
	\lib\cpp -E $< | as -o $@

.c.obj:	 
	${CC} ${CFLAGS} -c $<

ascii.lib:	${ALLO} 
	${RM} $@
	${AR} ${AR1} $@ ${AR2} map3270.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} termin.obj${AR3}
	cd ${CWD}
	
	
	${RANLIB} $@

clean:	 
	${RM} map3270.obj
	cd ${CWD}
	${RM} termin.obj
	cd ${CWD}
	${RM} errs
	cd ${CWD}
	${RM} makefile.bak
	cd ${CWD}
	${RM} ascii.lib
	cd ${CWD}
	
	

sccsclean:	 
	-sccs clean
	-sccs get makefile

sourcelist:	${ALLSOURCE} 
	echo ${DIRPATH}default.map
	cd ${CWD}
	echo ${DIRPATH}map3270.ext
	cd ${CWD}
	echo ${DIRPATH}state.h
	cd ${CWD}
	echo ${DIRPATH}termin.ext
	cd ${CWD}
	echo ${DIRPATH}map3270.c
	cd ${CWD}
	echo ${DIRPATH}mset.c
	cd ${CWD}
	echo ${DIRPATH}termin.c
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

lint:	 
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} ${ALLC}

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

map3270${O}:	state.h ..\general\globals.h map3270.ext default.map 
	

mset${O}:	..\ctlr\function.h state.h ..\api\astosc.h ..\general\globals.h map3270.ext 
	

termin${O}:	..\general\general.h ..\ctlr\function.h ..\ctlr\inbound.ext ..\ctlr\outbound.ext ..\telnet.ext termin.ext ..\api\astosc.h state.h ..\general\globals.h 
	
