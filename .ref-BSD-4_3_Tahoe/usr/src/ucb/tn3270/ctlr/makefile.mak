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
ALLH =	api.h function.h hostctlr.h inbound.ext oia.h options.ext options.h outbound.ext screen.h scrnctlr.h
ALLC =	api.c function.c inbound.c oia.c options.c outbound.c
ALLO =	api${O} inbound${O} oia${O} options${O} outbound${O}
ALLPRINT =	3180.kbd 3270pc.kbd unix.kbd ${ALLH} ${ALLC}
ALLSOURCE =	${ALLPRINT} makefile makefile.mak


#targets...

.s.o:	 
	\lib\cpp -E $< | as -o $@

.c.obj:	 
	${CC} ${CFLAGS} -c $<

ctlr.lib:	${ALLO} 
	${RM} $@
	${AR} ${AR1} $@ ${AR2} api.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} inbound.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} oia.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} options.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} outbound.obj${AR3}
	cd ${CWD}
	
	
	${RANLIB} $@

clean:	 
	${RM} api.obj
	cd ${CWD}
	${RM} inbound.obj
	cd ${CWD}
	${RM} oia.obj
	cd ${CWD}
	${RM} options.obj
	cd ${CWD}
	${RM} outbound.obj
	cd ${CWD}
	${RM} mset
	cd ${CWD}
	${RM} tn3270
	cd ${CWD}
	${RM} prt3270
	cd ${CWD}
	${RM} m4.out
	cd ${CWD}
	${RM} errs
	cd ${CWD}
	${RM} makefile.bak
	cd ${CWD}
	${RM} ctlr.lib
	cd ${CWD}
	${RM} kbd.out
	cd ${CWD}
	${RM} TMPfunc.out
	cd ${CWD}
	
	

sccsclean:	 
	-sccs clean
	-sccs get makefile

sourcelist:	${ALLSOURCE} 
	echo ${DIRPATH}3180.kbd
	cd ${CWD}
	echo ${DIRPATH}3270pc.kbd
	cd ${CWD}
	echo ${DIRPATH}unix.kbd
	cd ${CWD}
	echo ${DIRPATH}api.h
	cd ${CWD}
	echo ${DIRPATH}function.h
	cd ${CWD}
	echo ${DIRPATH}hostctlr.h
	cd ${CWD}
	echo ${DIRPATH}inbound.ext
	cd ${CWD}
	echo ${DIRPATH}oia.h
	cd ${CWD}
	echo ${DIRPATH}options.ext
	cd ${CWD}
	echo ${DIRPATH}options.h
	cd ${CWD}
	echo ${DIRPATH}outbound.ext
	cd ${CWD}
	echo ${DIRPATH}screen.h
	cd ${CWD}
	echo ${DIRPATH}scrnctlr.h
	cd ${CWD}
	echo ${DIRPATH}api.c
	cd ${CWD}
	echo ${DIRPATH}function.c
	cd ${CWD}
	echo ${DIRPATH}inbound.c
	cd ${CWD}
	echo ${DIRPATH}oia.c
	cd ${CWD}
	echo ${DIRPATH}options.c
	cd ${CWD}
	echo ${DIRPATH}outbound.c
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
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} -DTN3270 ${ALLC} -lcurses

.DEFAULT:	 
	sccs get $<

kbd.out:	${KBD} hostctlr.h 
	cd ..\tools
	 make mkhits${X}
	cd ${CWD}
	
	${RM} $@ TMPfunc.out
	${CC} ${CFLAGS} -E function.c > TMPfunc.out
	..\tools\mkhits - ..\ctlr\TMPfunc.out < ${KBD} > $@
	${RM} TMPfunc.out

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

api${O}:	api.h ..\general\general.h ..\api\disp_asc.h screen.h oia.h ..\general\globals.h 
	

function${O}:	function.h 
	

inbound${O}:	..\general\general.h function.h hostctlr.h oia.h scrnctlr.h screen.h options.h ..\api\dctype.h ..\api\ebc_disp.h ..\general\globals.h inbound.ext outbound.ext ..\telnet.ext kbd.out 
	

oia${O}:	..\general\general.h oia.h ..\general\globals.h 
	

options${O}:	options.h ..\general\globals.h options.ext 
	

outbound${O}:	..\general\general.h hostctlr.h oia.h screen.h ..\api\ebc_disp.h ..\general\globals.h options.ext ..\telnet.ext inbound.ext outbound.ext ..\general\bsubs.ext 
	
