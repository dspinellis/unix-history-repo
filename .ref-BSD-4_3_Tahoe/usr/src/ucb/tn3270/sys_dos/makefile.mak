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
ALLH =	spint.h termout.ext video.h
ALLC =	spintc.c system.c termout.c
ALLASM =	spintasm.asm support.asm
ALLO =	spintasm${O} spintc${O} support${O} system${O} termout${O}
ALLPRINT =	${ALLH} ${ALLASM} ${ALLC}
ALLSOURCE =	${ALLPRINT} makefile makefile.mak


#targets...

.c.obj:	 
	${CC} ${CFLAGS} -c $<

sys.lib:	${ALLO} 
	${RM} $@
	${AR} ${AR1} $@ ${AR2} spintasm.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} spintc.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} support.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} system.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} termout.obj${AR3}
	cd ${CWD}
	
	
	${RANLIB} $@

clean:	 
	${RM} spintasm.obj
	cd ${CWD}
	${RM} spintc.obj
	cd ${CWD}
	${RM} support.obj
	cd ${CWD}
	${RM} system.obj
	cd ${CWD}
	${RM} termout.obj
	cd ${CWD}
	${RM} errs
	cd ${CWD}
	${RM} makefile.bak
	cd ${CWD}
	${RM} sys.lib
	cd ${CWD}
	
	

sccsclean:	 
	-sccs clean
	-sccs get makefile

sourcelist:	${ALLSOURCE} 
	echo ${DIRPATH}spint.h
	cd ${CWD}
	echo ${DIRPATH}termout.ext
	cd ${CWD}
	echo ${DIRPATH}video.h
	cd ${CWD}
	echo ${DIRPATH}spintasm.asm
	cd ${CWD}
	echo ${DIRPATH}support.asm
	cd ${CWD}
	echo ${DIRPATH}spintc.c
	cd ${CWD}
	echo ${DIRPATH}system.c
	cd ${CWD}
	echo ${DIRPATH}termout.c
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
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} -DTN3270 ${TNMAIN} ${MOSTC} -lcurses
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} ${MSMAIN} map3270.c -lcurses

.DEFAULT:	 
	sccs get $<

depend:	 
	grep '^#include' ${ALLC} ${ALLH} | grep -v '<' | sed -e 's\:[^"]*"\([^"]*\)".*\: \1\' -e 's\\.c\$$O\' | awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
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

spintc${O}:	..\general\general.h spint.h 
	

system${O}:	..\general\general.h ..\ctlr\api.h spint.h ..\general\globals.h 
	

termout${O}:	..\general\general.h ..\telnet.ext ..\api\disp_asc.h ..\ascii\map3270.ext ..\ctlr\hostctlr.h ..\ctlr\inbound.ext ..\ctlr\oia.h ..\ctlr\options.ext ..\ctlr\outbound.ext ..\ctlr\screen.h ..\general\globals.h video.h 
	
