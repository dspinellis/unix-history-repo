# variables...
ALLPRINT =	${ALLH} ${ALLC} vaxbsubs.s genbsubs.c
ALLSOURCE =	${ALLPRINT} makefile makefile.mak
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
DESTDIR =	
BINDIR =	${DESTDIR}\usr\ucb
ETCDIR =	${DESTDIR}\etc
MANDIR =	${DESTDIR}\usr\man\man
LIBCURSES =	-lcurses
LIBTERM =	-ltermlib
ALLH =	bsubs.ext general.h globals.h
ALLC =	globals.c
SUBS =	genbsubs.c
SUBSO =	genbsubs${O}
ALLS =	
ALLO =	globals${O} ${SUBSO}


#targets...

.s.o:	 
	\lib\cpp -E $< | as -o $@

.c.obj:	 
	${CC} ${CFLAGS} -c $<

general.lib:	${ALLO} 
	${RM} $@
	${AR} ${AR1} $@ ${AR2} globals.obj${AR3}
	cd ${CWD}
	${AR} ${AR1} $@ ${AR2} genbsubs.obj${AR3}
	cd ${CWD}
	
	
	${RANLIB} $@

clean:	 
	${RM} globals.obj
	cd ${CWD}
	${RM} genbsubs.obj
	cd ${CWD}
	${RM} errs
	cd ${CWD}
	${RM} makefile.bak
	cd ${CWD}
	${RM} general.lib
	cd ${CWD}
	
	

sccsclean:	 
	-sccs clean
	-sccs get makefile

sourcelist:	${ALLSOURCE} 
	echo ${DIRPATH}bsubs.ext
	cd ${CWD}
	echo ${DIRPATH}general.h
	cd ${CWD}
	echo ${DIRPATH}globals.h
	cd ${CWD}
	echo ${DIRPATH}globals.c
	cd ${CWD}
	echo ${DIRPATH}vaxbsubs.s
	cd ${CWD}
	echo ${DIRPATH}genbsubs.c
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

globals${O}:	..\ctlr\hostctlr.h ..\ctlr\oia.h ..\ctlr\options.h ..\ctlr\screen.h globals.h ..\general\general.h 
	
