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
ACTION =	sccs tell
DEFINES =	${LINT_ARGS}
INCLUDES =	-I. -I..
OPTIMIZE =	${DEBUG_FLAGS}
CFLAGS =	${OPTIMIZE} ${INCLUDES} ${DEFINES}
LINTFLAGS =	-hbxaz
DESTDIR =	
BINDIR =	${DESTDIR}\usr\ucb
LIBCURSES =	
LIBTERM =	
ALLH =	telnet.ext
TNMAIN =	telnet.c
MSMAIN =	ascii\mset.c
ALLC =	${TNMAIN}
ALLO =	telnet${O} mset${O}
ALLPRINT =	${ALLH} ${ALLC}
ALLSOURCE =	${ALLPRINT} makefile makefile.mak makefile_4.2 README
SYS =	sys_dos
SUBDIR =	api ascii ctlr general ${SYS}
EXTRADIR =	arpa sys_dos tools utilities
SUBLIB =	${SYS}\sys.lib ctlr\ctlr.lib ascii\ascii.lib general\general.lib


#targets...

.s.o:	 
	\lib\cpp -E $< | as -o $@

.c.obj:	 
	${CC} ${CFLAGS} -c $<

all:	FRC tn3270${X} mset${X} 
	

FRC:	 
	cd api
	 make ${MFLAGS} "CFLAGS=${CFLAGS}"
	cd ${CWD}
	cd ascii
	 make ${MFLAGS} "CFLAGS=${CFLAGS}"
	cd ${CWD}
	cd ctlr
	 make ${MFLAGS} "CFLAGS=${CFLAGS}"
	cd ${CWD}
	cd general
	 make ${MFLAGS} "CFLAGS=${CFLAGS}"
	cd ${CWD}
	cd sys_dos
	 make ${MFLAGS} "CFLAGS=${CFLAGS}"
	cd ${CWD}
	
	

tn3270${X}:	telnet${O} ${SUBLIB} api\api.lib  
	link <@<
	telnet
	tn3270
	nul
	${SUBLIB} api\api.lib+
	\lib\ublib\ubtcp
<

mset${X}:	mset${O} ascii\map3270${O} 
	${CC} ${CFLAGS} -o mset mset${O} ascii\map3270${O} ${L} api\api.lib

telnet${O}:	${TNMAIN} ascii\termin.ext ctlr\screen.h ctlr\oia.h ctlr\options.ext ctlr\outbound.ext general\globals.h telnet.ext general\general.h 
	${CC} ${CFLAGS} -DTN3270 -c ${TNMAIN}

mset${O}:	${MSMAIN} 
	${CC} ${CFLAGS} -c ${MSMAIN}

install:	tn3270${X} mset${X} 
	install -m 755 -o bin -g bin -s tn3270 ${BINDIR}
	install -m 755 -o bin -g bin -s mset ${BINDIR}

action:	 
	${ACTION}

everywhere:	action 
	echo "[$$i]"
	 cd api
	 make ${MFLAGS} action "ACTION=${ACTION}"
	cd ${CWD}
	echo "[$$i]"
	 cd ascii
	 make ${MFLAGS} action "ACTION=${ACTION}"
	cd ${CWD}
	echo "[$$i]"
	 cd ctlr
	 make ${MFLAGS} action "ACTION=${ACTION}"
	cd ${CWD}
	echo "[$$i]"
	 cd general
	 make ${MFLAGS} action "ACTION=${ACTION}"
	cd ${CWD}
	echo "[$$i]"
	 cd sys_dos
	 make ${MFLAGS} action "ACTION=${ACTION}"
	cd ${CWD}
	echo "[$$i]"
	 cd arpa
	 make ${MFLAGS} action "ACTION=${ACTION}"
	cd ${CWD}
	echo "[$$i]"
	 cd sys_dos
	 make ${MFLAGS} action "ACTION=${ACTION}"
	cd ${CWD}
	echo "[$$i]"
	 cd tools
	 make ${MFLAGS} action "ACTION=${ACTION}"
	cd ${CWD}
	echo "[$$i]"
	 cd utilities
	 make ${MFLAGS} action "ACTION=${ACTION}"
	cd ${CWD}
	
	

clean:	 
	${RM} telnet.obj
	cd ${CWD}
	${RM} mset.obj
	cd ${CWD}
	${RM} mset
	cd ${CWD}
	${RM} tn3270
	cd ${CWD}
	${RM} errs
	cd ${CWD}
	${RM} makefile.bak
	cd ${CWD}
	
	
	cd api
	 make ${MFLAGS} clean
	cd ${CWD}
	cd ascii
	 make ${MFLAGS} clean
	cd ${CWD}
	cd ctlr
	 make ${MFLAGS} clean
	cd ${CWD}
	cd general
	 make ${MFLAGS} clean
	cd ${CWD}
	cd sys_dos
	 make ${MFLAGS} clean
	cd ${CWD}
	cd arpa
	 make ${MFLAGS} clean
	cd ${CWD}
	cd sys_dos
	 make ${MFLAGS} clean
	cd ${CWD}
	cd tools
	 make ${MFLAGS} clean
	cd ${CWD}
	cd utilities
	 make ${MFLAGS} clean
	cd ${CWD}
	
	

sccsclean:	 
	-sccs clean
	-sccs get makefile
	cd api
	 make ${MFLAGS} sccsclean
	cd ${CWD}
	cd ascii
	 make ${MFLAGS} sccsclean
	cd ${CWD}
	cd ctlr
	 make ${MFLAGS} sccsclean
	cd ${CWD}
	cd general
	 make ${MFLAGS} sccsclean
	cd ${CWD}
	cd sys_dos
	 make ${MFLAGS} sccsclean
	cd ${CWD}
	cd arpa
	 make ${MFLAGS} sccsclean
	cd ${CWD}
	cd sys_dos
	 make ${MFLAGS} sccsclean
	cd ${CWD}
	cd tools
	 make ${MFLAGS} sccsclean
	cd ${CWD}
	cd utilities
	 make ${MFLAGS} sccsclean
	cd ${CWD}
	
	

print:	 
	${PRINT} ${ALLPRINT}
	cd api
	 make ${MFLAGS} "PRINT=${PRINT}" print
	cd ${CWD}
	cd ascii
	 make ${MFLAGS} "PRINT=${PRINT}" print
	cd ${CWD}
	cd ctlr
	 make ${MFLAGS} "PRINT=${PRINT}" print
	cd ${CWD}
	cd general
	 make ${MFLAGS} "PRINT=${PRINT}" print
	cd ${CWD}
	cd sys_dos
	 make ${MFLAGS} "PRINT=${PRINT}" print
	cd ${CWD}
	cd arpa
	 make ${MFLAGS} "PRINT=${PRINT}" print
	cd ${CWD}
	cd sys_dos
	 make ${MFLAGS} "PRINT=${PRINT}" print
	cd ${CWD}
	cd tools
	 make ${MFLAGS} "PRINT=${PRINT}" print
	cd ${CWD}
	cd utilities
	 make ${MFLAGS} "PRINT=${PRINT}" print
	cd ${CWD}
	
	

tags:	${ALLC} ${ALLH} 
	ctags -t ${ALLC} ${ALLH}

sourcelist:	${ALLSOURCE} 
	echo ${DIRPATH}telnet.ext
	cd ${CWD}
	echo ${DIRPATH}telnet.c
	cd ${CWD}
	echo ${DIRPATH}makefile
	cd ${CWD}
	echo ${DIRPATH}makefile.mak
	cd ${CWD}
	echo ${DIRPATH}makefile_4.2
	cd ${CWD}
	echo ${DIRPATH}README
	cd ${CWD}
	
	
	cd api
	 make ${MFLAGS} "DIRPATH=${DIRPATH}$$i\" sourcelist
	cd ${CWD}
	cd ascii
	 make ${MFLAGS} "DIRPATH=${DIRPATH}$$i\" sourcelist
	cd ${CWD}
	cd ctlr
	 make ${MFLAGS} "DIRPATH=${DIRPATH}$$i\" sourcelist
	cd ${CWD}
	cd general
	 make ${MFLAGS} "DIRPATH=${DIRPATH}$$i\" sourcelist
	cd ${CWD}
	cd sys_dos
	 make ${MFLAGS} "DIRPATH=${DIRPATH}$$i\" sourcelist
	cd ${CWD}
	cd arpa
	 make ${MFLAGS} "DIRPATH=${DIRPATH}$$i\" sourcelist
	cd ${CWD}
	cd sys_dos
	 make ${MFLAGS} "DIRPATH=${DIRPATH}$$i\" sourcelist
	cd ${CWD}
	cd tools
	 make ${MFLAGS} "DIRPATH=${DIRPATH}$$i\" sourcelist
	cd ${CWD}
	cd utilities
	 make ${MFLAGS} "DIRPATH=${DIRPATH}$$i\" sourcelist
	cd ${CWD}
	
	

lint:	 
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} -DTN3270 ${TNMAIN} -lcurses
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} ${MSMAIN} map3270.c -lcurses

makefiles.pc:	tools\mkmake 
	sed -e "s\lib\.a\.lib\g" -e "s\^#PC_\\" < $$i\makefile | .\tools\mkmake | sed -e "sx\x\\\\xg" -e "s\[ 	]*\\" > $$i\makefile.mak
	cd ${CWD}
	sed -e "s\lib\.a\.lib\g" -e "s\^#PC_\\" < $$i\makefile | .\tools\mkmake | sed -e "sx\x\\\\xg" -e "s\[ 	]*\\" > $$i\makefile.mak
	cd ${CWD}
	sed -e "s\lib\.a\.lib\g" -e "s\^#PC_\\" < $$i\makefile | .\tools\mkmake | sed -e "sx\x\\\\xg" -e "s\[ 	]*\\" > $$i\makefile.mak
	cd ${CWD}
	sed -e "s\lib\.a\.lib\g" -e "s\^#PC_\\" < $$i\makefile | .\tools\mkmake | sed -e "sx\x\\\\xg" -e "s\[ 	]*\\" > $$i\makefile.mak
	cd ${CWD}
	sed -e "s\lib\.a\.lib\g" -e "s\^#PC_\\" < $$i\makefile | .\tools\mkmake | sed -e "sx\x\\\\xg" -e "s\[ 	]*\\" > $$i\makefile.mak
	cd ${CWD}
	sed -e "s\lib\.a\.lib\g" -e "s\^#PC_\\" < $$i\makefile | .\tools\mkmake | sed -e "sx\x\\\\xg" -e "s\[ 	]*\\" > $$i\makefile.mak
	cd ${CWD}
	sed -e "s\lib\.a\.lib\g" -e "s\^#PC_\\" < $$i\makefile | .\tools\mkmake | sed -e "sx\x\\\\xg" -e "s\[ 	]*\\" > $$i\makefile.mak
	cd ${CWD}
	sed -e "s\lib\.a\.lib\g" -e "s\^#PC_\\" < $$i\makefile | .\tools\mkmake | sed -e "sx\x\\\\xg" -e "s\[ 	]*\\" > $$i\makefile.mak
	cd ${CWD}
	sed -e "s\lib\.a\.lib\g" -e "s\^#PC_\\" < $$i\makefile | .\tools\mkmake | sed -e "sx\x\\\\xg" -e "s\[ 	]*\\" > $$i\makefile.mak
	cd ${CWD}
	sed -e "s\lib\.a\.lib\g" -e "s\^#PC_\\" < $$i\makefile | .\tools\mkmake | sed -e "sx\x\\\\xg" -e "s\[ 	]*\\" > $$i\makefile.mak
	cd ${CWD}
	
	

tools\mkmake:	 
	cd tools
	 make mkmake
	cd ${CWD}
	

.DEFAULT:	 
	sccs get $<

depend:	thisdepend 
	cd api
	 make ${MFLAGS} depend
	cd ${CWD}
	cd ascii
	 make ${MFLAGS} depend
	cd ${CWD}
	cd ctlr
	 make ${MFLAGS} depend
	cd ${CWD}
	cd general
	 make ${MFLAGS} depend
	cd ${CWD}
	cd sys_dos
	 make ${MFLAGS} depend
	cd ${CWD}
	
	

thisdepend:	 
	echo > eddep.c
	grep '^#include' ${ALLC} eddep.c | grep -v '<' | sed -e 's\:[^"]*"\([^"]*\)".*\: \1\' -e 's\\.c\$$O\' | awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
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
	rm eddep makedep eddep.c
