###################################################################
###################################################################
###								###
###								###
###	*****    *   *    *****    ****     *****     ****	###
###	  *      **  *    *        *   *    *        *    	###
###	  *      * * *    * ***    ****     ***       *** 	###
###	  *      *  **    *   *    *  *     *            *	###
###	*****    *   *    *****    *   *    *****    **** 	###
###								###
###								###
###	      A relational database management system		###
###								###
###			    RELEASE 7				###
###								###
###################################################################
###################################################################
#
#	For information contact:
#		Mike Stonebraker
#		Dept. of Electrical Engineering &
#		  Computer Science
#		University of California
#		Berkeley, California  94720
#

#
#	@(#)Makefile	7.3	3/4/81
#

#
#  This makefile compiles most of the modules for the bulk of
#  the INGRES system.  The modules affected are those used by
#  the "ingres" command itself.  Stand-alone utilities such as
#  "printr" and "restore" are in the 'support' directory.
#

#
#	Major recompilation will take place from this directory.
#	There are several interesting entry points:
#		allclean	cleans all cruft from all
#				directories.
#		allinstall	installs binaries in the INGRES
#				bin directory.
#			INSTALL=actual entry point called.
#		sysinstall	installs that subset of the INGRES
#				system in public directories.
#			SYSBIN=	bin directory that the binaries
#				should live in.
#			SYSLIB=	directory that libraries should live
#				in (typically /usr/lib).
#	for all of these:
#		CPUTYPE=type of cpu you are running on, either PDP
#			or VAX.  Actually, this falacious; there are
#			many other changes that must be made between
#			the two machines (mostly in LDFLAGS entries,
#			etc.).  If `make' only had "conditional
#			compilation"....
#
#	Examples of usage:
#		To recompile entire system from sources:
#			make allclean allinstall
#		To install INGRES for general users:
#			make allinstall sysinstall
#		To print the entire system:
#			make install INSTALL=print
#			{yes, this is cheating}
#

#
#  General system paths
#

# the ingres subtree
INGRES=	../..
# the location of the system header files
H=	../h
# the residence of the system (internal) libraries
LIB=	$(INGRES)/lib
# the residence of the system (internal) binaries
# this entry must be reflected in the process table
BIN=	$(INGRES)/bin
# the location of user binaries
SYSBIN=	/usr/local
# the location of user libraries (libq.a)
SYSLIB=	/usr/local/lib
# the cputype, PDP or VAX
CPUTYPE=VAX

PDPALL=	monitor parser qrymod decomp alldbu
VAXALL=	monitor vaxingres
ALLALL=	monitor parser qrymod decomp alldbu vaxingres sysdump

SRCS=	monitor.c parser.c qrymod.c decomp.c alldbu.c vaxingres.c \
	sysdump.c version.c

#
#  Libraries
#
#	Libraries should be loaded in the order listed below.
#
CTLMOD=	$(LIB)/ctlmod
IUTIL=	$(LIB)/iutil
GUTIL=	$(LIB)/gutil
MATH=	-lm

ALLLIB=	$(CTLMOD) $(IUTIL) $(GUTIL)

#
#  Binaries
#
MONITOR=$(LIB)/monitor
PARSER=	$(LIB)/parser
QRYMOD=	$(LIB)/qrymod
DECOMP=	$(LIB)/decomp
OVQP=	$(LIB)/ovqp
DBULIB=	$(LIB)/dbulib

#
#  Assorted commands & other garbage
#
CFLAGS=	-I$H -O
LDFLAGS=-z
CHMOD=	chmod
BINMODE=751
SUMODE=	4740
CP=	cp
GET=	sccs get
REL=
PR=	print
VGRIND=	csh /usr/ucb/vgrind
HEADER=	Configuration Tables
MAKE=	make CPUTYPE=$(CPUTYPE)
MFLAGS=	-k
INSTALL=install

#
#	Recompile absolutely everything
#

all: all$(CPUTYPE)

allPDP: $(PDPALL)

allVAX: $(VAXALL)

allall: $(ALLALL)

#
#  And the processes....
#
#	Proper loading order is:
#		conf table
#		library(s) for modules
#		ctlmod library
#		iutil library
#		gutil library
#

#  Sysdump

sysdump: version.o sysdump.o $(CTLMOD) $(ALLLIB)
	$(CC) $(LDFLAGS) version.o $@.o $(OTHERS) $(ALLLIB) -o $@
	$(CHMOD) $(SUMODE) $@
	ls -l $@; size $@

#  Monitor

monitor: version.o monitor.o $(MONITOR) $(CTLMOD) $(IUTIL) $(GUTIL)
	$(CC) $(LDFLAGS) version.o $@.o $(OTHERS) $(MONITOR) $(CTLMOD) $(IUTIL) $(GUTIL) -o $@
	$(CHMOD) $(BINMODE) $@
	ls -l $@; size $@

#  Parser

parser: version.o parser.o $(PARSER) $(ALLLIB)
	$(CC) $(LDFLAGS) version.o $@.o $(OTHERS) $(PARSER) $(ALLLIB) $(MATH) -o $@
	$(CHMOD) $(SUMODE) $@
	ls -l $@; size $@

#  Query Modification

qrymod: version.o qrymod.o $(QRYMOD) $(ALLLIB)
	$(CC) $(LDFLAGS) version.o $@.o $(OTHERS) $(QRYMOD) $(ALLLIB) $(MATH) -o $@
	$(CHMOD) $(SUMODE) $@
	ls -l $@; size $@

#  Decomp/OVQP

decomp: version.o decomp.o $(DECOMP) $(OVQP) $(ALLLIB)
	$(CC) $(LDFLAGS) version.o $@.o $(OTHERS) $(DECOMP) $(OVQP) $(ALLLIB) $(MATH) -o $@
	$(CHMOD) $(SUMODE) $@
	ls -l $@; size $@

#  All DBU routines.

alldbu: version.o alldbu.o $(DBULIB) $(ALLLIB)
	$(CC) $(LDFLAGS) version.o $@.o $(OTHERS) $(DBULIB) $(ALLLIB) $(MATH) -o $@
	$(CHMOD) $(SUMODE) $@
	ls -l $@; size $@

#  Full VAX System

ALLLIBS=$(PARSER) $(QRYMOD) $(DECOMP) $(OVQP) $(DBULIB) $(ALLLIB)

vaxingres: version.o vaxingres.o $(ALLLIBS)
	$(CC) $(LDFLAGS) version.o $@.o $(OTHERS) $(ALLLIBS) $(MATH) -o $@
	$(CHMOD) $(SUMODE) $@
	ls -l $@; size $@

#################################################################
#
#	Install the world
#

install: install$(CPUTYPE) $(BIN)/sysdump $(BIN)/monitor

installPDP: $(BIN)/parser $(BIN)/qrymod $(BIN)/decomp $(BIN)/alldbu

installVAX: $(BIN)/vaxingres

$(BIN)/monitor: monitor
	$(CP) monitor $(BIN)/monitor
$(BIN)/parser: parser
	$(CP) parser $(BIN)/parser; $(CHMOD) $(SUMODE) $(BIN)/parser
$(BIN)/qrymod: qrymod
	$(CP) qrymod $(BIN)/qrymod; $(CHMOD) $(SUMODE) $(BIN)/qrymod
$(BIN)/decomp: decomp
	$(CP) decomp $(BIN)/decomp; $(CHMOD) $(SUMODE) $(BIN)/decomp
$(BIN)/alldbu: alldbu
	$(CP) alldbu $(BIN)/alldbu; $(CHMOD) $(SUMODE) $(BIN)/alldbu
$(BIN)/vaxingres: vaxingres
	$(CP) vaxingres $(BIN)/vaxingres; $(CHMOD) $(SUMODE) $(BIN)/vaxingres
$(BIN)/sysdump: sysdump
	$(CP) sysdump $(BIN)/sysdump; $(CHMOD) $(SUMODE) $(BIN)/sysdump

allinstall: version.o all1 install

all1:
	chdir ../h; $(MAKE) $(MFLAGS) $(INSTALL)
	chdir ../gutil; $(MAKE) $(MFLAGS) $(INSTALL)
	chdir ../iutil; $(MAKE) $(MFLAGS) $(INSTALL)
	chdir ../ctlmod; $(MAKE) $(MFLAGS) $(INSTALL)
	chdir ../dbu; $(MAKE) $(MFLAGS) $(INSTALL)
	chdir ../decomp; $(MAKE) $(MFLAGS) $(INSTALL)
	chdir ../equel; $(MAKE) $(MFLAGS) $(INSTALL)
	chdir ../libq; $(MAKE) $(MFLAGS) $(INSTALL)
	chdir ../monitor; $(MAKE) $(MFLAGS) $(INSTALL)
	chdir ../ovqp; $(MAKE) $(MFLAGS) $(INSTALL)
	chdir ../parser; $(MAKE) $(MFLAGS) $(INSTALL)
	chdir ../qrymod; $(MAKE) $(MFLAGS) $(INSTALL)
	chdir ../support; $(MAKE) $(MFLAGS) $(INSTALL)

# install the user programs
sysinstall:
	@echo 'you may have to be root for this to work....'
	chdir ../support; $(MAKE) $(MFLAGS) SYSBIN=$(SYSBIN) SYSLIB=$(SYSLIB) sysinstall
	chdir ../equel; $(MAKE) $(MFLAGS) SYSBIN=$(SYSBIN) SYSLIB=$(SYSLIB) sysinstall
	chdir ../libq; $(MAKE) $(MFLAGS) SYSBIN=$(SYSBIN) SYSLIB=$(SYSLIB) sysinstall

#################################################################
#
#	Miscellaneous entries
#

new: new$(CPUTYPE)
newPDP:
	rm -f $(PDPALL)
newVAX:
	rm -f $(VAXALL)

print: sources
	$(PR) Makefile *.[csy]

vgrind: sources
	cp /dev/null index
	$(VGRIND) -h "$(HEADER) (Release 7)" -n Makefile
	$(VGRIND) -h "$(HEADER) (Release 7)" *.[csy]
	$(VGRIND) -h "$(HEADER) (Release 7)" -x index

clean: new
	rm -f *.o
	rm -f a.out core

allclean: clean
	chdir ../h; $(MAKE) $(MFLAGS) clean
	chdir ../gutil; $(MAKE) $(MFLAGS) clean
	chdir ../iutil; $(MAKE) $(MFLAGS) clean
	chdir ../ctlmod; $(MAKE) $(MFLAGS) clean
	chdir ../dbu; $(MAKE) $(MFLAGS) clean
	chdir ../decomp; $(MAKE) $(MFLAGS) clean
	chdir ../equel; $(MAKE) $(MFLAGS) clean
	chdir ../libq; $(MAKE) $(MFLAGS) clean
	chdir ../monitor; $(MAKE) $(MFLAGS) clean
	chdir ../ovqp; $(MAKE) $(MFLAGS) clean
	chdir ../parser; $(MAKE) $(MFLAGS) clean
	chdir ../qrymod; $(MAKE) $(MFLAGS) clean
	chdir ../support; $(MAKE) $(MFLAGS) clean

sources: $(SRCS)

$(SRCS):
	$(GET) $(REL) SCCS/s.$@

depend:
	grep '^#[ 	]*include' $(SRCS) | sed -n -f ../h/depend.sed > makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

monitor.o:$H/func.h
parser.o:$H/ingres.h
parser.o:$H/aux.h
parser.o:$H/access.h
parser.o:$H/func.h
qrymod.o:$H/func.h
decomp.o:$H/func.h
alldbu.o:$H/func.h
vaxingres.o:$H/ingres.h
vaxingres.o:$H/aux.h
vaxingres.o:$H/access.h
vaxingres.o:$H/func.h
sysdump.o:$H/func.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
