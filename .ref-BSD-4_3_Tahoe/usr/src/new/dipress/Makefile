#
# Copyright (c) 1984, 1985, 1986 Xerox Corp.
#
#
#  You might want to change the following defines:
#	TITROFFBINARY	the location of your typesetter-independent troff
#	DESTBIN		where the executable files should go
#	DESTINCLUDE	where the ".h" files go
#	DESTMAN		where the manual pages go

TITROFFBINARY = /usr/local/bin/titroff
DESTBIN = /usr/new
DESTINCLUDE = /usr/new/include
DESTLIB = /usr/new/lib
DESTMAN = /usr/man/mann

# the following is still experimental
DESTMACHINES = r-wing

#
# END of user tunable paramters
#-------------------------------------------------------------

#  targets:
#	all		- recompile and link everything
#	install		- copy include files, libraries, fonts and executables
#			  to correct destination 
#	rdist		- copy installed files to other machines running 4.3bsd
#

# set up for recursive makes
MFLAGS = DESTBIN=${DESTBIN} DESTINCLUDE=${DESTINCLUDE} DESTLIB=${DESTLIB} \
	DESTMACHINES=${DESTMACHINES} DESTMAN=${DESTMAN} \
	TITROFFBINARY=${TITROFFBINARY}
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
# order is important on the next line!
SUBDIR=	src conf
LINTDIR= src
INSTALLDIR= include bin lib man/mann
CLEANDIR= doc src conf bin lib


all:
	for i in ${SUBDIR}; do \
		(cd $$i; make install); done

lint:
	for i in ${LINTDIR}; do \
		(cd $$i; make lint); done

# install assumes that a previous "make all" has been done

install:
	for i in ${INSTALLDIR}; do \
		(cd $$i; make ${MFLAGS} install); done

# rdist assumes that a previous "make install" has been done

rdist:
	for i in ${INSTALLDIR}; do \
		(cd $$i; make ${MFLAGS} rdist); done

clean:
	rm -f a.out core *.s *.o
	for i in ${CLEANDIR}; do \
		(cd $$i; make ${MFLAGS} clean); done
	@echo "clean-up complete"
