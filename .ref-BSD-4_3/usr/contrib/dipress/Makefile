#
# Copyright (c) 1984, 1985 Xerox Corp.
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

#
# END of user tunable paramters
#-------------------------------------------------------------

#  targets:
#	all		- recompile and link everything
#	install		- copy include files, libraries, fonts and executables
#			  to correct destination 
#

# set up for recursive makes
MFLAGS = DESTBIN=${DESTBIN} DESTINCLUDE=${DESTINCLUDE} DESTLIB=${DESTLIB} \
	DESTMAN=${DESTMAN} TITROFFBINARY=${TITROFFBINARY}
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
# order is important on the next line!
SUBDIR=	src conf.troff 
INSTALLDIR= include bin lib man/man1
CLEANDIR= src conf.troff bin lib


all:
	for i in ${SUBDIR}; do \
		(cd $$i; make install); done

# install assumes that a previous "make all" has been done

install:
	for i in ${INSTALLDIR}; do \
		(cd $$i; make ${MFLAGS} install); done

clean:
	rm -f a.out core *.s *.o
	for i in ${CLEANDIR}; do \
		(cd $$i; make ${MFLAGS} clean); done
	@echo "clean-up complete"
