
#
# Make Unix XNS/Courier package
# $Header: Makefile,v 1.5 87/05/11 10:28:34 ed Exp $
#
#  You might want to change the following defines:
#	CHDEFAULT	default domain and organization for local system
#	DESTDIR		highest level intended directory (default /)
#	DESTBIN		where the executables go
#	DESTINCLUDE	where the ".h" files go
#	DESTLIB		where the libraries go
#	DESTMAN		where the manual pages go
#	DESTCOURIER	where the Courier services go
#	DESTETC		where the "/etc" related files go

CHDEFAULT = :Computer Science:Cornell-Univ
DESTDIR =
DESTBIN = ${DESTDIR}/usr/new
DESTINCLUDE = ${DESTDIR}/usr/include/xnscourier
DESTLIB = ${DESTDIR}/usr/local/lib
DESTMAN = ${DESTDIR}/usr/man/mann
DESTCOURIER = ${DESTDIR}/usr/new/lib/xnscourier
DESTETC = ${DESTDIR/etc

#
# END of user tunable parameters
#--------------------------------------------------------------

#  targets:
#	all		- recompile and link everything
#	install		- copy necessary things to correct destination
#	clean		- undo "make all" to free disk space
#

# set up for recursive makes
MFLAGS = DESTDIR=${DESTDIR}  DESTBIN=${DESTBIN} DESTINCLUDE=${DESTINCLUDE} \
	DESTLIB=${DESTLIB} DESTMAN=${DESTMAN} DESTCOURIER=${DESTCOURIER} \
	DESTETC=${DESTETC} CHDEFAULT="${CHDEFAULT}"

# Programs that live in subdirectories and have their own makefiles
#
# MAKE the following in the order specified
SUBDIRS = xnslib compiler courierlib morexnslib daemon examples
INSTALLDIRS = include bin lib xnscourier man
ALLDIRS = ${SUBDIRS} ${INSTALLDIRS} doc

RCSNAME = current

# remake everything (many things assume previous subdirectories in the
# list have had a "make" done).
all:
	for Dir in ${SUBDIRS}; do \
		(cd $$Dir; make ${MFLAGS} install); done

# install assumes that a previous "make all"  has been done

install:
	for i in ${INSTALLDIRS}; do \
		(cd $$i; make ${MFLAGS} install); done

# clean frees disk space

clean:
	-rm *.BAK *.CKP
	for Dir in ${ALLDIRS}; do (cd $$Dir; make ${MFLAGS} clean); done

rcs:
	for Dir in ${ALLDIRS}; do \
		(cd $$Dir; make RCSNAME="${RCSNAME}" RCSREV="${RCSREV}" \
			${MFLAGS} rcs); done

# you should probably do a "make clean" and "make rcs" before this.
# also, edit rcsdescription
courier.tar: rcsdescription
	-chmod -r */RCS morexnslib/*/RCS examples/*/RCS
	tar cvf courier.tar Makefile README ${ALLDIRS}
	-chmod +r */RCS morexnslib/*/RCS examples/*/RCS
