#
# Copyright 1985, Massachusetts Institute of Technology.
#
# $Header: Makefile,v 10.25 86/02/13 16:20:15 jg Exp $
#
# Subdirectories that have makefiles of their own.
#
# This is the master makefile for the X window system.
# Please look at the document in doc/installation for installation
# directions.
#
# If you have RCP'd the directory tree, you have destroyed
# a number of symbolic links that are set up so that only a single
# copy of some files exist.  We recommend 'tar'ing the files
# to move them.
#
# The CONFDIR should be set to the location where you want executables to
# be placed.  By default, it will put stuff in /usr/new.
# If you change it here, you probably want to do a "make reconfig" to
# change the makefiles one level down.
# If you are on a stock 4.2 system, you may need to remove the
# "#include <sys/types.h>" from Xlib/Xlib.h for many programs to build.
# This is the last distribution which will have any verification
# on 4.2BSD build capability.  From here on out, 4.3 BSD will be presumed.
#
# The command
#	make xlibchange
# will remove the include line from Xlib/Xlib.h.
#
# On a Sun system, you should first
#	make sun		# to set things up for a Sun workstation.
#				# also does 'make xlibchange'
#
# The basic procedure for 4.3 should be:
#	make all		# build the binaries (as yourself)
#	make install		# install the executables (as root)
# You will also have to build a kernel with the Vs100 driver in it,
# and add a line to /etc/ttys for each display (See the X(8c) manual page)
# You must also rename a pseudo tty pair for each Display configured.
# For example: mv /dev/ttyqf /dev/ttyv0; mv /dev/ptyqf /dev/ptyv0
# Make sure to make as many pty's as possible.
#
# If you want DECnet support, you must define the symbol DNETCONN in
# the X, Xlib, and xhost makefiles.  By default, only TCP and Unix domain
# are built.
#
#					Jim Gettys 
#
# 
CONFDIR= /usr/new
# XDIR is where the fonts and firmware will be installed.
# if you change XDIR, you must change libvs100/vssite.h to match.
XDIR= /lib/X
# The version should be increased when the protocol changes
# by convention, stuff only available in binary form goes in exe.v{XVERSION}
XVERSION= 10

SUBDIR= rgb inline Xlib XMenu libnest libvs100 libsun X bitmap man keycomp\
	pikapix uwm xterm xclock xcons \
	xdvi xfd xgedit xhost ximpv xinit xload xnwm xpr xrefresh xset\
	xshell xsetroot xwininfo xwd xwud xwm
all:
	for i in ${SUBDIR}; do \
	    (cd $$i; echo "compiling $$i";\
	     make ${MFLAGS} DESTDIR=${DESTDIR} CONFDIR=${CONFDIR} all;\
	     cd ..);\
	done

# the following is for use when the X library changes
relink:
	for i in ${SUBDIR}; do \
	    rm -f $$i/$$i; \
	done
	make all

# we install the CLU demo programs since most people don't have CLU compilers
install:
	echo "Moving X library directory to .old"
	-mv -f ${DESTDIR}${CONFDIR}${XDIR} ${DESTDIR}${CONFDIR}${XDIR}.old
	echo "Make sure the installation directories exist..."
	-mkdir ${DESTDIR}${CONFDIR}
	-mkdir ${DESTDIR}${CONFDIR}/lib
	-mkdir ${DESTDIR}${CONFDIR}${XDIR}
	make inc
	for i in ${SUBDIR}; do \
	    (cd $$i;  echo "installing $$i";\
	     make ${MFLAGS} DESTDIR=${DESTDIR} CONFDIR=${CONFDIR} install;\
	     cd ..); \
	done
	(cd exe.v${XVERSION}; install -c xfax  ${DESTDIR}${CONFDIR};\
		install -c xdemo ${DESTDIR}${CONFDIR};\
		install -c xted  ${DESTDIR}${CONFDIR}; cd ..)
	echo "Copying fonts."
	cp -r font ${DESTDIR}${CONFDIR}${XDIR}
	cp -r s-code ${DESTDIR}${CONFDIR}${XDIR}

inc:
	echo "Removing /usr/include/X.old if any exists....."
	rm -rf ${DESTDIR}/usr/include/X.old
	echo "/usr/include/X being moved to /usr/include/X.old"
	-mv -f ${DESTDIR}/usr/include/X ${DESTDIR}/usr/include/X.old
	mkdir ${DESTDIR}/usr/include/X
	for i in X Xlib XMenu; do \
	    (cd $$i;  echo "copying include files $$i";\
	     make ${MFLAGS} DESTDIR=${DESTDIR} CONFDIR=${CONFDIR} include;\
	     cd ..); \
	done

clean:
	rm -f a.out core *.o errs ERRS \#*
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean; cd ..); done
	-rm -rf ${DESTDIR}${CONFDIR}${XDIR}.old ${DESTDIR}/usr/include/X.old


# useful entry if you decide to change your configuration for binaries.
reconfig:
	for i in ${SUBDIR}; do \
	   (umask 222; cd $$i; \
	   sed -e "s[/usr/new[${CONFDIR}[" <Makefile >Makefile.new;\
	   mv -f Makefile.new Makefile; cd .. ); \
	done


# move bin directory into ${CONFDIR}
berkeleydist:	clean
	rm -rf xperfmon test
	rm -rf man/xperfmon.1
	rm -f xted/*.bin xdemo/*.bin xfax/*.bin
	rm -rf RCS */RCS */*/RCS */*/*/RCS
	rm -rf maint

mitdist:
	rm -rf xperfmon test
	rm -rf man/xperfmon.1
	rm -rf RCS */RCS */*/RCS */*/*/RCS
	rm -rf maint

# fixup Xlib.h to get rid of <sys/types.h> for Sun 2.0 and before.
# replace VAX demo executables with Sun executables.
sun:
	make xlibchange
	for i in exe.v${XVERSION}/*.sun;  do \
		-mv $$i exe.v${XVERSION}/`basename $$i .sun` ; \
	done

xlibchange:
	fgrep -v '#include <sys/types.h>' Xlib/Xlib.h >/tmp/nxlib
	mv -f /tmp/nxlib Xlib/Xlib.h
