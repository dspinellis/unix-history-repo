# make all	to compile and build Emacs
# make install	to install it
# make install.sysv  to install on system V.
# make install.xenix  to install on Xenix
# make tags	to update tags tables
#
# make distclean	to delete everything that wasn't in the distribution
#	This is a very dangerous thing to do!
# make clean
#       This is a little less dangerous.

SHELL = /bin/sh

# Where to install things
# Note that on system V you must change MANDIR to /use/local/man/man1.
LIBDIR= /usr/contrib/lib/emacs
BINDIR= /usr/contrib/bin
MANDIR= /usr/contrib/man/cat1
LOCKDIR= /var/emacs/lock

# Flags passed down to subdirectory makefiles.
MFLAGS=

# Subdirectories to make recursively.  `lisp' is not included
# because the compiled lisp files are part of the distribution
# and you cannot remake them without installing Emacs first.
SUBDIR= etc src

# Subdirectories to install
COPYDIR= etc info lisp

# Subdirectories to clean
CLEANDIR= ${COPYDIR} lisp/term

all:	src/paths.h ${SUBDIR}

src/paths.h: Makefile src/paths.h-dist
	/usr/bin/sed -e 's;/usr/local/emacs;${LIBDIR};g' \
		-e 's;${LIBDIR}/lock;${LOCKDIR};g' \
		< src/paths.h-dist > src/paths.h

src:	etc

.RECURSIVE: ${SUBDIR}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS} all

install: all mkdir lockdir
	-if [ `/bin/pwd` != `(cd ${LIBDIR}; /bin/pwd)` ] ; then \
		tar cf - ${COPYDIR} | (cd ${LIBDIR}; umask 0; tar xBf - ) ;\
		for i in ${CLEANDIR}; do \
			(rm -rf ${LIBDIR}/$$i/RCS; \
			 rm -f ${LIBDIR}/$$i/\#*; \
			 rm -f ${LIBDIR}/$$i/*~); \
		done \
	else true; \
	fi
	install -c -s etc/emacsclient ${BINDIR}/emacsclient
	install -c -s etc/etags ${BINDIR}/etags
	install -c -s etc/ctags ${BINDIR}/ctags
	install -c -s -m 1755 src/xemacs ${BINDIR}/xemacs
	nroff -man etc/emacs.1 > ${MANDIR}/emacs.0
	chmod 444 ${MANDIR}/emacs.0
	-rm -f ${BINDIR}/emacs
	mv ${BINDIR}/xemacs ${BINDIR}/emacs

install.sysv: all mkdir lockdir
	-if [ `/bin/pwd` != `(cd ${LIBDIR}; /bin/pwd)` ] ; then \
		find ${COPYDIR} -print | cpio -pdum ${LIBDIR} ;\
		for i in ${CLEANDIR}; do \
			(rm -rf ${LIBDIR}/$$i/RCS; \
			 rm -f ${LIBDIR}/$$i/\#*; \
			 rm -f ${LIBDIR}/$$i/*~); \
		done \
	else true; \
	fi
	-cpset etc/emacsclient ${BINDIR}/emacsclient 755 bin bin
	-cpset etc/etags ${BINDIR}/etags 755 bin bin
	-cpset etc/ctags ${BINDIR}/ctags 755 bin bin
	-cpset etc/emacs.1 ${MANDIR}/emacs.1 444 bin bin
	-/bin/rm -f ${BINDIR}/emacs
	-cpset src/xemacs ${BINDIR}/emacs 1755 bin bin

install.xenix: all mkdir lockdir
	if [ `pwd` != `(cd ${LIBDIR}; pwd)` ] ; then \
		tar cf - ${COPYDIR} | (cd ${LIBDIR}; umask 0; tar xpf - ) ;\
		for i in ${CLEANDIR}; do \
			(rm -rf ${LIBDIR}/$$i/RCS; \
			 rm -f ${LIBDIR}/$$i/\#*; \
			 rm -f ${LIBDIR}/$$i/*~); \
		done \
	else true; \
	fi
	cp etc/etags etc/ctags etc/emacsclient ${BINDIR}
	chmod 755 ${BINDIR}/etags ${BINDIR}/ctags ${BINDIR}/emacsclient
	cp etc/emacs.1 ${MANDIR}/emacs.1
	chmod 444 ${MANDIR}/emacs.1
	-mv -f ${BINDIR}/emacs ${BINDIR}/emacs.old
	cp src/xemacs ${BINDIR}/emacs
	chmod 1755 ${BINDIR}/emacs
	-rm -f ${BINDIR}/emacs.old

mkdir: FRC
	-mkdir ${LIBDIR}
	-chmod 777 ${LIBDIR}

distclean:
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} distclean); done

clean:
	cd src; make clean
	if [ `pwd` != `(cd ${LIBDIR}; pwd)` ] ; then \
		cd etc; make clean; \
	else true; \
	fi

lockdir:
	-mkdir ${LOCKDIR}
	-chmod 777 ${LOCKDIR}

FRC:

tags:	etc
	cd src; ../etc/etags *.[ch] ../lisp/*.el ../lisp/term/*.el
