#
#	Top level Makefile for the University of Illinois Notesfile
#	package. This Makefile manages compilation of the entire
#	package, updating the distribution copy, and not a lot else.
#	It isn't particularly good for installing the code even though
#	it has a case to do just that.
#
#	Define the Source files
#
SRC=	Makefile Answer.this README HINTS
SUBDIR=	doc man src utility

#
#	Some directories and commands
#
DCP=		/usr/ucb/rcp
DISTHOST=	uiucdcsp
DDIR=		/usr/src/uiuc/notesfiles
DISTDIR=	${DISTHOST}:${DDIR}
UUCPHOST=	uiucdcs
FTPHOST=	uiucdcs

#	The nice normal rules that everyone pays attention to.

all:	$(SUBDIR)
	for i in ${SUBDIR}; do (cd $$i; echo $$i; make ${MFLAGS}); done;

install:
	for i in ${SUBDIR}; do (cd $$i; echo $$i; make ${MFLAGS} install); done;

clean:
	for i in ${SUBDIR}; do (cd $$i; echo $$i; make clean); done;

${SUBDIR}:	FRC

FRC:

#
#	Here come the biggies;
#
#	Revision management

freeze:	$(SRC)
	ci -r1.6.1 $?
	co -r1.6.1 $?
	chmod ug+w $?
	touch freeze

#
#	Send updates to the distribution machine
#

dist:	ldist subdist
	/usr/ucb/rsh ${DISTHOST} -n "cd ${DDIR}; make MANIFEST"
	touch dist

subdist:
	for i in $(SUBDIR); do \
		(cd $$i;echo $$i; make ${MFLAGS} DCP=${DCP} DISTDIR=${DISTDIR} dist); done

ldist:	$(SRC)
	$(DCP) $? $(DISTDIR)/.
	touch ldist

MANIFEST: FRC
	find . -type f -print | sort > MANIFEST

FRC:

ftp uucp: MANIFEST
	@echo "Checking if you are on the right machine...."
	@if [ `hostname` = ${DISTHOST} ]; then \
		echo "Ok, you are on the right machine"; true; \
	else \
		echo "Must be done from ${DISTHOST}"; false; \
	fi
	tar rcf /tmp/Notes.tar .
	@echo "Sizes"
	@du .
	@echo "Making a Compressed Copy"
	compress < /tmp/Notes.tar > /tmp/Notes.tar.Z
	@ls -l /tmp/Notes.tar /tmp/Notes.tar.Z
	@echo "Copy to the UUCP host"
	@echo "The uucpable copies are symlinked to the FTP'able copies..."
#	rcp /tmp/Notes.tar ${UUCPHOST}:/usr/spool/uucppublic/Notes.tar
#	rcp /tmp/Notes.tar.Z ${UUCPHOST}:/usr/spool/uucppublic/Notes.tar.Z
	rsh ${UUCPHOST} -n ls -l /usr/spool/uucppublic/Notes.tar /usr/spool/uucppublic/Notes.tar.Z
	@echo "Copy to the FTP host"
	rcp /tmp/Notes.tar "${FTPHOST}:~ftp/pub/Notes.tar"
	rcp /tmp/Notes.tar.Z "${FTPHOST}:~ftp/pub/Notes.tar.Z"
	rsh ${FTPHOST} -n "ls -l ~ftp/pub/Notes.tar ~ftp/pub/Notes.tar.Z"
	@echo "Unpack for people to get individual files"
	rsh ${FTPHOST} -n "cd ~ftp/pub/notes; tar xfp ~ftp/pub/Notes.tar"
	rm -f /tmp/Notes.tar /tmp/Notes.tar.Z

