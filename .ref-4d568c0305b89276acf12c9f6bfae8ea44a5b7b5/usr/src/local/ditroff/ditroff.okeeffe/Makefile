#######################################################################
#
#	@(#)Makefile	1.7	(CWI)	93/07/27
#
# Makefile for ditroff and nroff
#
#######################################################################

# utilities maintained by this makefile

UDIRS = troff utilities eqn pic ideal grap tbl term chem
DEVS= devhar devnver

# location to install final output

BINDIR = /usr/local/bin
LIBXDIR = /usr/local/libexec
MANDIR = /usr/local/man/cat1

all:	$(UDIRS)

install: all
	for i in $(UDIRS); do (cd $$i; make $(MFLAGS) DESTDIR=${DESTDIR} install); done

clean:  force
	for i in ${UDIRS}; do (cd $$i; make ${MFLAGS} clean); done

$(UDIRS): force
	cd $@; make $(MFLAGS)


# fake dependency to force a make
force:
