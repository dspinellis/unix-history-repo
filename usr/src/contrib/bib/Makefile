#	@(#)Makefile	2.3 (Berkeley) %G%
#
# all executables go in DEST_BIN
# all man pages go in DEST_MAN
# the bib libraries go in DEST_LIB

# At UCB, the file structures have diverged, resulting in the following
# choice:

# UNIX on VAX machines:
DEST_BASE=/usr/contrib
DEST_BIN=$(DEST_BASE)
DEST_MAN=/usr/man

# UNIX on Suns, etc
#DEST_BASE=/usr/local
#DEST_BIN=$(DEST_BASE)/bin
#DEST_MAN=$(DEST_BASE)/man

# the software expects the following to be true
DEST_LIB=$(DEST_BASE)/lib

DESTDIRS=DEST_BASE=$(DEST_BASE) DEST_BIN=$(DEST_BIN) \
	 DEST_MAN=$(DEST_MAN)   DEST_LIB=$(DEST_LIB)

all:
	cd src; make ${MFLAGS} $(DESTDIRS)

install: all installsrc installmacs installman

installman:
	cd man; make ${MFLAGS} $(DESTDIRS) install

installsrc:
	cd src; make ${MFLAGS} $(DESTDIRS) install

installmacs:
	cd macs; make ${MFLAGS} $(DESTDIRS) install

clean:
	cd src; make ${MFLAGS} $(DESTDIRS) clean
