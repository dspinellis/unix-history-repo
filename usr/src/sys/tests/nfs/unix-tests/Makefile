#
#       @(#)Makefile	1.5 90/01/10 NFS Rev 2 Testsuite
#
# 'make all'			makes test programs
# 'make clean'			cleans directories
# 'make copy DESTDIR=path'	copies test programs to path
# 'make dist DESTDIR=path'	copies sources to path

MAKE=make
DESTDIR=/no/such/path
COPYFILES=runtests tests.init server domount.c README Testitems getopt.c

# -----------------------------------------------
#  Defines for various variables used in this and
#  other Makefiles.

#  Define NFS3_2 for Sun NFS 3.2 compatibility
# COMPAT = -DNFS3_2

# Use with Sys V Release 3 systems.
# SYSTYPE=-DSVR3
# LIBS=-lrpc
# COMPAT = -DSVR3 

# Use with BSD systems.
SYSTYPE=
LIBS=
COMPAT=
# -----------------------------------------------

CFLAGS= ${SYSTYPE} -O

all: domount getopt
	cd basic; $(MAKE) SYSTYPE="$(SYSTYPE)" LIBS="$(LIBS)" COMPAT="$(COMPAT)"
	cd general; $(MAKE) SYSTYPE="$(SYSTYPE)" LIBS="$(LIBS)" COMPAT="$(COMPAT)"
	cd special; $(MAKE) SYSTYPE="$(SYSTYPE)" LIBS="$(LIBS)" COMPAT="$(COMPAT)"

domount: domount.c
	$(CC) $(CFLAGS) -o $@ $@.c
	chown root domount
	chmod u+s domount

getopt: getopt.c
	$(CC) $(CFLAGS) -o $@ $@.c

clean:
	rm -f domount getopt
	cd basic; $(MAKE) clean
	cd general; $(MAKE) clean
	cd special; $(MAKE) clean
	cd tools; $(MAKE) clean;

copy: mknewdirs
	cp $(COPYFILES) $(DESTDIR)
	cd basic; $(MAKE) copy DESTDIR=$(DESTDIR)/basic
	cd general; $(MAKE) copy DESTDIR=$(DESTDIR)/general
	cd special; $(MAKE) copy DESTDIR=$(DESTDIR)/special
	cd tools; $(MAKE) copy DESTDIR=$(DESTDIR)/tools

dist: mknewdirs
	cp Makefile $(COPYFILES) $(DESTDIR)
	cd basic; $(MAKE) dist DESTDIR=$(DESTDIR)/basic
	cd general; $(MAKE) dist DESTDIR=$(DESTDIR)/general
	cd special; $(MAKE) dist DESTDIR=$(DESTDIR)/special
	cd tools; $(MAKE) dist DESTDIR=$(DESTDIR)/tools

mknewdirs:
	-mkdir $(DESTDIR)/basic $(DESTDIR)/general $(DESTDIR)/special \
	       $(DESTDIR)/tools
