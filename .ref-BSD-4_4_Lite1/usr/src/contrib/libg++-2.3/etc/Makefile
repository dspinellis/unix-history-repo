# This file was generated automatically by configure.  Do not edit.
VPATH = .
links = 
host_alias = m68k-hp-bsd
host_cpu = m68k
host_vendor = hp
host_os = bsd
host_canonical = m68k-hp-bsd
target_alias = m68k-hp-bsd
target_cpu = m68k
target_vendor = hp
target_os = bsd
target_canonical = m68k-hp-bsd
ALL=all.internal
# 
# Makefile.in for etc
#

prefix = /usr/local
exec_prefix = $(prefix)

srcdir = .
bindir  = $(exec_prefix)/bin
libdir  = $(exec_prefix)/lib
tooldir = $(libdir)/m68k-hp-bsd
datadir = $(prefix)/lib

mandir  = $(prefix)/man
man1dir = $(mandir)/man1
man2dir = $(mandir)/man2
man3dir = $(mandir)/man3
man4dir = $(mandir)/man4
man5dir = $(mandir)/man5
man6dir = $(mandir)/man6
man7dir = $(mandir)/man7
man8dir = $(mandir)/man8
man9dir = $(mandir)/man9
infodir = $(prefix)/info

SHELL = /bin/sh

INSTALL 	= install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA    = $(INSTALL)

RM	= rm -f

MAKEINFO = makeinfo

#### Host, target, and site specific Makefile fragments come in here.
###

INFOFILES = configure.info intro.info standards.info cfg-paper.info

all:

install:  $(srcdir)/configure.man
	-parent=`echo $(man1dir)|sed -e 's@/[^/]*$$@@'`; \
	if [ -d $$parent ] ; then true ; else mkdir $$parent ; fi
	-if [ -d $(man1dir) ] ; then true ; else mkdir $(man1dir) ; fi
	$(INSTALL_DATA) $(srcdir)/configure.man $(man1dir)/configure.1

info: $(INFOFILES)

install-info:
	-parent=`echo $(infodir)|sed -e 's@/[^/]*$$@@'`; \
	if [ -d $$parent ] ; then true ; else mkdir $$parent ; fi
	-if [ -d $(infodir) ] ; then true ; else mkdir $(infodir) ; fi
	for i in *.info* ; do \
	  $(INSTALL) $$i $(infodir) ; \
	done

configure.info: configure.texi
	$(MAKEINFO) -o configure.info $(srcdir)/configure.texi

intro.info: intro.texi
	$(MAKEINFO) -o intro.info $(srcdir)/intro.texi

standards.info: standards.texi
	$(MAKEINFO) -o standards.info $(srcdir)/standards.texi

cfg-paper.info : cfg-paper.texi
	$(MAKEINFO) -o cfg-paper.info $(srcdir)/cfg-paper.texi


clean:
	$(RM) *.info *.info-*

distclean:   clean
mostlyclean: clean
realclean:   clean

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status

## these last targets are for standards.texi conformance
dist:
check:

TAGS:
