# Generated automatically from Makefile.in by configure.
# @(#) $Header: /home/daffy/u0/vern/flex/RCS/Makefile.in,v 1.44 95/04/21 11:53:05 vern Exp $ (LBL)



# Possible values for DEFS:
#
# By default, flex generates 8-bit scanners when using table compression,
# and 7-bit scanners when using uncompressed tables (-f or -F options).
# For flex to always generate 8-bit scanners, add "-DDEFAULT_CSIZE=256"
# to DEFS.
#
# For Vax/VMS, add "-DVMS" to DEFS.
#
# For MS-DOS, add "-DMS_DOS" to DEFS.  See the directory MISC/MSDOS for
# additional info.

CFLAGS = -g -O
CPPFLAGS = 
DEFS = -DHAVE_CONFIG_H
LDFLAGS = 
LIBS = 

# Installation targeting.  Files will be installed under the tree
# rooted at prefix.  flex will be installed in bindir, libfl.a in
# libdir, FlexLexer.h will be installed in includedir, and the manual
# pages will be installed in mandir with extension manext.
# 
# Raw, unformatted troff source will be installed if INSTALLMAN=man,
# nroff preformatted versions will be installed if INSTALLMAN=cat.

prefix = /usr/local
exec_prefix = ${prefix}
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib
includedir = $(prefix)/include
manext = 1
mandir = $(prefix)/man/man$(manext)

# You can define these to be "lex" and "libl.a" if you want to replace
# lex at your site.
FLEX = lex
FLEXLIB = libfl.a

INSTALLMAN = man

SHELL = /bin/sh
srcdir = .

LN_S = ln -s
YACC = yacc
CC = gcc
AR = ar
RANLIB = ranlib
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_PROGRAM = ${INSTALL}

# You normally do not need to modify anything below this point.
# ------------------------------------------------------------

CPPFLAGS = -I. -I$(srcdir)

.c.o:
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $<

HEADERS = flexdef.h version.h

SOURCES = ccl.c dfa.c ecs.c gen.c main.c misc.c nfa.c parse.y \
	scan.l skel.c sym.c tblcmp.c yylex.c
OBJECTS = ccl.o dfa.o ecs.o gen.o main.o misc.o nfa.o parse.o \
	scan.o skel.o sym.o tblcmp.o yylex.o 

LIBSRCS = libmain.c libyywrap.c
LIBOBJS = libmain.o libyywrap.o

LINTSRCS = ccl.c dfa.c ecs.c gen.c main.c misc.c nfa.c parse.c \
	scan.c skel.c sym.c tblcmp.c yylex.c

DISTFILES = README NEWS COPYING INSTALL FlexLexer.h \
	configure.in conf.in Makefile.in mkskel.sh flex.skl \
	$(HEADERS) $(SOURCES) $(LIBSRCS) MISC \
	flex.1 scan.c install.sh mkinstalldirs configure

DIST_NAME = flex

# which "flex" to use to generate scan.c from scan.l
FLEX_EXEC = ./$(FLEX)
FLEX_FLAGS = -t $(PERF_REPORT)
COMPRESSION =
PERF_REPORT = -p


all: $(FLEX)

$(FLEX): .bootstrap $(OBJECTS) $(FLEXLIB)
	$(CC) $(CFLAGS) -o $(FLEX) $(LDFLAGS) $(OBJECTS) $(FLEXLIB) $(LIBS)

.bootstrap: initscan.c
	@rm -f scan.c
	cp $(srcdir)/initscan.c scan.c
	touch .bootstrap

parse.c: parse.y
	$(YACC) -d $(srcdir)/parse.y
	@sed '/extern char.*malloc/d' <y.tab.c >parse.tmp
	@mv parse.tmp parse.c
	@mv y.tab.h parse.h
	@rm -f y.tab.c

parse.h: parse.c

scan.c: scan.l
	$(FLEX_EXEC) $(FLEX_FLAGS) $(COMPRESSION) $(srcdir)/scan.l >scan.c
	@sed s,\"$(srcdir)/scan.l\",\"scan.l\", <scan.c >scan.tmp
	@mv scan.tmp scan.c

scan.o: scan.c parse.h flexdef.h config.h
yylex.o: yylex.c parse.h flexdef.h config.h

skel.c: flex.skl mkskel.sh
	$(SHELL) $(srcdir)/mkskel.sh $(srcdir)/flex.skl >skel.c

main.o: main.c flexdef.h config.h version.h
ccl.o: ccl.c flexdef.h config.h
dfa.o: dfa.c flexdef.h config.h
ecs.o: ecs.c flexdef.h config.h
gen.o: gen.c flexdef.h config.h
misc.o: misc.c flexdef.h config.h
nfa.o: nfa.c flexdef.h config.h
parse.o: parse.c flexdef.h config.h
skel.o: skel.c flexdef.h config.h
sym.o: sym.c flexdef.h config.h
tblcmp.o: tblcmp.c flexdef.h config.h

alloca.o: alloca.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -Dxmalloc=yy_flex_xmalloc alloca.c

alloca.c: $(srcdir)/MISC/alloca.c
	@rm -f alloca.c
	cp $(srcdir)/MISC/alloca.c .

test: check
check: $(FLEX)
	$(FLEX_EXEC) $(FLEX_FLAGS) $(COMPRESSION) $(srcdir)/scan.l \
	| sed s,\"$(srcdir)/scan.l\",\"scan.l\", \
	| diff scan.c -
	@echo "Check successful, using COMPRESSION=\"$(COMPRESSION)\""

bigcheck:
	rm -f scan.c ; $(MAKE) COMPRESSION="-C" check
	rm -f scan.c ; $(MAKE) COMPRESSION="-Ce" check
	rm -f scan.c ; $(MAKE) COMPRESSION="-Cm" check
	rm -f scan.c ; $(MAKE) COMPRESSION="-f" check
	rm -f scan.c ; $(MAKE) COMPRESSION="-Cfea" check
	rm -f scan.c ; $(MAKE) COMPRESSION="-CFer" check
	rm -f scan.c ; $(MAKE) COMPRESSION="-l" PERF_REPORT="" check
	rm -f scan.c ; $(MAKE)
	@echo "All checks successful"

$(FLEXLIB): $(LIBOBJS)
	$(AR) cru $(FLEXLIB) $(LIBOBJS)
	-$(RANLIB) $(FLEXLIB)

$(FLEX).man: flex.1
	cd $(srcdir) && nroff -man flex.1 >$(FLEX).man

install: $(FLEX) $(FLEXLIB) installdirs install.$(INSTALLMAN)
	$(INSTALL_PROGRAM) $(FLEX) $(bindir)/$(FLEX)
	@rm -f $(bindir)/$(FLEX)++
	cd $(bindir) && $(LN_S) $(FLEX) $(FLEX)++
	$(INSTALL_DATA) $(FLEXLIB) $(libdir)/$(FLEXLIB)
	-cd $(libdir) && $(RANLIB) $(FLEXLIB)
	$(INSTALL_DATA) $(srcdir)/FlexLexer.h $(includedir)/FlexLexer.h

# Note, the following rules delete any vestigial flexdoc installed
# for a prior flex release.
install.man: flex.1
	rm -f $(mandir)/$(FLEX)doc.$(manext)
	$(INSTALL_DATA) $(srcdir)/flex.1 $(mandir)/$(FLEX).$(manext)

install.cat: $(FLEX).man
	rm -f $(mandir)/$(FLEX)doc.$(manext)
	$(INSTALL_DATA) $(srcdir)/$(FLEX).man $(mandir)/$(FLEX).$(manext)

installdirs:
	$(SHELL) $(srcdir)/mkinstalldirs \
	  $(bindir) $(libdir) $(includedir) $(mandir)

uninstall:
	rm -f $(bindir)/$(FLEX) $(bindir)/$(FLEX)++
	rm -f $(libdir)/$(FLEXLIB)
	rm -f $(includedir)/FlexLexer.h
	rm -f $(mandir)/$(FLEX).$(manext) $(mandir)/$(FLEX)doc.$(manext)

tags: $(SOURCES)
	ctags $(SOURCES)

TAGS: $(SOURCES)
	etags $(SOURCES)

lint: $(LINTSRCS)
	lint -Dconst= $(LINTSRCS) > flex.lint

gcc-lint: $(LINTSRCS)
	gcc -Dlint -Wall $(LINTSRCS) >flex.gcc-lint 2>&1

mostlyclean:
	rm -f a.out *.bak core errs scan.tmp

clean: mostlyclean
	rm -f flex parse.c parse.h *.o alloca.c *.lint lex.yy.c lex.yy.cc \
		$(FLEXLIB) config.log config.cache

distclean: clean
	rm -f .bootstrap $(FLEX) scan.c tags TAGS Makefile config.status \
		config.h

maintainer-clean: distclean
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."
	rm -f $(FLEX).man skel.c flex*.tar.gz flex*.tar.Z

dist: $(FLEX) $(DISTFILES) parse.c parse.h $(srcdir)/$(FLEX).man
	$(MAKE) DIST_NAME=flex-`sed <version.h 's/[^"]*"//' | sed 's/"//'` dist2

dist2:
	@rm -rf $(DIST_NAME)
	@rm -f $(DIST_NAME).tar $(DIST_NAME).tar.Z $(DIST_NAME).tar.gz
	@mkdir $(DIST_NAME)
	tar cf - $(DISTFILES) | (cd $(DIST_NAME) && tar xfB -)
	@mv $(DIST_NAME)/scan.c $(DIST_NAME)/initscan.c
	@chmod 444 $(DIST_NAME)/initscan.c
	@chmod +w $(DIST_NAME)/Makefile.in
	@cp parse.c parse.h $(DIST_NAME)/MISC
	@col -b <$(srcdir)/$(FLEX).man >$(DIST_NAME)/MISC/flex.man
	tar chf $(DIST_NAME).tar $(DIST_NAME)
	compress <$(DIST_NAME).tar >$(DIST_NAME).tar.Z
	gzip <$(DIST_NAME).tar >$(DIST_NAME).tar.gz
	@rm $(DIST_NAME).tar

# For an explanation of the following Makefile rules, see node
# `Automatic Remaking' in GNU Autoconf documentation.
Makefile: $(srcdir)/Makefile.in config.status
	CONFIG_FILES=$@ CONFIG_HEADERS= ./config.status
config.status: configure
	./config.status --recheck
configure: configure.in
	cd $(srcdir) && autoconf
config.h: stamp-h
stamp-h: conf.in config.status
	CONFIG_FILES= CONFIG_HEADERS=config.h:conf.in ./config.status
	echo timestamp >stamp-h
# conf.in: stamp-h.in
# stamp-h.in: configure.in acconfig.h
# 	cd $(srcdir) && autoheader
# 	config.h.in conf.in
# 	cho timestamp > $(srcdir)/stamp-h.in

# Tell versions [3.59,3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
