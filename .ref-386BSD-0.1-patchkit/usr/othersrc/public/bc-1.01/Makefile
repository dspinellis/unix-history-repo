#
# A makefile for bc.  This is part of the bc/sbc distribution.
#
#
#  Make sure these have the correct directories for your machine.
#
#  LIBDIR and BINDIR are where bc and libmath.b will be put.
#
LIBDIR = /usr/local/lib
BINDIR = /usr/local/bin
#
#  INCLUDE is the directory from where header files are included.
#
INCLUDE = /usr/include
#
# This is the name of the library file, if needed.  This definition should
# not be deleted.
#
LIBFILE = $(LIBDIR)/libmath.b
#
# Programs definitions for use by make.
#
SHELL = /bin/sh
YACC = yacc
LEX = lex -I
#LEX = lex
CC = cc
UUENCODE = uue
MAKE = make
#
#  The following are the standard definitions.  For MINIX PC, you need to
#  comment these out and select one of the MINIX PC definition sets.
#  Other configuration defines are generated automatically.  See the 
#  README file for more details.
#
O=o
CFLAGS = -O -D_POSIX_SOURCE
LDFLAGS = 
#
# For the PC version of MINIX (K&R compiler), use the following lines.
# 
#O=s
#CFLAGS = -O -D_POSIX_SOURCE
#LDFLAGS = -i
#
# For the PC version of MINIX (ANSI compiler), use the following lines:
# 
#O=o
#CFLAGS = -m -D_POSIX_SOURCE
#LDFLAGS = -i
#
#
OFILES = scan.$O util.$O main.$O number.$O storage.$O load.$O execute.$O 
#
DISTFILES = COPYING Makefile Install bc.y bcdefs.h const.h version.h \
            execute.c global.c global.h load.c main.c number.c storage.c \
	    number.h proto.h scan.l util.c vfprintf.c README bc.1 sbc.y \
	    fix_math.h math.h libmath.b configure ChangeLog 
#
SUBDIRS = Examples Test
#
EXTRAFILES = bc.c.dist scan.c.dist y.tab.h.dist
#


all: bc
bc: config.h bc.$O $(OFILES) global.$O
	$(CC) -o bc $(LDFLAGS) bc.$O $(OFILES) global.$O

sbc: sbc.$O $(OFILES) global.$O
	$(CC) -o sbc $(LDFLAGS) sbc.$O $(OFILES) global.$O

config.h:
	./configure $(INCLUDE) $(LIBFILE) $(CC)

#math.h: libmath.b
#	$(MAKE) fbc
#	./fbc -c libmath.b </dev/null >math.h
#	./fix_math.h
#	rm -f ./fbc

#fbc: $(OFILES) bc.$O
#	echo \"\" > math.h
#	$(CC) -c $(CFLAGS) global.c
#	$(CC) -o fbc $(LDFLAGS) bc.$O $(OFILES) global.$O

install : bc libmath.b config.h
	rm -f $(BINDIR)/bc
	cp bc $(BINDIR)
	chmod 555 $(BINDIR)/bc
	if grep -s BC_MATH_FILE config.h; then rm -f $(LIBDIR)/libmath.b; \
	cp libmath.b $(LIBDIR); chmod 444 $(LIBDIR)/libmath.b; else true; fi

dist: $(EXTRAFILES)
	OF=`sed -n 's/.*\([0-9][0-9]*\.[0-9][0-9]*\).*/bc-\1/p' version.h` \
	; rm -rf $$OF \
	; mkdir $$OF \
	; cd $$OF \
	; for file in $(DISTFILES) $(EXTRAFILES); do ln ../$$file; done \
	; mkdir $(SUBDIRS) \
	; for dir in $(SUBDIRS); do cp ../$$dir/* $$dir; done \
	; cd .. \
	; tar cf $$OF.tar $$OF \
	; compress $$OF.tar \
	; rm -rf $$OF

shars: dist
	OF=`sed -n 's/.*\([0-9][0-9]*\.[0-9][0-9]*\).*/bc-\1/p' version.h` \
	; $(UUENCODE) $$OF.tar.Z - > $$OF.uue \
	; split -775 $$OF.uue $$OF-u. \
	; for file in $$OF-u.a?; do shar $$file > $$file.sh; done

minixdist: $(EXTRAFILES)
	OF=`sed -n 's/.*\([0-9][0-9]*\.[0-9][0-9]*\).*/bc-\1/p' version.h` \
	; rm -rf $$OF \
	; mkdir $$OF \
	; cd $$OF \
	; for file in $(DISTFILES) $(EXTRAFILES); do ln ../$$file; done \
	; cd .. \
	; tar cf $$OF.tar $$OF \
	; compress -b13 $$OF.tar \
	; rm -rf $$OF

minixshars: minixdist
	OF=`sed -n 's/.*\([0-9][0-9]*\.[0-9][0-9]*\).*/bc-\1/p' version.h` \
	; $(UUENCODE) $$OF.tar.Z - > $$OF.uue \
	; split -775 $$OF.uue $$OF-u. \
	; for file in $$OF-u.a?; do shar $$file > $$file.sh; done

csrshars: $(EXTRAFILES)
	findsrc -dy $(DISTFILES) $(EXTRAFILES) $(SUBDIRS) | \
	makekit -npart -oMANIFEST

bc.c.dist: bc.c
	cp bc.c bc.c.dist

scan.c.dist: scan.c
	cp scan.c scan.c.dist

y.tab.h.dist: y.tab.h
	cp y.tab.h y.tab.h.dist

extra:  $(EXTRAFILES)

# assumes that the extra files exist!
derived:
	cp bc.c.dist bc.c
	cp scan.c.dist scan.c
	cp y.tab.h.dist y.tab.h

clean:
	rm -f *.$O core ERRS *~ *.bak *.Z 

distclean: clean
	rm -f scan.c y.tab.h bc.c sbc.c bc sbc bc-dist* config.h  bc-*.* \
	part?? MANIFEST*

realclean: distclean
	rm -rf *.dist

scan.c: scan.l
	$(LEX) scan.l
	mv lex.yy.c scan.c

y.tab.h bc.c: bc.y
	@echo "expect 1 shift/reduce conflict"
	$(YACC) -d bc.y
	mv y.tab.c bc.c

sbc.c: sbc.y
	$(YACC) -d sbc.y
	mv y.tab.c sbc.c

global.$O: bcdefs.h global.h math.h
bc.$O:	bcdefs.h global.h 
execute.$O: bcdefs.h global.h
load.$O: bcdefs.h global.h 
main.$O: bcdefs.h global.h version.h
number.$O: bcdefs.h Makefile
sbc.$O: bcdefs.h global.h 
scan.$O: y.tab.h bcdefs.h global.h
storage.$O: bcdefs.h global.h
util.$O: bcdefs.h global.h version.h

bcdefs.h: number.h const.h config.h
	touch bcdefs.h
