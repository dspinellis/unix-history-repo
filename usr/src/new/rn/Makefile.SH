case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting Makefile (with variable substitutions)"
cat >Makefile <<!GROK!THIS!
# $Header: Makefile.SH,v 4.3.1.2 85/05/13 17:21:18 lwall Exp $
#
# $Log:	Makefile.SH,v $
# Revision 4.3.1.2  85/05/13  17:21:18  lwall
# System V make needs space before line-continuing backslash.
# 
# Revision 4.3.1.1  85/05/10  11:30:15  lwall
# Branch for patches.
# 
# Revision 4.3  85/05/01  11:33:26  lwall
# Baseline for release with 4.3bsd.
# 

CC = $cc
rnbin = $rnbin
rnlib = $rnlib
mansrc = $mansrc
manext = $manext
CFLAGS = $iandd -O
LDFLAGS = $iandd
NDIRC = $ndirc
NDIRO = $ndiro

libs = $ndirlib $termlib $jobslib
!GROK!THIS!
cat >>Makefile <<'!NO!SUBS!'

public = rn newsetup newsgroups Pnews Rnmail
private = norm.saver mbox.saver ng.help art.help pager.help subs.help makedir filexp Pnews.header
manpages = rn.1 Pnews.1 Rnmail.1 newsetup.1 newsgroups.1
util = Makefile makedepend newsnews

h1 = addng.h art.h artio.h artsrch.h backpage.h bits.h cheat.h common.h
h2 = final.h head.h help.h init.h intrp.h kfile.h last.h ndir.h ng.h
h3 = ngdata.h ngsrch.h ngstuff.h only.h rcln.h rcstuff.h
h4 = respond.h rn.h search.h sw.h term.h util.h

h = $(h1) $(h2) $(h3) $(h4)

c1 = addng.c art.c artio.c artsrch.c backpage.c bits.c cheat.c
c2 = final.c head.c help.c init.c intrp.c kfile.c last.c $(NDIRC) ng.c
c3 = ngdata.c ngsrch.c ngstuff.c only.c rcln.c rcstuff.c
c4 = respond.c rn.c search.c sw.c term.c util.c

c = $(c1) $(c2) $(c3) $(c4)

obj1 = addng.o art.o artio.o artsrch.o backpage.o bits.o cheat.o
obj2 = final.o head.o help.o init.o intrp.o kfile.o last.o $(NDIRO) ng.o
obj3 = ngdata.o ngsrch.o ngstuff.o only.o rcln.o rcstuff.o
obj4 = respond.o rn.o search.o sw.o term.o util.o

obj = $(obj1) $(obj2) $(obj3) $(obj4)

lintflags = -phbvxac

add1 = Makefile.old Pnews Rnmail art.help
add2 = bsd config.h config.sh eunice filexp
add3 = loc makedepend makedir mbox.saver newsetup
add4 = newsgroups newsnews ng.help norm.saver pager.help
add5 = pdp11 rn subs.help usg v7

addedbyconf = $(add1) $(add2) $(add3) $(add4) $(add5)

# grrr
SHELL = /bin/sh

.c.o:
	$(CC) -c $(CFLAGS) $*.c

all: $(public) $(private) $(util)
	touch all

rn: $(obj)
	$(CC) $(LDFLAGS) $(obj) $(libs) -o rn

# if a .h file depends on another .h file...
$(h):
	touch $@

install: rn
# won't work with csh
	export PATH || exit 1
	- mv $(rnbin)/rn $(rnbin)/rn.old
	- if test `pwd` != $(rnbin); then cp $(public) $(rnbin); fi
	cd $(rnbin); chmod 755 $(public)
	chmod 755 makedir
	- ./makedir `./filexp $(rnlib)`
	- if test `pwd` != `./filexp $(rnlib)`; then cp INIT $(private) `./filexp $(rnlib)`; fi
	cd `./filexp $(rnlib)`; chmod 755 $(private)
	- if test ! -f `./filexp $(rnlib)/newsnews`; then cp newsnews `./filexp $(rnlib)`; fi
	- if test `pwd` != $(mansrc); then \
for page in $(manpages); do \
cp $$page $(mansrc)/`basename $$page .1`.$(manext); \
done; \
fi

clean:
	rm -f *.o

realclean:
	rm -f rn *.o core $(addedbyconf)

# The following lint has practically everything turned on.  Unfortunately,
# you have to wade through a lot of mumbo jumbo that can't be suppressed.
# If the source file has a /*NOSTRICT*/ somewhere, ignore the lint message
# for that spot.

lint:
	lint $(lintflags) $(defs) $(c) > rn.fuzz

depend:
	makedepend

# AUTOMATICALLY GENERATED MAKE DEPENDENCIES--PUT NOTHING BELOW THIS LINE
$(obj):
	@ echo "You haven't done a "'"make depend" yet!'; exit 1
!NO!SUBS!
$eunicefix Makefile
