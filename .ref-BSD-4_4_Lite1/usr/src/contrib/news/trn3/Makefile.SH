case $CONFIG in
    '') . ./config.sh ;;
esac
echo "Extracting Makefile (with variable substitutions)"
$cat >Makefile <<!GROK!THIS!
# $Id: Makefile.SH,v 3.0 1992/03/01 02:13:32 davison Trn $
#
# This software is Copyright 1991 by Stan Barber. 
#
# Permission is hereby granted to copy, reproduce, redistribute or otherwise
# use this software as long as: there is no monetary profit gained
# specifically from the use or reproduction of this software, it is not
# sold, rented, traded or otherwise marketed, and this copyright notice is
# included prominently in any copy made. 
#
# The author make no claims as to the fitness or correctness of this software
# for any use whatsoever, and it is provided as is. Any use of this software
# is at the user's own risk. 
# 

CC = $cc
rnbin = $bin
rnibin = $installbin
rnlib = $privlib
rnilib = $installprivlib
mansrc = $mansrc
manext = $manext
CFLAGS = $optimize $ccflags
LDFLAGS = $ldflags
#NNTPNNTPFLAGS = $nntpflags
ndirc = $ndirc
ndiro = $ndiro

libs = $libndir $termlib $jobslib -lm $libs
!GROK!THIS!
$cat >>Makefile <<'!NO!SUBS!'
public = trn newsetup newsgroups Pnews Rnmail
private = norm.saver mbox.saver makedir Pnews.header Speller artcheck $(nntp)
util = makedepend newsnews
manpages = trn.1 Pnews.1 Rnmail.1 newsetup.1 newsgroups.1
#NNTPnntp=getactive
#NNTPnntpsrc=nntpinit.c nntpclient.c nntp.c
#NNTPnntpobj=nntpinit.o nntpclient.o nntp.o

h1 = addng.h art.h artio.h artsrch.h autosub.h backpage.h bits.h cache.h 
h2 = common.h decode.h final.h hash.h head.h help.h init.h intrp.h kfile.h
h3 = last.h ng.h ngdata.h ngsrch.h ngstuff.h only.h rcln.h rcstuff.h
h4 = respond.h rthread.h rt-mt.h rt-ov.h rt-page.h rt-process.h rt-select.h
h5 = rt-util.h rt-wumpus.h search.h sw.h term.h trn.h util.h

h = $(h1) $(h2) $(h3) $(h4) $(h5)

c1 = addng.c art.c artio.c artsrch.c autosub.c backpage.c bits.c cache.c
c2 = decode.c edit_dist.c final.c hash.c head.c help.c init.c intrp.c
c3 = kfile.c last.c $(ndirc) ng.c ngdata.c nghash.c ngsrch.c ngstuff.c
c4 = only.c rcln.c rcstuff.c respond.c rthread.c rt-mt.c rt-ov.c
c5 = rt-process.c rt-page.c rt-select.c rt-util.c rt-wumpus.c search.c
c6 = sw.c term.c trn.c util.c unship.c uudecode.c $(nntpsrc)

c = $(c1) $(c2) $(c3) $(c4) $(c5) $(c6)

obj1 = addng.o art.o artio.o artsrch.o autosub.o backpage.o bits.o cache.o
obj2 = decode.o edit_dist.o final.o hash.o head.o help.o init.o intrp.o
obj3 = kfile.o last.o $(ndiro) ng.o ngdata.o nghash.o ngsrch.o ngstuff.o
obj4 = only.o rcln.o rcstuff.o respond.o rthread.o rt-mt.o rt-ov.o
obj5 = rt-process.o rt-page.o rt-select.o rt-util.o rt-wumpus.o search.o
obj6 = sw.o term.o trn.o util.o unship.o uudecode.o parsedate.o $(nntpobj)

obj = $(obj1) $(obj2) $(obj3) $(obj4) $(obj5) $(obj6)

lintflags = -phbvxac

addedbymake = $(public) $(private) $(util) $(nntp) Makefile.old config.h\
	cppstdin all

# grrr
SHELL = /bin/sh

.c.o:
	$(CC) -c $(CFLAGS) $*.c

all: $(public) $(private) $(util) Makefile
	touch all

trn: $(obj)
	$(CC) $(LDFLAGS) $(obj) $(libs) -o trn

artcheck: artcheck.o
	$(CC) $(LDFLAGS) artcheck.o -o artcheck

#NNTP
#NNTPnntpinit.o:
#NNTP	$(CC) -c $(CFLAGS) $(NNTPFLAGS) $*.c
#NNTP
#NNTPgetactive: getactive.o nntpinit.o nntpclient.o
#NNTP	$(CC) $(LDFLAGS) getactive.o nntpinit.o nntpclient.o -o getactive $(libs)

parsedate.o: parsedate.y
	@echo 'Expect 6 shift/reduce conflicts'
	yacc parsedate.y
	mv -f y.tab.c parsedate.c
	$(CC) -c $(CFLAGS) parsedate.c
	rm -f parsedate.c

# if a .h file depends on another .h file...
$(h):
	touch $@

install: $(public) $(private) $(manpages)
# won't work with csh
	export PATH || exit 1
	- mv $(rnibin)/trn $(rnibin)/trn.old
	- if test `pwd` != $(rnibin); then cp $(public) $(rnibin); fi
	- ./makedir `./filexp $(rnilib)`
	- if test `pwd` != `./filexp $(rnilib)`; then cp INIT filexp $(private) `./filexp $(rnilib)`; fi
#NNTP	- if test `pwd` != `./filexp $(rnilib)`; then strip `./filexp $(rnilib)`/getactive ; fi
	- if test ! -f `./filexp $(rnilib)/newsnews`; then cp newsnews `./filexp $(rnilib)`; fi
	- if test "X`pwd`" != X$(mansrc) -a X$(mansrc) != X; then \
for page in $(manpages); do \
dest=$(mansrc)/`basename $$page .1`.$(manext); \
rm -f $$dest; cp $$page $$dest; chmod 444 $$dest; \
done; \
fi

clean:
	@echo 'Use "make realclean" to also remove the Makefile.'
	@echo 'Use "make spotless" to even remove config.sh.'
	rm -rf UU
	rm -f *.o core $(addedbymake)

realclean:
	@echo 'You can use "Configure -S ; make depend ; make" to reverse this.'
	rm -rf UU
	rm -f *.o core $(addedbymake) Makefile

spotless:
	rm -rf UU
	rm -f *.o core $(addedbymake) Makefile filexp config.sh

# The following lint has practically everything turned on.  Unfortunately,
# you have to wade through a lot of mumbo jumbo that can't be suppressed.
# If the source file has a /*NOSTRICT*/ somewhere, ignore the lint message
# for that spot.

lint: lint_trn

lint_trn:
	lint $(lintflags) $(defs) $(c) > trn.fuzz

sabertrn: $(c)
	#load $(c) $(libs)

depend: config.h Makefile makedepend
	./makedepend

# AUTOMATICALLY GENERATED MAKE DEPENDENCIES--PUT NOTHING BELOW THIS LINE
config.h: config.h.SH config.sh ; sh config.h.SH
Makefile: Makefile.SH config.sh ; sh Makefile.SH
makedepend: makedepend.SH config.sh ; sh makedepend.SH
$(obj1):
	@echo "You haven't done a "'"make depend" yet!'; exit 1
!NO!SUBS!
case "$d_nntp" in
define)	sed < Makefile -e '/^#NNTP/s/^#NNTP//' > Makefile.new ;;
*)	sed < Makefile -e '/^#NNTP/d' > Makefile.new ;;
esac
mv Makefile.new Makefile
$eunicefix Makefile
