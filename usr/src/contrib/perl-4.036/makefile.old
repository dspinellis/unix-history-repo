# : Makefile.SH,v 24920Revision: 4.0.1.4 24920Date: 92/06/08 11:40:43 $
#
# $Log:	Makefile.SH,v $
# Revision 4.0.1.4  92/06/08  11:40:43  lwall
# patch20: cray didn't give enough memory to /bin/sh
# patch20: various and sundry fixes
# 
# Revision 4.0.1.3  91/11/05  15:48:11  lwall
# patch11: saberized perl
# patch11: added support for dbz
# 
# Revision 4.0.1.2  91/06/07  10:14:43  lwall
# patch4: cflags now emits entire cc command except for the filename
# patch4: alternate make programs are now semi-supported
# patch4: uperl.o no longer tries to link in libraries prematurely
# patch4: installperl now installs x2p stuff too
# 
# Revision 4.0.1.1  91/04/11  17:30:39  lwall
# patch1: C flags are now settable on a per-file basis
# 
# Revision 4.0  91/03/20  00:58:54  lwall
# 4.0 baseline.
# 
# 

CC = cc
YACC = /usr/bin/yacc
bin = /usr/contrib/bin
scriptdir = /usr/contrib/bin
privlib = /usr/contrib/lib/perl
mansrc = /usr/contrib/man/cat1
manext = 1
LDFLAGS = 
CLDFLAGS = 
SMALL = 
LARGE =  
mallocsrc = 
mallocobj = 
SLN = ln -s
RMS = rm -f

libs = -lm 

public = perl taintperl 

shellflags = 

# To use an alternate make, set  in config.sh.
MAKE = make


CCCMD = `sh $(shellflags) cflags $@`

private = 

scripts = h2ph

manpages = perl.man h2ph.man

util =

sh = Makefile.SH makedepend.SH h2ph.SH

h1 = EXTERN.h INTERN.h arg.h array.h cmd.h config.h form.h handy.h
h2 = hash.h perl.h regcomp.h regexp.h spat.h stab.h str.h util.h

h = $(h1) $(h2)

c1 = array.c cmd.c cons.c consarg.c doarg.c doio.c dolist.c dump.c
c2 = eval.c form.c hash.c $(mallocsrc) perl.c regcomp.c regexec.c
c3 = stab.c str.c toke.c util.c usersub.c

c = $(c1) $(c2) $(c3)

s1 = array.c cmd.c cons.c consarg.c doarg.c doio.c dolist.c dump.c
s2 = eval.c form.c hash.c perl.c regcomp.c regexec.c
s3 = stab.c str.c toke.c util.c usersub.c perly.c

saber = $(s1) $(s2) $(s3)

obj1 = array.o cmd.o cons.o consarg.o doarg.o doio.o dolist.o dump.o
obj2 = eval.o form.o $(mallocobj) perl.o regcomp.o regexec.o
obj3 = stab.o str.o toke.o util.o

obj = $(obj1) $(obj2) $(obj3)

tobj1 = tarray.o tcmd.o tcons.o tconsarg.o tdoarg.o tdoio.o tdolist.o tdump.o
tobj2 = teval.o tform.o thash.o $(mallocobj) tregcomp.o tregexec.o
tobj3 = tstab.o tstr.o ttoke.o tutil.o

tobj = $(tobj1) $(tobj2) $(tobj3)

lintflags = -hbvxac

addedbyconf = Makefile.old bsd eunice filexp loc pdp11 usg v7

# grrr
SHELL = /bin/sh

.c.o:
	$(CCCMD) $*.c

all: $(public) $(private) $(util) uperl.o $(scripts)
	cd x2p; $(MAKE) all
	touch all

# This is the standard version that contains no "taint" checks and is
# used for all scripts that aren't set-id or running under something set-id.
# The $& notation is tells Sequent machines that it can do a parallel make,
# and is harmless otherwise.

perl: $& perly.o $(obj) hash.o usersub.o
	$(CC) $(LARGE) $(CLDFLAGS) $(obj) hash.o perly.o usersub.o $(libs) -o perl

# This command assumes that /usr/include/dbz.h and /usr/lib/dbz.o exist.

dbzperl: $& perly.o $(obj) zhash.o usersub.o
	$(CC) $(LARGE) $(CLDFLAGS) $(obj) zhash.o /usr/lib/dbz.o perly.o usersub.o $(libs) -o dbzperl

zhash.o: hash.c $(h)
	$(RMS) zhash.c
	$(SLN) hash.c zhash.c
	$(CCCMD) -DWANT_DBZ zhash.c
	$(RMS) zhash.c

uperl.o: $& perly.o $(obj) hash.o
	-ld $(LARGE) $(LDFLAGS) -r $(obj) hash.o perly.o -o uperl.o

saber: $(saber)
	# load $(saber)
	# load /lib/libm.a

# This version, if specified in Configure, does ONLY those scripts which need
# set-id emulation.  Suidperl must be setuid root.  It contains the "taint"
# checks as well as the special code to validate that the script in question
# has been invoked correctly.

suidperl: $& tperly.o sperl.o $(tobj) usersub.o
	$(CC) $(LARGE) $(CLDFLAGS) sperl.o $(tobj) tperly.o usersub.o $(libs) \
	    -o suidperl

# This version interprets scripts that are already set-id either via a wrapper
# or through the kernel allowing set-id scripts (bad idea).  Taintperl must
# NOT be setuid to root or anything else.  The only difference between it
# and normal perl is the presence of the "taint" checks.

taintperl: $& tperly.o tperl.o $(tobj) usersub.o
	$(CC) $(LARGE) $(CLDFLAGS) tperl.o $(tobj) tperly.o usersub.o $(libs) \
	    -o taintperl

# Replicating all this junk is yucky, but I don't see a portable way to fix it.

tperly.o: perly.c perly.h $(h)
	$(RMS) tperly.c
	$(SLN) perly.c tperly.c
	$(CCCMD) -DTAINT tperly.c
	$(RMS) tperly.c

tperl.o: perl.c perly.h patchlevel.h perl.h $(h)
	$(RMS) tperl.c
	$(SLN) perl.c tperl.c
	$(CCCMD) -DTAINT tperl.c
	$(RMS) tperl.c

sperl.o: perl.c perly.h patchlevel.h $(h)
	$(RMS) sperl.c
	$(SLN) perl.c sperl.c
	$(CCCMD) -DTAINT -DIAMSUID sperl.c
	$(RMS) sperl.c

tarray.o: array.c $(h)
	$(RMS) tarray.c
	$(SLN) array.c tarray.c
	$(CCCMD) -DTAINT tarray.c
	$(RMS) tarray.c

tcmd.o: cmd.c $(h)
	$(RMS) tcmd.c
	$(SLN) cmd.c tcmd.c
	$(CCCMD) -DTAINT tcmd.c
	$(RMS) tcmd.c

tcons.o: cons.c $(h) perly.h
	$(RMS) tcons.c
	$(SLN) cons.c tcons.c
	$(CCCMD) -DTAINT tcons.c
	$(RMS) tcons.c

tconsarg.o: consarg.c $(h)
	$(RMS) tconsarg.c
	$(SLN) consarg.c tconsarg.c
	$(CCCMD) -DTAINT tconsarg.c
	$(RMS) tconsarg.c

tdoarg.o: doarg.c $(h)
	$(RMS) tdoarg.c
	$(SLN) doarg.c tdoarg.c
	$(CCCMD) -DTAINT tdoarg.c
	$(RMS) tdoarg.c

tdoio.o: doio.c $(h)
	$(RMS) tdoio.c
	$(SLN) doio.c tdoio.c
	$(CCCMD) -DTAINT tdoio.c
	$(RMS) tdoio.c

tdolist.o: dolist.c $(h)
	$(RMS) tdolist.c
	$(SLN) dolist.c tdolist.c
	$(CCCMD) -DTAINT tdolist.c
	$(RMS) tdolist.c

tdump.o: dump.c $(h)
	$(RMS) tdump.c
	$(SLN) dump.c tdump.c
	$(CCCMD) -DTAINT tdump.c
	$(RMS) tdump.c

teval.o: eval.c $(h)
	$(RMS) teval.c
	$(SLN) eval.c teval.c
	$(CCCMD) -DTAINT teval.c
	$(RMS) teval.c

tform.o: form.c $(h)
	$(RMS) tform.c
	$(SLN) form.c tform.c
	$(CCCMD) -DTAINT tform.c
	$(RMS) tform.c

thash.o: hash.c $(h)
	$(RMS) thash.c
	$(SLN) hash.c thash.c
	$(CCCMD) -DTAINT thash.c
	$(RMS) thash.c

tregcomp.o: regcomp.c $(h)
	$(RMS) tregcomp.c
	$(SLN) regcomp.c tregcomp.c
	$(CCCMD) -DTAINT tregcomp.c
	$(RMS) tregcomp.c

tregexec.o: regexec.c $(h)
	$(RMS) tregexec.c
	$(SLN) regexec.c tregexec.c
	$(CCCMD) -DTAINT tregexec.c
	$(RMS) tregexec.c

tstab.o: stab.c $(h)
	$(RMS) tstab.c
	$(SLN) stab.c tstab.c
	$(CCCMD) -DTAINT tstab.c
	$(RMS) tstab.c

tstr.o: str.c $(h) perly.h
	$(RMS) tstr.c
	$(SLN) str.c tstr.c
	$(CCCMD) -DTAINT tstr.c
	$(RMS) tstr.c

ttoke.o: toke.c $(h) perly.h
	$(RMS) ttoke.c
	$(SLN) toke.c ttoke.c
	$(CCCMD) -DTAINT ttoke.c
	$(RMS) ttoke.c

tutil.o: util.c $(h)
	$(RMS) tutil.c
	$(SLN) util.c tutil.c
	$(CCCMD) -DTAINT tutil.c
	$(RMS) tutil.c

perly.h: perly.c
	@ echo Dummy dependency for dumb parallel make
	touch perly.h

perly.c: perly.y perly.fixer
	@ \
case "$(YACC)" in \
    *bison*) echo 'Expect' 25 shift/reduce and 59 reduce/reduce conflicts;; \
    *) echo 'Expect' 27 shift/reduce and 57 reduce/reduce conflicts;; \
esac
	$(YACC) -d perly.y
	sh $(shellflags) ./perly.fixer y.tab.c perly.c
	mv y.tab.h perly.h
	echo 'extern YYSTYPE yylval;' >>perly.h

perly.o: perly.c perly.h $(h)
	$(CCCMD) perly.c

install: all
	nroff -man perl.man > /usr/contrib/man/cat1/perl.0
	nroff -man h2ph.man > /usr/contrib/man/cat1/h2ph.0
	nroff -man x2p/s2p.man > /usr/contrib/man/cat1/s2p.0
	nroff -man x2p/a2p.man > /usr/contrib/man/cat1/a2p.0
	./perl installperl

clean:
	rm -f *.o all perl taintperl suidperl perly.c
	cd x2p; $(MAKE) clean

realclean: clean
	cd x2p; $(MAKE) realclean
	rm -f *.orig */*.orig *~ */*~ core $(addedbyconf) h2ph h2ph.man
	rm -f perly.c perly.h t/perl Makefile config.h makedepend makedir
	rm -f makefile x2p/Makefile x2p/makefile cflags x2p/cflags
	rm -f c2ph pstruct

# The following lint has practically everything turned on.  Unfortunately,
# you have to wade through a lot of mumbo jumbo that can't be suppressed.
# If the source file has a /*NOSTRICT*/ somewhere, ignore the lint message
# for that spot.

lint: perly.c $(c)
	lint $(lintflags) $(defs) perly.c $(c) > perl.fuzz

depend: makedepend
	- test -f perly.h || cp /dev/null perly.h
	./makedepend
	- test -s perly.h || /bin/rm -f perly.h
	cd x2p; $(MAKE) depend

test: perl
	- cd t && chmod +x TEST */*.t
	- cd t && (rm -f perl; $(SLN) ../perl perl) && ./perl TEST </dev/tty

clist:
	echo $(c) | tr ' ' '\012' >.clist

hlist:
	echo $(h) | tr ' ' '\012' >.hlist

shlist:
	echo $(sh) | tr ' ' '\012' >.shlist

# AUTOMATICALLY GENERATED MAKE DEPENDENCIES--PUT NOTHING BELOW THIS LINE
$(obj) hash.o:
	@ echo "You haven't done a "'"make depend" yet!'; exit 1
makedepend: makedepend.SH
	/bin/sh $(shellflags) makedepend.SH
