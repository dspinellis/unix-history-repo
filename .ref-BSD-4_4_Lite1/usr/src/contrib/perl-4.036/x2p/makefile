# : Makefile.SH,v 24920Revision: 4.0.1.3 24920Date: 92/06/08 16:11:32 $
#
# $Log:	Makefile.SH,v $
# Revision 4.0.1.3  92/06/08  16:11:32  lwall
# patch20: SH files didn't work well with symbolic links
# patch20: cray didn't give enough memory to /bin/sh
# patch20: makefiles now display new shift/reduce expectations
# 
# Revision 4.0.1.2  91/11/05  19:19:04  lwall
# patch11: random cleanup
# 
# Revision 4.0.1.1  91/06/07  12:12:14  lwall
# patch4: cflags now emits entire cc command except for the filename
# 
# Revision 4.0  91/03/20  01:57:03  lwall
# 4.0 baseline.
# 
# 

CC = cc
YACC = /usr/bin/yacc
bin = /usr/contrib/bin
lib = 
mansrc = /usr/contrib/man/cat1
manext = 1
LDFLAGS = 
SMALL = 
LARGE =  
mallocsrc = 
mallocobj = 
shellflags = 

libs = -lm

CCCMD = `sh $(shellflags) cflags $@`

public = a2p s2p find2perl

private = 

manpages = a2p.man s2p.man

util =

sh = Makefile.SH makedepend.SH

h = EXTERN.h INTERN.h ../config.h handy.h hash.h a2p.h str.h util.h

c = hash.c $(mallocsrc) str.c util.c walk.c

obj = hash.o $(mallocobj) str.o util.o walk.o

lintflags = -phbvxac

addedbyconf = Makefile.old bsd eunice filexp loc pdp11 usg v7

# grrr
SHELL = /bin/sh

.c.o:
	$(CCCMD) $*.c

all: $(public) $(private) $(util)
	touch all

a2p: $(obj) a2p.o
	$(CC) $(LDFLAGS) $(obj) a2p.o $(libs) -o a2p

a2p.c: a2p.y
	@echo Expect 231 shift/reduce conflicts...
	$(YACC) a2p.y
	mv y.tab.c a2p.c

a2p.o: a2p.c a2py.c a2p.h EXTERN.h util.h INTERN.h handy.h ../config.h str.h hash.h
	$(CCCMD) $(LARGE) a2p.c

install: a2p s2p
# won't work with csh
	export PATH || exit 1
	- mv $(bin)/a2p $(bin)/a2p.old 2>/dev/null
	- mv $(bin)/s2p $(bin)/s2p.old 2>/dev/null
	- if test `pwd` != $(bin); then cp $(public) $(bin); fi
	cd $(bin); \
for pub in $(public); do \
chmod +x `basename $$pub`; \
done
	- if test `pwd` != $(mansrc); then \
for page in $(manpages); do \
cp $$page $(mansrc)/`basename $$page .man`.$(manext); \
done; \
fi

clean:
	rm -f a2p *.o a2p.c

realclean: clean
	rm -f *.orig */*.orig core $(addedbyconf) a2p.c s2p find2perl all cflags

# The following lint has practically everything turned on.  Unfortunately,
# you have to wade through a lot of mumbo jumbo that can't be suppressed.
# If the source file has a /*NOSTRICT*/ somewhere, ignore the lint message
# for that spot.

lint:
	lint $(lintflags) $(defs) $(c) > a2p.fuzz

depend: $(mallocsrc) ../makedepend
	../makedepend

clist:
	echo $(c) | tr ' ' '\012' >.clist

hlist:
	echo $(h) | tr ' ' '\012' >.hlist

shlist:
	echo $(sh) | tr ' ' '\012' >.shlist

config.sh: ../config.sh
	rm -f config.sh
	ln ../config.sh .

malloc.c: ../malloc.c
	sed 's/"perl.h"/"..\/perl.h"/' ../malloc.c >malloc.c

# AUTOMATICALLY GENERATED MAKE DEPENDENCIES--PUT NOTHING BELOW THIS LINE
# If this runs make out of memory, delete /usr/include lines.
hash.o: 
hash.o: ../config.h
hash.o: /usr/include/ctype.h
hash.o: /usr/include/machine/ansi.h
hash.o: /usr/include/machine/endian.h
hash.o: /usr/include/machine/types.h
hash.o: /usr/include/stdio.h
hash.o: /usr/include/sys/cdefs.h
hash.o: /usr/include/sys/types.h
hash.o: EXTERN.h
hash.o: a2p.h
hash.o: handy.h
hash.o: hash.c
hash.o: hash.h
hash.o: str.h
hash.o: util.h
str.o: 
str.o: ../config.h
str.o: /usr/include/ctype.h
str.o: /usr/include/machine/ansi.h
str.o: /usr/include/machine/endian.h
str.o: /usr/include/machine/types.h
str.o: /usr/include/stdio.h
str.o: /usr/include/sys/cdefs.h
str.o: /usr/include/sys/types.h
str.o: EXTERN.h
str.o: a2p.h
str.o: handy.h
str.o: hash.h
str.o: str.c
str.o: str.h
str.o: util.h
util.o: 
util.o: ../config.h
util.o: /usr/include/ctype.h
util.o: /usr/include/machine/ansi.h
util.o: /usr/include/machine/endian.h
util.o: /usr/include/machine/types.h
util.o: /usr/include/stdio.h
util.o: /usr/include/sys/cdefs.h
util.o: /usr/include/sys/types.h
util.o: EXTERN.h
util.o: INTERN.h
util.o: a2p.h
util.o: handy.h
util.o: hash.h
util.o: str.h
util.o: util.c
util.o: util.h
walk.o: 
walk.o: ../config.h
walk.o: /usr/include/ctype.h
walk.o: /usr/include/machine/ansi.h
walk.o: /usr/include/machine/endian.h
walk.o: /usr/include/machine/types.h
walk.o: /usr/include/stdio.h
walk.o: /usr/include/sys/cdefs.h
walk.o: /usr/include/sys/types.h
walk.o: EXTERN.h
walk.o: a2p.h
walk.o: handy.h
walk.o: hash.h
walk.o: str.h
walk.o: util.h
walk.o: walk.c
Makefile: Makefile.SH config.sh ; /bin/sh Makefile.SH
makedepend: makedepend.SH config.sh ; /bin/sh makedepend.SH
# WARNING: Put nothing here or make depend will gobble it up!
