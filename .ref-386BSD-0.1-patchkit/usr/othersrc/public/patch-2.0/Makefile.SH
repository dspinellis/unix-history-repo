case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting Makefile (with variable substitutions)"
cat >Makefile <<!GROK!THIS!
# $Header: Makefile.SH,v 2.0 86/09/17 15:36:15 lwall Exp $
#
# $Log:	Makefile.SH,v $
# Revision 2.0  86/09/17  15:36:15  lwall
# Baseline for netwide release.
# 
# Revision 1.2  86/09/08  14:07:42  lwall
# Split up patch.c.
# 
# Revision 1.1  86/08/01  20:18:35  lwall
# Initial revision
# 

CC = $cc
bin = $bin
mansrc = $mansrc
manext = $manext
CFLAGS = $iandd -O
LDFLAGS = $iandd

!GROK!THIS!
cat >>Makefile <<'!NO!SUBS!'

public = patch
private = 
manpages = patch.man
util = Makefile

c = patch.c pch.c inp.c version.c util.c

obj = patch.o pch.o inp.o util.o version.o

lintflags = -phbvxac

addedbyconf = Makefile.old bsd config.h config.sh eunice loc pdp11 usg v7

# grrr
SHELL = /bin/sh

.c.o:
	$(CC) -c $(CFLAGS) $*.c

all: $(public) $(private) $(util)
	touch all

patch: $(obj)
	$(CC) $(LDFLAGS) $(obj) $(libs) -o patch

# won't work with csh
install: patch
	export PATH || exit 1
	- mv $(bin)/patch $(bin)/patch.old
	- if test `pwd` != $(bin); then cp $(public) $(bin); fi
	cd $(bin); chmod 755 $(public)
	- if test `pwd` != $(mansrc); then \
for page in $(manpages); do \
cp $$page $(mansrc)/`basename $$page .man`.$(manext); \
done; \
fi

clean:
	rm -f *.o *.orig core

realclean:
	rm -f patch *.o *.orig core $(addedbyconf)

# The following lint has practically everything turned on.  Unfortunately,
# you have to wade through a lot of mumbo jumbo that can't be suppressed.
# If the source file has a /*NOSTRICT*/ somewhere, ignore the lint message
# for that spot.

lint:
	lint $(lintflags) $(defs) $(c) > patch.fuzz

patch.o: config.h common.h patch.c inp.h pch.h util.h version.h
pch.o: config.h common.h pch.c pch.h util.h
inp.o: config.h common.h inp.c inp.h util.h
util.o: config.h common.h util.c util.h
version.o: config.h common.h version.c version.h patchlevel.h util.h

!NO!SUBS!
$eunicefix Makefile
