case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting Makefile (with variable substitutions)"
cat >Makefile <<!GROK!THIS!
# $Header: Makefile.SH,v 2.0.1.2 88/06/22 20:43:40 lwall Locked $
#
# $Log:	Makefile.SH,v $
# Revision 2.0.1.2  88/06/22  20:43:40  lwall
# patch12: config.h now depends on config.h.SH
# 
# Revision 2.0.1.1  88/06/03  15:00:48  lwall
# patch10: upgraded to match some new metaconfig stuff
# 
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
CFLAGS = $ccflags -O
LDFLAGS = $ldflags
SMALL = $small
LARGE = $large $split

!GROK!THIS!
cat >>Makefile <<'!NO!SUBS!'

public = patch
private = 
manpages = patch.man
util = Makefile

c = patch.c pch.c inp.c util.c version.c backupfile.c

obj = patch.o pch.o inp.o util.o version.o backupfile.o

lintflags = -phbvxac

addedbyconf = Makefile Makefile.old bsd config.h config.sh eunice loc pdp11 usg v7

# grrr
SHELL = /bin/sh

.c.o:
	$(CC) -c $(CFLAGS) $(LARGE) $*.c

all: $(public) $(private) $(util)
	@echo Done.

patch: $(obj)
	$(CC) $(LDFLAGS) $(obj) $(libs) -o patch

config.h: config.h.SH
	sh config.h.SH

# won't work with csh
install: patch
	export PATH || exit 1
	-mv $(bin)/patch $(bin)/patch.old
	cp patch $(bin)/patch
	chmod 755 $(bin)/patch
	-cp patch.man $(mansrc)/patch.$(manext)

uninstall:
	rm -f $(bin)/patch $(mansrc)/patch.$(manext)
	test ! -f $(bin)/patch.old || mv $(bin)/patch.old $(bin)/patch

clean:
	rm -f patch *.o core

mostlyclean: clean

distclean: clean
	rm -f $(addedbyconf)

realclean: distclean
	rm -f *.orig *~

DISTFILES=ChangeLog Configure EXTERN.h INTERN.h MANIFEST Makefile.SH \
README common.h config.H config.h.SH inp.c inp.h malloc.c patch.c patch.man \
patchlevel.h pch.c pch.h util.c util.h version.c version.h backupfile.c \
backupfile.h

dist: $(DISTFILES)
	echo patch-2.0.`sed -e '/PATCHLEVEL/!d' -e 's/[^0-9]*\([0-9a-z.]*\).*/\1/' -e q patchlevel.h` > .fname
	rm -rf `cat .fname`
	mkdir `cat .fname`
	ln $(DISTFILES) `cat .fname`
	tar chZf `cat .fname`.tar.Z `cat .fname`
	rm -rf `cat .fname` .fname

# The following lint has practically everything turned on.  Unfortunately,
# you have to wade through a lot of mumbo jumbo that can't be suppressed.
# If the source file has a /*NOSTRICT*/ somewhere, ignore the lint message
# for that spot.

lint:
	lint $(lintflags) $(defs) $(c) > patch.fuzz

patch.o: config.h common.h patch.c inp.h pch.h util.h version.h backupfile.h
pch.o: config.h common.h pch.c pch.h util.h
inp.o: config.h common.h inp.c inp.h util.h
util.o: config.h common.h util.c util.h backupfile.h
version.o: config.h common.h version.c version.h patchlevel.h util.h
backupfile.o: config.h backupfile.c backupfile.h

!NO!SUBS!
$eunicefix Makefile
