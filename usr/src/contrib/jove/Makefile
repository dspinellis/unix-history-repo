###########################################################################
# This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE #
# is provided to you without charge, and with no warranty.  You may give  #
# away copies of JOVE, including sources, provided that this notice is    #
# included in all the files.                                              #
###########################################################################

# TMPDIR is where the tmp files get stored, usually /tmp or /tmp/jove.  If
# your system does not remove subdirectories of /tmp on reboot (lots do
# remove them these days) then it makes sense to make TMPDIR be /tmp/jove.
# But if you want to recover buffers on system crashes, you should create a
# directory that doesn't get cleaned upon reboot, and use that instead.
# You would probably want to clean out that directory periodically with
# /etc/cron.  
#
# SHAREDIR is for online documentation, and the system-wide .joverc file.  
# LIBDIR is for the PORTSRV and KBD processes and RECOVER. 
# BINDIR is where to put the executables JOVE and TEACHJOVE.  
# MANDIR is where the manual pages go for JOVE, RECOVER and TEACHJOVE.  
# MANEXT is the extension for the man pages, e.g., jove.1 or jove.l or jove.m.

DESTDIR =

TMPDIR = /tmp
RECDIR = /tmp
SHAREDIR = /usr/new/lib/jove
LIBDIR = /usr/new/lib/jove
BINDIR = /usr/new
MANDIR = /usr/man/cat1
MANEXT = 0
JSHELL = /bin/csh

PROG = jove
VERSION = 4.14

# These should all just be right if the above ones are.
JOVE = $(DESTDIR)$(BINDIR)/jove
TEACHJOVE = $(DESTDIR)$(BINDIR)/teachjove
RECOVER = $(DESTDIR)$(LIBDIR)/recover
PORTSRV = $(DESTDIR)$(LIBDIR)/portsrv
KBD = $(DESTDIR)$(LIBDIR)/kbd
JOVERC = $(DESTDIR)$(SHAREDIR)/jove.rc
CMDS.DOC = $(DESTDIR)$(SHAREDIR)/cmds.doc
TEACH-JOVE = $(DESTDIR)$(SHAREDIR)/teach-jove
JOVEM = $(DESTDIR)$(MANDIR)/jove.$(MANEXT)
TEACHJOVEM = $(DESTDIR)$(MANDIR)/teachjove.$(MANEXT)

CC=	cc

# Select the right libraries for your system.
#	2.10BSD:LIBS = -ltermcap
#	v7:	LIBS = -ltermcap
#	4.1BSD:	LIBS = -ltermcap -ljobs
#	4.2BSD:	LIBS = -ltermcap
#	4.3BSD:	LIBS = -ltermcap
#	SysV Rel. 2: LIBS = -lcurses
#	SCO Xenix: LIBS = -ltermcap -lx
#	MIPS: -lbsd

LIBS = -ltermcap

# If you are not VMUNIX (vax running Berkeley Version 4), you must specify
# the -i flags (split I/D space) and maybe the -x option (for adb to work).
#	2.10BSD:LDFLAGS =
#	v7:	LDFLAGS =
#	4.1BSD:	LDFLAGS =
#	4.2BSD:	LDFLAGS =
#	4.3BSD:	LDFLAGS =
#	SysV Rel. 2: LDFLAGS = -Ml
#	SCO Xenix: LDFLAGS = -Ml -F 3000
#
# SEPFLAG should be:
#	not on a PDP-11:		SEPFLAG =
#	PDP-11 with separate I&D:	SEPFLAG = -i
#	PDP-11 without separate I&D:	SEPFLAG = -n
#

LDFLAGS =

SEPFLAG =

# for SCO Xenix, set
#	MEMFLAGS = -Mle
#	CFLAGS = -LARGE -O -F 3000 -K -Mle  (say -Mle2 for an 80286)
# for MIPS, set
#	CFLAGS = -O -I/usr/include/bsd

CFLAGS = -O

BASESEG = funcdefs.o keys.o argcount.o ask.o buf.o ctype.o delete.o \
	  disp.o insert.o io.o jove.o marks.o misc.o re.o screen.o \
	  tune.o util.o vars.o version.o list.o keymaps.o
OVLAY1 = abbrev.o rec.o paragraph.o fmt.o
OVLAY2 = c.o wind.o fp.o move.o
OVLAY3 = extend.o macros.o
OVLAY4 = iproc.o re1.o
OVLAY5 = proc.o scandir.o term.o case.o

OBJECTS = $(BASESEG) $(OVLAY1) $(OVLAY2) $(OVLAY3) $(OVLAY4) $(OVLAY5)

C_SRC = funcdefs.c abbrev.c argcount.c ask.c buf.c c.c case.c ctype.c \
	delete.c disp.c extend.c fp.c fmt.c insert.c io.c iproc.c \
	jove.c list.c macros.c marks.c misc.c move.c paragraph.c \
	proc.c re.c re1.c rec.c scandir.c screen.c term.c util.c \
	vars.c version.c wind.c getch.c mac.c keymaps.c pcscr.c

SOURCES = $(C_SRC) portsrv.c recover.c setmaps.c teachjove.c kbd.c

HEADERS = argcount.h buf.h chars.h ctype.h dataobj.h disp.h \
	externs.h fp.h io.h iproc.h jove.h keymaps.h list.h mac.h \
	re.h rec.h scandir.h screen.h style.h sysdep.h temp.h termcap.h \
	ttystate.h tune.h util.h vars.h wait.h wind.h


DOCS1 =	doc/example.rc doc/jove.1 doc/jove.2 doc/jove.3 \
	doc/jove.4 doc/jove.5 doc/jove.nr doc/system.rc \
	doc/teach-jove doc/teachjove.nr doc/README doc/jove.qref
DOCS2 = doc/cmds.doc.nr
DOCS3 = doc/joveman doc/cmds.doc doc/manpage
DOCS = $(DOCS1) $(DOCS2)

MISC = Makefile Makefile.dos tune.dos tune.template README Readme.dos \
	Readme.mac iproc-pipes.c iproc-ptys.c

SUPPORT = teachjove.c recover.c setmaps.c portsrv.c kbd.c keys.txt \
	macvert.c menumaps.txt mjovers.Hqx

BACKUPS = $(HEADERS) $(C_SRC) $(DOCS) $(SUPPORT) $(MISC)

all:	sdate xjove recover teachjove portsrv kbd macvert edate

sdate:
	@echo "**** make started at `date` ****"

edate:
	@echo "**** make completed at `date` ****"

xjove:	$(OBJECTS)
	$(CC) $(LDFLAGS) -o xjove $(OBJECTS) $(LIBS)
	@-size xjove

gjove:	$(OBJECTS)
	ld -X /lib/gcrt0.o -o gjove $(OBJECTS) -lc $(LIBS)
	@-size gjove

ovjove:	$(OBJECTS)
	ld $(SEPFLAG) $(LDFLAGS) -X /lib/crt0.o \
		-Z $(OVLAY1) \
		-Z $(OVLAY2) \
		-Z $(OVLAY3) \
		-Z $(OVLAY4) \
		-Z $(OVLAY5) \
		-Y $(BASESEG) \
		-o xjove $(LIBS) -lc
	@-size xjove

portsrv:	portsrv.o
	$(CC) $(LDFLAGS) -o portsrv $(SEPFLAG) portsrv.o $(LIBS)

kbd:	kbd.o
	$(CC) $(LDFLAGS) -o kbd $(SEPFLAG) kbd.o $(LIBS)

recover:	rectune.h recover.o tune.o rec.h temp.h
	$(CC) $(LDFLAGS) -o recover $(SEPFLAG) recover.o tune.o $(LIBS)

teachjove:	teachjove.o
	$(CC) $(LDFLAGS) -o teachjove $(SEPFLAG) teachjove.o $(LIBS)

setmaps:	setmaps.o funcdefs.c
	$(CC) $(LDFLAGS) -o setmaps setmaps.o

teachjove.o:	teachjove.c /usr/include/sys/types.h /usr/include/sys/file.h
	$(CC) -c $(CFLAGS) -DTEACHJOVE=\"$(TEACH-JOVE)\" teachjove.c

# don't optimize setmaps.c because it produces bad code in some places
# for some reason
setmaps.o:	funcdefs.c keys.txt
	$(CC) $(MEMFLAGS) -c setmaps.c

# ignore error messages from setmaps
# it doesn't understand ifdefs

keys.c:	setmaps keys.txt
	-./setmaps < keys.txt > keys.c

keys.o:	keys.c jove.h

tune.c: Makefile tune.template
	-rm -f tune.c
	@echo "/* Changes should be made in Makefile, not to this file! */" > tune.c
	@echo "" >> tune.c
	@sed -e 's;TMPDIR;$(TMPDIR);' \
	     -e 's;LIBDIR;$(LIBDIR);' \
	     -e 's;SHAREDIR;$(SHAREDIR);' \
	     -e 's;BINDIR;$(BINDIR);' \
	     -e 's;SHELL;$(JSHELL);' tune.template >> tune.c

rectune.h: Makefile
	-rm -f nrectune.h
	@echo "/* Changes should be made in Makefile, not to this file! */" > nrectune.h
	@echo "" >> nrectune.h
	@echo \#define TMP_DIR \"$(TMPDIR)\" >> nrectune.h
	@echo \#define REC_DIR \"$(RECDIR)\" >> nrectune.h
	-cmp -s nrectune.h rectune.h || (rm -f rectune.h; cp nrectune.h rectune.h)

iproc.o: iproc-ptys.c iproc-pipes.c iproc.c
	$(CC) -c $(CFLAGS) iproc.c

macvert:	macvert.c
	$(CC) $(CFLAGS) -o macvert macvert.c

# install doesn't work for Xenix (no install program)

install: $(DESTDIR)$(LIBDIR) $(DESTDIR)$(SHAREDIR) \
	 $(TEACH-JOVE) $(CMDS.DOC) $(JOVERC) \
	 $(PORTSRV) $(KBD) $(RECOVER) $(JOVE) $(TEACHJOVE) $(JOVEM) \
	 $(RECOVERM) $(TEACHJOVEM)
	@echo See the README about changes to /etc/rc or /etc/rc.local
	@echo so that the system recovers jove files on reboot after a crash

$(DESTDIR)$(LIBDIR) $(DESTDIR)$(SHAREDIR):
	-mkdir $@

$(TEACH-JOVE): doc/teach-jove
	install -c -m 644 doc/teach-jove $(TEACH-JOVE)

doc/cmds.doc:	doc/cmds.doc.nr doc/jove.4 doc/jove.5
	nroff doc/cmds.doc.nr doc/jove.4 doc/jove.5 > doc/cmds.doc

$(CMDS.DOC): doc/cmds.doc
	install -c -m 644 doc/cmds.doc $(CMDS.DOC)

$(JOVERC): doc/system.rc
	install -c -m 644 doc/system.rc $(JOVERC)

$(PORTSRV): portsrv
	install -c -s -m 755 portsrv $(PORTSRV)

$(KBD): kbd
	install -c -s -m 755 kbd $(KBD)

$(RECOVER): recover
	install -c -s -m 755 recover $(RECOVER)

$(JOVE): xjove
	install -c -s -m 755 xjove $(JOVE)

$(TEACHJOVE): teachjove
	install -c -s -m 755 teachjove $(TEACHJOVE)

$(JOVEM): doc/jove.nr
	@sed -e 's;TMPDIR;$(TMPDIR);' \
	     -e 's;LIBDIR;$(LIBDIR);' \
	     -e 's;SHELL;$(JSHELL);' doc/jove.nr \
	| nroff -h -man >$(JOVEM)
	chmod 444 $(JOVEM)

$(TEACHJOVEM): doc/teachjove.nr
	@sed -e 's;TMPDIR;$(TMPDIR);' \
	     -e 's;LIBDIR;$(LIBDIR);' \
	     -e 's;SHELL;$(JSHELL);' doc/teachjove.nr \
	| nroff -h -man >$(TEACHJOVEM)
	chmod 444 $(TEACHJOVEM)

echo:
	@echo $(C-FILES) $(HEADERS)

lint:
	lint -n $(C_SRC) tune.c keys.c
	@echo Done

tags:
	ctags -w $(C_SRC) $(HEADERS) iproc-ptys.c

ciall:
	ci $(BACKUPS)

coall:
	co $(BACKUPS)

jove.shar:
	shar $(BACKUPS) > jove.shar

tar:
	cd ..; ls -d `cat $(PROG)$(VERSION)/Exclude | \
		sed 's,^,$(PROG)$(VERSION)/,'` > /tmp/tar$$$$.exclude ; \
	rm -f /tmp/$(PROG)$(VERSION).tar.Z ; \
	tar cvfX - /tmp/tar$$$$.exclude $(PROG)$(VERSION) | \
		compress > /tmp/$(PROG)$(VERSION).tar.Z ; \
	rm -f /tmp/tar$$$$.exclude

backup: $(BACKUPS)
	tar cf backup $(BACKUPS)

tape-backup:
	tar cf /dev/rst8 $(BACKUPS)

srcdownload:
	kermit -s $(SUPPORT) $(MISC) $(HEADERS) $(C_SRC)

docdownload:
	kermit -s $(DOCS1) $(DOCS3)
	kermit -s doc/cmds.doc.nr -a cmdsdoc.nr

touch:
	touch $(OBJECTS)

clean:
	rm -f a.out core *.o keys.c tune.c xjove portsrv kbd recover setmaps \
	teachjove macvert nrectune.h rectune.h

# This version only works under 4.3BSD
# To enable, remove single # from start of following lines
depend:
	-rm -f makedep eddep
	for i in ${SOURCES} ; do \
		$(CC) -M ${CFLAGS} $$i | \
		awk ' /[/]usr[/]include/ { next } \
			{ if ($$1 != prev) \
		    { if (rec != "") print rec; rec = $$0; prev = $$1; } \
		    else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		    else rec = rec " " $$2 } } \
		    END { print rec } ' >> makedep; \
	done
	echo '$$a' >eddep
	echo '' >>eddep
	echo '.' >>eddep
	echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >>eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	-rm -f eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it
funcdefs.o: funcdefs.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
funcdefs.o: ./iproc.h ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h
funcdefs.o: ./screen.h ./style.h ./externs.h ./ctype.h
abbrev.o: abbrev.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
abbrev.o: ./iproc.h ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h
abbrev.o: ./screen.h ./style.h ./externs.h ./fp.h ./ctype.h
argcount.o: argcount.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
argcount.o: ./iproc.h ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h
argcount.o: ./screen.h ./style.h ./externs.h
ask.o: ask.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
ask.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
ask.o: ./style.h ./externs.h ./termcap.h ./ctype.h ./chars.h ./disp.h
buf.o: buf.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
buf.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
buf.o: ./style.h ./externs.h ./ctype.h ./disp.h
c.o: c.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
c.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
c.o: ./style.h ./externs.h ./re.h ./ctype.h
case.o: case.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
case.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
case.o: ./style.h ./externs.h ./disp.h ./ctype.h
ctype.o: ctype.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
ctype.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
ctype.o: ./style.h ./externs.h ./ctype.h
delete.o: delete.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
delete.o: ./iproc.h ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h
delete.o: ./screen.h ./style.h ./externs.h ./disp.h
disp.o: disp.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
disp.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
disp.o: ./style.h ./externs.h ./ctype.h ./termcap.h ./chars.h ./fp.h ./disp.h
extend.o: extend.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
extend.o: ./iproc.h ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h
extend.o: ./screen.h ./style.h ./externs.h ./fp.h ./termcap.h ./ctype.h
extend.o: ./chars.h ./disp.h
fp.o: fp.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
fp.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
fp.o: ./style.h ./externs.h ./fp.h ./ctype.h ./termcap.h
fmt.o: fmt.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
fmt.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
fmt.o: ./style.h ./externs.h ./fp.h ./termcap.h ./ctype.h ./disp.h
insert.o: insert.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
insert.o: ./iproc.h ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h
insert.o: ./screen.h ./style.h ./externs.h ./ctype.h ./list.h ./chars.h
insert.o: ./disp.h
io.o: io.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
io.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
io.o: ./style.h ./externs.h ./list.h ./fp.h ./termcap.h ./ctype.h ./disp.h
io.o: ./io.h ./temp.h
iproc.o: iproc.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
iproc.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
iproc.o: ./style.h ./externs.h ./re.h ./ctype.h ./disp.h ./iproc-ptys.c
iproc.o: ./wait.h ./ttystate.h
jove.o: jove.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
jove.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
jove.o: ./style.h ./externs.h ./fp.h ./termcap.h ./ctype.h ./chars.h ./disp.h
jove.o: ./ttystate.h
list.o: list.c ./list.h ./tune.h ./sysdep.h
macros.o: macros.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
macros.o: ./iproc.h ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h
macros.o: ./screen.h ./style.h ./externs.h ./ctype.h ./fp.h ./chars.h ./disp.h
marks.o: marks.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
marks.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
marks.o: ./style.h ./externs.h
misc.o: misc.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
misc.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
misc.o: ./style.h ./externs.h ./ctype.h
move.o: move.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
move.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
move.o: ./style.h ./externs.h ./re.h ./ctype.h
paragraph.o: paragraph.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
paragraph.o: ./iproc.h ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h
paragraph.o: ./screen.h ./style.h ./externs.h
proc.o: proc.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
proc.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
proc.o: ./style.h ./externs.h ./ctype.h ./fp.h ./re.h ./termcap.h ./wait.h
re.o: re.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
re.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
re.o: ./style.h ./externs.h ./re.h ./ctype.h
re1.o: re1.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
re1.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
re1.o: ./style.h ./externs.h ./fp.h ./re.h ./ctype.h ./chars.h ./disp.h
rec.o: rec.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
rec.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
rec.o: ./style.h ./externs.h ./fp.h ./rec.h
scandir.o: scandir.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
scandir.o: ./iproc.h ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h
scandir.o: ./screen.h ./style.h ./externs.h
screen.o: screen.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
screen.o: ./iproc.h ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h
screen.o: ./screen.h ./style.h ./externs.h ./fp.h ./ctype.h ./termcap.h
screen.o: ./disp.h
term.o: term.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
term.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
term.o: ./style.h ./externs.h ./fp.h ./termcap.h
util.o: util.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
util.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
util.o: ./style.h ./externs.h ./ctype.h ./termcap.h ./disp.h
vars.o: vars.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
vars.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
vars.o: ./style.h ./externs.h
version.o: version.c
wind.o: wind.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
wind.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
wind.o: ./style.h ./externs.h ./termcap.h ./chars.h ./disp.h
getch.o: getch.c ./tune.h ./sysdep.h
mac.o: mac.c ./tune.h ./sysdep.h
keymaps.o: keymaps.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
keymaps.o: ./iproc.h ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h
keymaps.o: ./screen.h ./style.h ./externs.h ./list.h ./fp.h ./termcap.h
keymaps.o: ./chars.h
pcscr.o: pcscr.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./iproc.h
pcscr.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
pcscr.o: ./style.h ./externs.h
portsrv.o: portsrv.c ./tune.h ./sysdep.h
recover.o: recover.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
recover.o: ./iproc.h ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h
recover.o: ./screen.h ./style.h ./externs.h ./temp.h ./rec.h ./ctype.h
setmaps.o: setmaps.c ./funcdefs.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h
setmaps.o: ./io.h ./iproc.h ./dataobj.h ./keymaps.h ./argcount.h ./util.h
setmaps.o: ./vars.h ./screen.h ./style.h ./externs.h ./ctype.h
teachjove.o: teachjove.c
kbd.o: kbd.c ./tune.h ./sysdep.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
