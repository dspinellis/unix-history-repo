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
# RECDIR is the directory in which RECOVER looks for JOVE's tempfiles.
#
# JOVEHOME is the directory in which pieces of JOVE are kept.  It is only used
#	in the default definitions of SHAREDIR, LIBDIR, BINDIR, and MANDIR.
# SHAREDIR is for online documentation, and the system-wide .joverc file.
# LIBDIR is for the PORTSRV and KBD processes and RECOVER.
# BINDIR is where to put the executables JOVE and TEACHJOVE.
# MANDIR is where the manual pages go for JOVE, RECOVER and TEACHJOVE.
# MANEXT is the extension for the man pages, e.g., jove.1 or jove.l or jove.m.
# DFLTSHELL is the default shell invoked by JOVE and TEACHJOVE.
#
# If they don't exist, this makefile will try to create the directories
# LIBDIR and SHAREDIR.  All others must already exist.

TMPDIR = /var/tmp
RECDIR = /var/preserve

JOVEHOME = /usr/contrib
SHAREDIR = $(JOVEHOME)/lib/jove
LIBDIR = $(JOVEHOME)/lib/jove
BINDIR = $(JOVEHOME)/bin
MANDIR = $(JOVEHOME)/man/cat1
MANEXT = 0
DFLTSHELL = /bin/csh

PROG = jove
VERSION = 4.14

# These should all just be right if the above ones are.
JOVE = $(BINDIR)/jove
TEACHJOVE = $(BINDIR)/teachjove
RECOVER = $(LIBDIR)/recover
PORTSRV = $(LIBDIR)/portsrv
KBD = $(LIBDIR)/kbd
JOVERC = $(SHAREDIR)/jove.rc
CMDS.DOC = $(SHAREDIR)/cmds.doc
TEACH-JOVE = $(SHAREDIR)/teach-jove
JOVEM = $(MANDIR)/jove.$(MANEXT)
TEACHJOVEM = $(MANDIR)/teachjove.$(MANEXT)

# Select the right libraries for your system.
#	2.10BSD:LIBS = -ltermcap
#	v7:	LIBS = -ltermcap
#	4.1BSD:	LIBS = -ltermcap -ljobs
#	4.2BSD:	LIBS = -ltermcap
#	4.3BSD:	LIBS = -ltermcap
#	SysV Rel. 2: LIBS = -lcurses
#	SCO Xenix: LIBS = -ltermcap -lx
#	SCO: LIBS = -lcurses
#	AIX on the R6000s: LIBS = -lcurses
#	MIPS: LIBS = -ltermcap

LIBS = -ltermcap

#	2.10BSD:LDFLAGS =
#	v7:	LDFLAGS =
#	4.1BSD:	LDFLAGS =
#	4.2BSD:	LDFLAGS =
#	4.3BSD:	LDFLAGS =
#	SysV Rel. 2: LDFLAGS = -Ml
#	SCO Xenix: LDFLAGS = -Ml -F 3000
#	SCO Unix: LDFLAGS = 
#
# SEPFLAG should be:
#	not on a PDP-11:		SEPFLAG =
#	PDP-11 with separate I&D:	SEPFLAG = -i
#	PDP-11 without separate I&D:	SEPFLAG = -n
#

LDFLAGS =

SEPFLAG =

# define a symbol for your OS if it hasn't got one. sysdep.h tries to
# use cpp predefined symbols to decide on the appropriate behaviour
# in most cases. Exceptions are
# 	Apple A/UX on macIIs 		SYSDEFS=-DA_UX
#	SunOS4.0			SYSDEFS=-DSUNOS4
#	MIPS RiscOS4.x			-systype bsd43
#	SCO Unix			SYSDEFS=-DSCO -DSYSV
#	IBM RS6000s			SYSDEFS=-DAIX -D_BSD -D_BSD_INCLUDES -D_NO_PROTO
#	A system V system that doesn't
#	define one of SVR2,SVR3,SYSV	SYSDEFS=-DSYSV
#
# You can just say 'make SYSDEFS=-Dwhatever' on these systems.
# 
SYSDEFS = -DBSD386

# for SCO Xenix, set
#	MEMFLAGS = -Mle
#	CFLAGS = -LARGE -O -F 3000 -K -Mle  (say -Mle2 for an 80286)
CC=gcc
CFLAGS = -O $(SYSDEFS)

# For cross compiling Jove, set CC to the cross compiler, and LOCALC
# to the local C compiler. LOCALCC will be used for compiling setmaps,
# which is run as part of the compilation to generate the keymaps.
# Set LOCALCFLAGS and LOCALLDFLAGS appropriately too. For Xenix, note
# that LOCALCFLAGS must be set to $(MEMFLAGS)

LOCALCC = $(CC)
LOCALCFLAGS = $(CFLAGS)	# $(MEMFLAGS)
LOCALLDFLAGS = $(LDFLAGS)

BASESEG = funcdefs.o keys.o argcount.o ask.o buf.o ctype.o delete.o \
	  disp.o insert.o io.o jove.o loadavg.o marks.o misc.o re.o \
	  screen.o tune.o util.o vars.o version.o list.o keymaps.o
OVLAY1 = abbrev.o rec.o paragraph.o fmt.o
OVLAY2 = c.o wind.o fp.o move.o
OVLAY3 = extend.o macros.o
OVLAY4 = iproc.o re1.o
OVLAY5 = proc.o scandir.o term.o case.o

OBJECTS = $(BASESEG) $(OVLAY1) $(OVLAY2) $(OVLAY3) $(OVLAY4) $(OVLAY5)

C_SRC = funcdefs.c abbrev.c argcount.c ask.c buf.c c.c case.c ctype.c \
	delete.c disp.c extend.c fp.c fmt.c insert.c io.c iproc.c \
	jove.c list.c loadavg.c macros.c marks.c misc.c move.c paragraph.c \
	proc.c re.c re1.c rec.c scandir.c screen.c term.c util.c \
	vars.c version.c wind.c getch.c mac.c keymaps.c pcscr.c

SOURCES = $(C_SRC) portsrv.c recover.c setmaps.c teachjove.c kbd.c

HEADERS = argcount.h buf.h chars.h ctype.h dataobj.h disp.h \
	externs.h fp.h io.h iproc.h jove.h keymaps.h list.h mac.h \
	re.h rec.h scandir.h screen.h sysdep.h temp.h termcap.h \
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
	$(LOCALCC) $(LOCALLDFLAGS) -o setmaps setmaps.o

teachjove.o:	teachjove.c /usr/include/sys/types.h /usr/include/sys/file.h
	$(CC) -c $(CFLAGS) -DTEACHJOVE=\"$(TEACH-JOVE)\" teachjove.c

# don't optimize setmaps.c because it produces bad code in some places
# for some reason
setmaps.o:	funcdefs.c keys.txt
	$(LOCALCC) $(LOCALCFLAGS) -c setmaps.c

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
	     -e 's;SHELL;$(DFLTSHELL);' tune.template >> tune.c

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

install: $(JOVEHOME) $(LIBDIR) $(SHAREDIR) $(MANDIR) \
	 $(TEACH-JOVE) $(CMDS.DOC) $(JOVERC) \
	 $(PORTSRV) $(KBD) $(RECOVER) $(JOVE) $(TEACHJOVE) $(JOVEM) \
	 $(RECOVERM) $(TEACHJOVEM)
	@echo See the README about changes to /etc/rc or /etc/rc.local
	@echo so that the system recovers jove files on reboot after a crash

$(JOVEHOME)::
	-mkdir $(JOVEHOME)

$(LIBDIR)::
	-mkdir $(LIBDIR)

$(SHAREDIR)::
	-mkdir $(SHAREDIR)

$(MANDIR)::
	-mkdir $(MANDIR)

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
	install -c -m 755 xjove $(JOVE)

$(TEACHJOVE): teachjove
	install -c -s -m 755 teachjove $(TEACHJOVE)

$(JOVEM): doc/jove.nr
	@sed -e 's;TMPDIR;$(TMPDIR);' \
	     -e 's;LIBDIR;$(LIBDIR);' \
	     -e 's;SHAREDIR;$(SHAREDIR);' \
	     -e 's;SHELL;$(DFLTSHELL);' doc/jove.nr > /tmp/jove.nr
	nroff -mandoc /tmp/jove.nr > $(JOVEM)
	-rm -f /tmp/jove.nr

$(TEACHJOVEM): doc/teachjove.nr
	@sed -e 's;TMPDIR;$(TMPDIR);' \
	     -e 's;LIBDIR;$(LIBDIR);' \
	     -e 's;SHAREDIR;$(SHAREDIR);' \
	     -e 's;SHELL;$(DFLTSHELL);' doc/teachjove.nr > /tmp/teachjove.nr
	nroff -mandoc /tmp/teachjove.nr > $(TEACHJOVEM)
	-rm -f /tmp/teachjove.nr

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
	@tar cvf - `find . -type f -print | \
		egrep -v '(,v|\.o|xjove|kbd|portsrv|setmaps|~)$$' | \
		sort`

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
	teachjove macvert nrectune.h rectune.h \#* *~ make.log

clobber: clean
	rm -f *.orig *.rej

# This version only works under 4.3BSD
depend:
	@echo '"make depend" only works under 4.3BSD'
	sed -e '/^# DO NOT DELETE THIS LINE/q' Makefile >Makefile.new
	for i in ${SOURCES} ; do \
		cc -M ${CFLAGS} $$i | \
		awk ' /[/]usr[/]include/ { next } \
			{ if ($$1 != prev) \
		    { if (rec != "") print rec; rec = $$0; prev = $$1; } \
		    else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		    else rec = rec " " $$2 } } \
		    END { print rec } ' >>Makefile.new; \
	done
	echo '# DEPENDENCIES MUST END AT END OF FILE' >>Makefile.new
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >>Makefile.new
	echo '# see "make depend" above' >>Makefile.new
	@echo 'New makefile is in "Makefile.new".  Move it to "Makefile".'

# DO NOT DELETE THIS LINE -- "make depend" uses it
funcdefs.o: funcdefs.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
funcdefs.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
funcdefs.o: ./externs.h ./ctype.h
abbrev.o: abbrev.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
abbrev.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
abbrev.o: ./externs.h ./fp.h ./ctype.h
argcount.o: argcount.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
argcount.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
argcount.o: ./externs.h ./ctype.h
ask.o: ask.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
ask.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
ask.o: ./termcap.h ./ctype.h ./chars.h ./disp.h ./fp.h ./scandir.h
buf.o: buf.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
buf.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
buf.o: ./ctype.h ./disp.h ./fp.h ./iproc.h
c.o: c.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
c.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h ./re.h
c.o: ./ctype.h ./disp.h
case.o: case.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
case.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
case.o: ./disp.h ./ctype.h
ctype.o: ctype.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
ctype.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
ctype.o: ./externs.h ./ctype.h
delete.o: delete.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
delete.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
delete.o: ./externs.h ./disp.h
disp.o: disp.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
disp.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
disp.o: ./ctype.h ./termcap.h ./chars.h ./fp.h ./disp.h ./iproc.h
extend.o: extend.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
extend.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
extend.o: ./externs.h ./fp.h ./termcap.h ./ctype.h ./chars.h ./disp.h ./re.h
fp.o: fp.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
fp.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h ./fp.h
fp.o: ./ctype.h ./termcap.h ./disp.h
fmt.o: fmt.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
fmt.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h ./fp.h
fmt.o: ./termcap.h ./ctype.h ./disp.h
insert.o: insert.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
insert.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
insert.o: ./externs.h ./ctype.h ./list.h ./chars.h ./disp.h ./re.h
io.o: io.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
io.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
io.o: ./list.h ./fp.h ./termcap.h ./ctype.h ./disp.h ./scandir.h ./temp.h
iproc.o: iproc.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
iproc.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
iproc.o: ./externs.h ./re.h ./ctype.h ./disp.h ./fp.h ./iproc.h ./iproc-ptys.c
iproc.o: ./wait.h ./ttystate.h
jove.o: jove.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
jove.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
jove.o: ./fp.h ./termcap.h ./ctype.h ./chars.h ./disp.h ./re.h ./rec.h
jove.o: ./iproc.h ./ttystate.h
list.o: list.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
list.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
list.o: ./list.h
loadavg.o: loadavg.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
loadavg.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
loadavg.o: ./externs.h
macros.o: macros.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
macros.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
macros.o: ./externs.h ./ctype.h ./fp.h ./chars.h ./disp.h
marks.o: marks.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
marks.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
marks.o: ./externs.h
misc.o: misc.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
misc.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
misc.o: ./ctype.h ./disp.h
move.o: move.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
move.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
move.o: ./re.h ./ctype.h ./disp.h
paragraph.o: paragraph.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
paragraph.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
paragraph.o: ./externs.h ./disp.h
proc.o: proc.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
proc.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
proc.o: ./ctype.h ./fp.h ./re.h ./termcap.h ./disp.h ./rec.h ./wait.h
re.o: re.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
re.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h ./re.h
re.o: ./ctype.h
re1.o: re1.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
re1.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h ./fp.h
re1.o: ./re.h ./ctype.h ./chars.h ./disp.h
rec.o: rec.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
rec.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h ./fp.h
rec.o: ./rec.h
scandir.o: scandir.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
scandir.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
scandir.o: ./externs.h ./scandir.h
screen.o: screen.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
screen.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
screen.o: ./externs.h ./fp.h ./ctype.h ./termcap.h ./disp.h
term.o: term.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
term.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
term.o: ./fp.h ./disp.h ./ctype.h ./termcap.h
util.o: util.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
util.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
util.o: ./ctype.h ./termcap.h ./disp.h ./fp.h
vars.o: vars.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
vars.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
version.o: version.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
version.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
version.o: ./externs.h
wind.o: wind.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
wind.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
wind.o: ./termcap.h ./chars.h ./disp.h
getch.o: getch.c ./tune.h ./sysdep.h
mac.o: mac.c ./tune.h ./sysdep.h
keymaps.o: keymaps.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
keymaps.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
keymaps.o: ./externs.h ./list.h ./fp.h ./termcap.h ./chars.h ./disp.h ./re.h
pcscr.o: pcscr.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
pcscr.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
pcscr.o: ./externs.h
portsrv.o: portsrv.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
portsrv.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
portsrv.o: ./externs.h
recover.o: recover.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h
recover.o: ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h
recover.o: ./externs.h ./temp.h ./rec.h ./ctype.h ./scandir.c ./scandir.h
setmaps.o: setmaps.c ./funcdefs.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h
setmaps.o: ./io.h ./dataobj.h ./keymaps.h ./argcount.h ./util.h ./vars.h
setmaps.o: ./screen.h ./externs.h ./ctype.h
teachjove.o: teachjove.c
kbd.o: kbd.c ./jove.h ./tune.h ./sysdep.h ./buf.h ./wind.h ./io.h ./dataobj.h
kbd.o: ./keymaps.h ./argcount.h ./util.h ./vars.h ./screen.h ./externs.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see "make depend" above
