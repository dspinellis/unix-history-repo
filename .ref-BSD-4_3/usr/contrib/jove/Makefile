#########################################################################
# This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    #
# provided to you without charge for use only on a licensed Unix        #
# system.  You may copy JOVE provided that this notice is included with #
# the copy.  You may not sell copies of this program or versions        #
# modified for use on microcomputer systems, unless the copies are      #
# included with a Unix system distribution and the source is provided.  #
#########################################################################

# Figure out the directories for things.  TMPDIR is where the tmp files get
# stored, usually /tmp or /tmp/jove.  RECDIR is where the tmp files should be
# moved when the system comes back up (and a call to recover -syscrash should
# be put in /etc/rc before /tmp is cleared).  If your system does not remove
# subdirectories of /tmp on reboot (lots do remove them these days) then it
# makes sense to make RECDIR and TMPDIR both be /tmp/jove.  That way you
# don't need the call to jove_recover in /etc/rc.  LIBDIR is for online
# documentation, the PORTSRV process, JOVE_RECOVER, and the system-wide
# .joverc file.  BINDIR is where to put the executables JOVE, JOVE_RECOVER
# and TEACHJOVE.  MANDIR is where the manual pages go for JOVE, JOVE_RECOVER
# and TEACHJOVE.  MANEXT is the extension for the man pages, e.g., jove.1 or
# jove.l or jove.m.

DESTDIR =
TMPDIR = /tmp
RECDIR = /tmp
LIBDIR = /usr/new/lib/jove
BINDIR = /usr/new
MANDIR = /usr/man/mann
MANEXT = n
SHELL = /bin/csh

# These should all just be right if the above ones are.
JOVE = $(DESTDIR)$(BINDIR)/jove
RECOVER = $(DESTDIR)$(BINDIR)/jove_recover
TEACHJOVE = $(DESTDIR)$(BINDIR)/teachjove
JOVERC = $(DESTDIR)$(LIBDIR)/.joverc
CMDS.DOC = $(DESTDIR)$(LIBDIR)/cmds.doc
TEACH-JOVE = $(DESTDIR)$(LIBDIR)/teach-jove
PORTSRV = $(DESTDIR)$(LIBDIR)/portsrv
JOVEM = $(DESTDIR)$(MANDIR)/jove.$(MANEXT)
RECOVERM = $(DESTDIR)$(MANDIR)/jove_recover.$(MANEXT)
TEACHJOVEM = $(DESTDIR)$(MANDIR)/teachjove.$(MANEXT)

# Select the right libraries for your system.
#	2.9BSD:	LIBS =	-ltermcap -ljobs
#	v7:	LIBS =	-ltermcap
#	4.1BSD:	LIBS =	-ltermcap -ljobs
#	4.2BSD:	LIBS =	-ltermcap
#	4.3BSD:	LIBS =	-ltermcap

LIBS = -ltermcap

# If you are not VMUNIX (vax running Berkeley Version 4), you must specify
# the -i flags (split I/D space) and maybe the -x option (for adb to work).
#	2.9BSD:	LDFLAGS = -x -i
#	v7:	LDFLAGS = -x -i
#	4.1BSD:	LDFLAGS =
#	4.2BSD:	LDFLAGS =
#	4.3BSD:	LDFLAGS =

LDFLAGS =

CFLAGS = -O
COFLAGS = -rworking -q

OBJECTS = keymaps.o funcdefs.o abbrev.o ask.o buf.o c.o case.o ctype.o \
	delete.o disp.o extend.o fp.o fmt.o insert.o io.o iproc.o jove.o macros.o \
	malloc.o marks.o misc.o move.o paragraph.o proc.o re.o re1.o rec.o \
	scandir.o screen.o term.o tune.o util.o vars.o version.o wind.o

JOVESRC = funcdefs.c abbrev.c ask.c buf.c c.c case.c ctype.c \
	delete.c disp.c extend.c fp.c fmt.c insert.c io.c iproc.c \
	jove.c macros.c malloc.c marks.c misc.c move.c paragraph.c \
	proc.c re.c re1.c rec.c scandir.c screen.c term.c util.c \
	vars.c version.c wind.c

SOURCES = $(JOVESRC) portsrv.c recover.c setmaps.c teachjove.c

HEADERS = ctype.h io.h jove.h re.h rec.h temp.h termcap.h tune.h

DOCS =	doc/cmds.doc.nr doc/example.rc doc/jove.1 doc/jove.2 doc/jove.3 \
	doc/jove.4 doc/jove.nr doc/recover.nr doc/system.rc doc/teach-jove \
	doc/teachjove.nr doc/README


BACKUPS = $(HEADERS) $(JOVESRC) iproc-pipes.c iproc-ptys.c \
	teachjove.c recover.c setmaps.c portsrv.c tune.template \
	Makefile Ovmakefile keymaps.txt README $(DOCS)


all:	xjove recover teachjove portsrv

xjove:	$(OBJECTS)
	$(CC) $(LDFLAGS) -o xjove $(OBJECTS) version.o $(LIBS)
	@-size xjove
	@-date

portsrv:	portsrv.o
	cc -o portsrv -n portsrv.o $(LIBS)

recover:	recover.o tune.o rec.h temp.h
	cc -o recover -n recover.o tune.o $(LIBS)

teachjove:	teachjove.o
	cc -o teachjove -n teachjove.o $(LIBS)

setmaps:	setmaps.o funcdefs.c
	cc -o setmaps setmaps.o

teachjove.o:	teachjove.c /usr/include/sys/types.h /usr/include/sys/file.h
	cc -c $(CFLAGS) -DTEACHJOVE=\"$(TEACH-JOVE)\" teachjove.c

setmaps.o:	funcdefs.c keymaps.txt

keymaps.c:	setmaps keymaps.txt
	setmaps < keymaps.txt > keymaps.c

keymaps.o:	keymaps.c jove.h

tune.c: Makefile tune.template
	@echo "/* Changes should be made in Makefile, not to this file! */" > tune.c
	@echo "" >> tune.c
	@sed -e 's;TMPDIR;$(TMPDIR);' \
	     -e 's;RECDIR;$(RECDIR);' \
	     -e 's;LIBDIR;$(LIBDIR);' \
	     -e 's;BINDIR;$(BINDIR);' \
	     -e 's;SHELL;$(SHELL);' tune.template >> tune.c

install: $(DESTDIR)$(LIBDIR) $(TEACH-JOVE) $(CMDS.DOC) $(JOVERC) \
	 $(PORTSRV) $(RECOVER) $(JOVE) $(TEACHJOVE) $(JOVEM) \
	 $(RECOVERM) $(TEACHJOVEM)

$(DESTDIR)$(LIBDIR):
	-mkdir $(DESTDIR)$(LIBDIR)

$(TEACH-JOVE): doc/teach-jove
	install -c -m 644 doc/teach-jove $(TEACH-JOVE)

doc/cmds.doc:	doc/cmds.doc.nr doc/jove.4
	nroff doc/cmds.doc.nr doc/jove.4 > doc/cmds.doc

$(CMDS.DOC): doc/cmds.doc
	install -c -m 644 doc/cmds.doc $(CMDS.DOC)

$(JOVERC): doc/system.rc
	install -c -m 644 doc/system.rc $(JOVERC)

$(PORTSRV): portsrv
	install -c -s -m 755 portsrv $(PORTSRV)

$(RECOVER): recover
	install -c -s -m 755 recover $(RECOVER)

$(JOVE): xjove
	install -c -m 755 xjove $(JOVE)

$(TEACHJOVE): teachjove
	install -c -s -m 755 teachjove $(TEACHJOVE)

$(JOVEM): doc/jove.nr
	@sed -e 's;TMPDIR;$(TMPDIR);' \
	     -e 's;RECDIR;$(RECDIR);' \
	     -e 's;LIBDIR;$(LIBDIR);' \
	     -e 's;SHELL;$(SHELL);' doc/jove.nr > /tmp/jove.nr
	install -m 644 /tmp/jove.nr $(JOVEM)

$(RECOVERM): doc/recover.nr
	@sed -e 's;TMPDIR;$(TMPDIR);' \
	     -e 's;RECDIR;$(RECDIR);' \
	     -e 's;LIBDIR;$(LIBDIR);' \
	     -e 's;SHELL;$(SHELL);' doc/recover.nr > /tmp/recover.nr
	install -m 644 /tmp/recover.nr $(RECOVERM)

$(TEACHJOVEM): doc/teachjove.nr
	@sed -e 's;TMPDIR;$(TMPDIR);' \
	     -e 's;RECDIR;$(RECDIR);' \
	     -e 's;LIBDIR;$(LIBDIR);' \
	     -e 's;SHELL;$(SHELL);' doc/teachjove.nr > /tmp/teachjove.nr
	install -m 644 /tmp/teachjove.nr $(TEACHJOVEM)

echo:
	@echo $(C-FILES) $(HEADERS)

lint:
	lint -n $(JOVESRC) tune.c keymaps.c
	@echo Done

tags:
	ctags -w $(JOVESRC) $(HEADERS)

ciall:
	ci $(BACKUPS)

coall:
	co $(BACKUPS)

jove.shar:
	shar $(BACKUPS) > jove.shar

backup:
	tar cf backup $(BACKUPS)

tape-backup:
	tar cbf 20 /dev/rmt0 $(BACKUPS)

touch:
	touch $(OBJECTS)

clean:
	rm -f a.out core *.o keymaps.c tune.c xjove portsrv recover setmaps \
	teachjove

# This version only works under 4.3BSD
depend:
	for i in ${SOURCES} ; do \
		cc -M ${CFLAGS} $$i | awk ' { if ($$1 != prev) \
		    { if (rec != "") print rec; rec = $$0; prev = $$1; } \
		    else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		    else rec = rec " " $$2 } } \
		    END { print rec } ' >> makedep; done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it
# DEPENDENCIES MUST END AT END OF FILE
funcdefs.o: funcdefs.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h
funcdefs.o: ./tune.h /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
funcdefs.o: /usr/include/sys/ttydev.h
abbrev.o: abbrev.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h
abbrev.o: ./tune.h ./io.h ./ctype.h
ask.o: ask.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
ask.o: ./termcap.h ./ctype.h /usr/include/signal.h /usr/include/varargs.h
ask.o: /usr/include/sys/stat.h
buf.o: buf.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
buf.o: /usr/include/sys/stat.h
c.o: c.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
c.o: ./re.h ./ctype.h
case.o: case.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
case.o: ./ctype.h
ctype.o: ctype.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h
ctype.o: ./tune.h ./ctype.h
delete.o: delete.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h
delete.o: ./tune.h
disp.o: disp.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
disp.o: ./ctype.h ./termcap.h /usr/include/varargs.h /usr/include/signal.h
disp.o: /usr/include/sys/stat.h
extend.o: extend.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h
extend.o: ./tune.h ./io.h ./termcap.h ./ctype.h /usr/include/signal.h
extend.o: /usr/include/varargs.h
fp.o: fp.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
fp.o: ./io.h ./termcap.h /usr/include/sys/stat.h /usr/include/sys/file.h
fp.o: /usr/include/errno.h
fmt.o: fmt.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
fmt.o: ./io.h ./termcap.h /usr/include/varargs.h
insert.o: insert.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h
insert.o: ./tune.h ./ctype.h
io.o: io.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
io.o: ./io.h ./termcap.h /usr/include/signal.h /usr/include/sys/stat.h
io.o: /usr/include/sys/file.h /usr/include/errno.h ./temp.h
iproc.o: iproc.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h
iproc.o: ./tune.h ./iproc-ptys.c /usr/include/sys/wait.h /usr/include/signal.h
iproc.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
iproc.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
jove.o: jove.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
jove.o: ./io.h ./termcap.h /usr/include/varargs.h /usr/include/sys/stat.h
jove.o: /usr/include/signal.h /usr/include/errno.h /usr/include/sgtty.h
jove.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
jove.o: /usr/include/sys/ttydev.h /usr/include/fcntl.h
macros.o: macros.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h
macros.o: ./tune.h
malloc.o: malloc.c ./tune.h
marks.o: marks.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h
marks.o: ./tune.h
misc.o: misc.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
misc.o: ./ctype.h /usr/include/signal.h ./termcap.h
move.o: move.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
move.o: ./ctype.h
paragraph.o: paragraph.c ./jove.h /usr/include/setjmp.h
paragraph.o: /usr/include/sys/types.h ./tune.h
proc.o: proc.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
proc.o: ./io.h ./termcap.h /usr/include/signal.h /usr/include/sys/wait.h
re.o: re.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
re.o: ./ctype.h
re1.o: re1.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
re1.o: ./io.h ./re.h
rec.o: rec.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
rec.o: ./io.h ./temp.h ./rec.h /usr/include/sys/file.h
scandir.o: scandir.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h
scandir.o: ./tune.h /usr/include/sys/stat.h /usr/include/sys/dir.h
screen.o: screen.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h
screen.o: ./tune.h ./io.h ./ctype.h ./temp.h ./termcap.h
term.o: term.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
term.o: /usr/include/errno.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
term.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
term.o: /usr/include/signal.h
util.o: util.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
util.o: ./ctype.h /usr/include/signal.h /usr/include/nlist.h
util.o: /usr/include/sys/time.h /usr/include/time.h
vars.o: vars.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
version.o: version.c
wind.o: wind.c ./jove.h /usr/include/setjmp.h /usr/include/sys/types.h ./tune.h
wind.o: ./termcap.h
portsrv.o: portsrv.c ./tune.h
recover.o: recover.c /usr/include/stdio.h ./jove.h /usr/include/setjmp.h
recover.o: /usr/include/sys/types.h ./tune.h ./temp.h ./rec.h
recover.o: /usr/include/signal.h /usr/include/sys/file.h
recover.o: /usr/include/sys/stat.h /usr/include/sys/dir.h ./ctype.h
setmaps.o: setmaps.c ./funcdefs.c ./jove.h /usr/include/setjmp.h
setmaps.o: /usr/include/sys/types.h ./tune.h /usr/include/sys/ioctl.h
setmaps.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
setmaps.o: /usr/include/stdio.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
