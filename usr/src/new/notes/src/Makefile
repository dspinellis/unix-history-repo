#	Makefile for University of Illinois Notesfiles
#
# $Header: Makefile,v 1.7.0.5 85/10/09 18:11:23 notes Rel $
#
#	Ray Essick
#	214 DCL
#	University of Illinois at Urbana-Champaign
#	1304 W. Springfield
#	Urbana, Illinois 61801
#	217-333-7937
#

BIN = /usr/local
MSTDIR = /usr/spool/notes
ARCHDIR = /usr/spool/oldnotes
NET = /usr/bin
AUTOSEQ = autoseq
NOTES = notes
NOTESUID = 10
NOTESGRP = notes
ANON = anon
ANONUID = 4
LIBDIR = /usr/local/lib
CC = cc
CFLAGS = -DMSTDIR=\"$(MSTDIR)\" -DARCHDIR=\"$(ARCHDIR)\" \
	-DNOTES=\"$(NOTES)\" -DNOTESUID=$(NOTESUID) \
	-DANON=\"$(ANON)\" -DANONUID=$(ANONUID) \
	-DBIN=\"$(BIN)\" -DRCSIDENT -O
LFLAGS = -n
#	-n is shared, read-only text segment
TERMCAP = -ltermcap
# Evidently on System V (and other USG systems?) the termcap routines
# are in /usr/lib/libcurses.a

#
#	The following probably don't need to be modified
#
UTILITY = $(MSTDIR)/.utilities
SEQUENCER = $(MSTDIR)/.sequencer
LOCKS = $(MSTDIR)/.locks
ALIASES = $(UTILITY)/net.aliases
UNIQID = $(MSTDIR)/.SEQ

# directory where the "release" copy is kept
DISTDIR= p:/usr/src/uiuc/notesfiles
DCP=	rcp
RCSID = 1.7.0
RCSSTATE = Rel
RCSIFLAGS = -r$(RCSID) -s$(RCSSTATE)
RCSOFLAGS = -r$(RCSID) -s$(RCSSTATE)

#
#	Define some collections of routines so that things will
#	be easy to maintain
#

MISCFILES = Makefile rinstall getdate.y
HFILES = canon.h doc.h net.h newsgate.h parms.h structs.h dump.h
HLPFILES = access.help dir.help index.help lim.help read.help
CFILES = access.c adnote.c adresp.c archiver.c asearch.c buildnf.c check.c \
	checknotes.c compress.c control.c cursor.c datein.c delete.c \
	dmpnote.c dmpresp.c dropt.c dsply.c find.c gdate.c getnet.c \
	gname.c gtext.c gtime.c help.c index.c ldmisc.c limindx.c loadem.c \
	lprnote.c lrsp.c mailit.c main.c misc.c miscio.c mknf.c newsdump.c \
	newsgroup.c newsinput.c newsout.c newsoutput.c next.c nfalias.c \
	nfarchive.c nfcomment.c nfmail.c nfpipe.c nfprint.c nfrcv.c nfsend.c \
	nfstats.c nfxmit.c note.c pagein.c pageout.c pattern.c permit.c \
	perms.c preptxt.c prtind.c readem.c recsio.c resp.c rmnf.c \
	savtxt.c startup.c talkto.c times.c tsearch.c \
	expand.c newspath.c nfaccess.c acssort.c nfabort.c \
	nfdump.c dump.c nfload.c loadnf.c parse.c loadvars.c \
	bnewshead.c loadproto1.c ftime.c parsepath.c pagemove.c
USERCMDS = main nfpipe nfprint nfstats checknotes
NETCMDS = nfxmit nfrcv newsinput newsoutput
UTILCMDS = mknf rmnf nfarchive nfaccess nfmail
MISCCMDS = nfload nfdump
CMDS =	$(USERCMDS) $(NETCMDS) $(UTILCMDS) $(MISCCMDS)

#
#	Objects (other than the mainline) to load with each program
#

CHECKNOTES = check.o cursor.o gname.o gtime.o lrsp.o misc.o miscio.o \
	  next.o pattern.o perms.o recsio.o startup.o times.o expand.o
MAIN	= access.o acssort.o adnote.o adresp.o asearch.o check.o compress.o \
	  control.o cursor.o delete.o dropt.o dsply.o gdate.o gname.o \
	  gtext.o gtime.o help.o index.o limindx.o lrsp.o mailit.o \
	  misc.o miscio.o next.o note.o pagein.o pageout.o pattern.o \
	  permit.o perms.o preptxt.o prtind.o readem.o recsio.o resp.o \
	  savtxt.o startup.o talkto.o times.o tsearch.o \
	  expand.o datein.o getdate.o ftime.o pagemove.o
MKNF	= buildnf.o check.o cursor.o gname.o gtime.o misc.o \
	  miscio.o startup.o acssort.o recsio.o perms.o
NEWSINPUT = buildnf.o check.o cursor.o datein.o find.o gname.o gtime.o \
	  lrsp.o misc.o miscio.o newsgroup.o note.o pagein.o permit.o \
	  perms.o recsio.o resp.o startup.o tsearch.o acssort.o \
	  expand.o bnewshead.o pattern.o getdate.o ftime.o parsepath.o
NEWSOUTPUT = check.o cursor.o gname.o gtime.o lrsp.o misc.o miscio.o \
	  newsdump.o newsgroup.o newsout.o newspath.o next.o pageout.o \
	  pattern.o perms.o recsio.o startup.o times.o expand.o \
	  getnet.o datein.o getdate.o ftime.o
NFACCESS = acssort.o check.o cursor.o gname.o gtime.o misc.o miscio.o \
	  pattern.o permit.o perms.o recsio.o startup.o expand.o
NFARCHIVE = archiver.o buildnf.o check.o compress.o cursor.o dmpnote.o \
	  dmpresp.o gname.o gtime.o lrsp.o misc.o miscio.o \
	  next.o nfalias.o note.o pagein.o pageout.o pattern.o permit.o \
	  perms.o recsio.o resp.o startup.o expand.o acssort.o find.o \
	  pagemove.o
NFMAIL	= gname.o gtime.o note.o pagein.o recsio.o resp.o startup.o \
	  tsearch.o check.o cursor.o misc.o miscio.o perms.o parsepath.o
NFPIPE	= check.o cursor.o gname.o gtime.o misc.o miscio.o note.o \
	  pagein.o permit.o perms.o recsio.o startup.o
NFPRINT	= check.o cursor.o gname.o gtime.o lprnote.o lrsp.o \
	  misc.o miscio.o pageout.o permit.o perms.o recsio.o \
	  startup.o
NFRCV	= buildnf.o check.o cursor.o find.o gname.o gtime.o ldmisc.o \
	  loadem.o lrsp.o misc.o miscio.o note.o permit.o perms.o \
	  recsio.o resp.o startup.o acssort.o
NFSTATS	= check.o cursor.o gname.o gtime.o misc.o miscio.o \
	  pattern.o perms.o recsio.o startup.o expand.o
NFXMIT	= check.o cursor.o dmpnote.o dmpresp.o getnet.o gname.o \
	  gtime.o lrsp.o misc.o miscio.o next.o nfalias.o nfsend.o \
	  pageout.o pattern.o permit.o perms.o recsio.o \
	  startup.o times.o expand.o datein.o getdate.o ftime.o
RMNF	= check.o cursor.o misc.o miscio.o startup.o gname.o \
	  expand.o pattern.o
LIBOBJS	= nfabort.o nfcomment.o
#
NFDUMP	= dump.o dmpnote.o dmpresp.o check.o recsio.o note.o resp.o \
	  lrsp.o startup.o misc.o miscio.o pageout.o gname.o perms.o \
	  permit.o gtime.o cursor.o
NFLOAD	= loadvars.o loadnf.o buildnf.o recsio.o gname.o gtime.o \
	  startup.o misc.o miscio.o cursor.o loadem.o parse.o acssort.o \
	  check.o perms.o permit.o note.o ldmisc.o find.o resp.o \
	  lrsp.o datein.o getdate.o ftime.o

bin:	user network utility
	@echo notesfile binaries up-to-date
	touch bin

all:	bin
	touch all

clean:
	rm -f *.o $(CMDS) libnfcom.a
#	DO NOT remove "boot" and "spool".
	rm -f placebin placeuser placenet placeutil placelib placedoc \
		bin user network utility install print all
	rm -f Make.errs

sizes:
	size $(CMDS)

help:
	@echo type make base as su to make notes directories.
	@echo type make install to compile and install code and man pages.
	@echo type make bin to merely make binaries.
	@echo for more info, see the Notesfile Reference Manual.

user:	$(USERCMDS)
	touch user

network: $(NETCMDS)
	touch network

utility: $(UTILCMDS)
	touch utility

#	make base should only be run once!
#	This set of commands sets up the required files down in the
#	binary directories. It's main purpose is to do the several
#	installation commands that might just have to be super-user.

base:
	mkdir $(MSTDIR) $(ARCHDIR)
	-chown $(NOTES) $(MSTDIR) $(ARCHDIR)
	-chgrp $(NOTESGRP) $(MSTDIR) $(ARCHDIR)
	(cd $(BIN); touch notes nfprint nfstats checknotes nfpipe)
	-(cd $(BIN); chown $(NOTES) notes nfprint nfstats nfpipe checknotes)
	-(cd $(BIN); chgrp $(NOTESGRP) notes nfprint nfstats nfpipe checknotes)
#	establish "aliases" for auto-sequencing entry points
#	make more links with other names if you want them.
	ln $(BIN)/notes $(BIN)/$(AUTOSEQ)
	-(cd $(NET); touch nfxmit nfrcv)
	-(cd $(NET); chown $(NOTES) nfxmit nfrcv)
	-(cd $(NET); chgrp $(NOTESGRP) nfxmit nfrcv)
	touch $(LIBDIR)/libnfcom.a
	-chown $(NOTES) $(LIBDIR)/libnfcom.a
	-chgrp $(NOTESGRP) $(LIBDIR)/libnfcom.a
	touch base
	@echo base generation done

boot:	spool install
#	make the nfgripes file and open it
	-mknf -o nfgripes
	-mknf -on nfmaint
	@echo notesfile system booted

spool:
	mkdir $(UTILITY) $(SEQUENCER) $(LOCKS) $(ALIASES)
	-chown $(NOTES) $(UTILITY) $(SEQUENCER) $(LOCKS) $(ALIASES)
	-chgrp $(NOTESGRP) $(UTILITY) $(SEQUENCER) $(LOCKS) $(ALIASES)
	chmod 775 $(MSTDIR) $(UTILITY) $(SEQUENCER) $(LOCKS) $(ALIASES)
	mkdir $(UTILITY)/coredump
	chmod 777 $(UTILITY)/coredump
	echo 1 > $(UNIQID)
	chmod 660 $(UNIQID)
	-chown $(NOTES) $(UTILITY)/coredump $(UNIQID)
	-chgrp $(NOTESGRP) $(UTILITY)/coredump $(UNIQID)
#	generate the available notesfile file here - only once!
	echo "available notesfiles" > $(UTILITY)/avail.notes
	touch $(UTILITY)/Dflt-Seq
	-chown $(NOTES) $(UTILITY)/avail.notes $(UTILITY)/Dflt-Seq
	-chgrp $(NOTESGRP) $(UTILITY)/avail.notes $(UTILITY)/Dflt-Seq
	chmod 664 $(UTILITY)/avail.notes $(UTILITY)/Dflt-Seq
	touch spool
	@echo Notesfile Spool directories ready

#	make sure that all the appropriate directories exist
dirs:	$(MSTDIR) $(ARCHDIR) $(UTILITY) $(SEQUENCER) $(LOCKS) \
	$(ALIASES) $(UTILITY)/coredump
	touch dirs

$(MSTDIR) $(ARCHDIR) $(UTILITY) $(SEQUENCER) $(LOCKS) $(ALIASES):
	mkdir $@
	-chown $(NOTES) $@
	-chgrp $(NOTESGRP) $@
	chmod 775 $@

$(UTILITY)/coredump:
	mkdir $@
	-chown $(NOTES) $@
	-chgrp $(NOTESGRP) $@
	chmod 777 $@

install: dirs placedoc placelib placebin
	touch install
	@echo notesfiles installed

placedoc:	access.help dir.help index.help lim.help read.help
	cp $? $(UTILITY)
	-(cd $(UTILITY); chmod 644 $?)
	-(cd $(UTILITY); chown $(NOTES) $?; chgrp $(NOTESGRP) $?)
	@echo notesfile help files installed
	touch placedoc

placelib: libnfcom.a
	cp libnfcom.a $(LIBDIR)/libnfcom.a
	-ranlib $(LIBDIR)/libnfcom.a
	touch placelib

cp:	placebin

placebin: $(BIN)/notes $(BIN)/nfpipe $(BIN)/nfprint \
	  $(BIN)/nfstats $(BIN)/checknotes $(UTILITY)/mknf \
	  $(UTILITY)/rmnf $(UTILITY)/nfarchive $(UTILITY)/nfaccess \
	  $(UTILITY)/nfmail $(NET)/nfxmit $(NET)/nfrcv \
	  $(UTILITY)/newsinput $(UTILITY)/newsoutput
	@echo notesfile binaries installed
	touch placebin

$(BIN)/notes:	main
	cp main $(BIN)/notes
	strip $(BIN)/notes
	-chown $(NOTES) $(BIN)/notes
	-chgrp $(NOTESGRP) $(BIN)/notes
	chmod 2755 $(BIN)/notes

$(BIN)/nfpipe:	nfpipe
	cp nfpipe $(BIN)/nfpipe
	strip $(BIN)/nfpipe
	-chown $(NOTES) $(BIN)/nfpipe
	-chgrp $(NOTESGRP) $(BIN)/nfpipe
	chmod 2755 $(BIN)/nfpipe

$(BIN)/nfstats:	nfstats
	cp nfstats $(BIN)/nfstats
	strip $(BIN)/nfstats
	-chown $(NOTES) $(BIN)/nfstats
	-chgrp $(NOTESGRP) $(BIN)/nfstats
	chmod 2755 $(BIN)/nfstats

$(BIN)/nfprint:	nfprint
	cp nfprint $(BIN)/nfprint
	strip $(BIN)/nfprint
	-chown $(NOTES) $(BIN)/nfprint
	-chgrp $(NOTESGRP) $(BIN)/nfprint
	chmod 2755 $(BIN)/nfprint

$(BIN)/checknotes:	checknotes
	cp checknotes $(BIN)/checknotes
	strip $(BIN)/checknotes
	-chown $(NOTES) $(BIN)/checknotes
	-chgrp $(NOTESGRP) $(BIN)/checknotes
	chmod 2755 $(BIN)/checknotes

$(NET)/nfxmit:	nfxmit
	cp nfxmit $(NET)/nfxmit
	strip $(NET)/nfxmit
	-chown $(NOTES) $(NET)/nfxmit
	-chgrp $(NOTESGRP) $(NET)/nfxmit
	chmod 6750 $(NET)/nfxmit

$(NET)/nfrcv:	nfrcv
	cp nfrcv $(NET)/nfrcv
	strip $(NET)/nfrcv
	-chown $(NOTES) $(NET)/nfrcv
	-chgrp $(NOTESGRP) $(NET)/nfrcv
	chmod 6755 $(NET)/nfrcv

$(UTILITY)/newsinput:	newsinput
	cp newsinput $(UTILITY)/newsinput
	strip $(UTILITY)/newsinput
	-chown $(NOTES) $(UTILITY)/newsinput
	-chgrp $(NOTESGRP) $(UTILITY)/newsinput
	chmod 6755 $(UTILITY)/newsinput

$(UTILITY)/newsoutput:	newsoutput
	cp newsoutput $(UTILITY)/newsoutput
	strip $(UTILITY)/newsoutput
	-chown $(NOTES) $(UTILITY)/newsoutput
	-chgrp $(NOTESGRP) $(UTILITY)/newsoutput
	chmod 6755 $(UTILITY)/newsoutput

$(UTILITY)/mknf:	mknf
	cp mknf $(UTILITY)/mknf
	strip $(UTILITY)/mknf
	-chown $(NOTES) $(UTILITY)/mknf
	-chgrp $(NOTESGRP) $(UTILITY)/mknf
	chmod 4710 $(UTILITY)/mknf

$(UTILITY)/rmnf:	rmnf
	cp rmnf $(UTILITY)/rmnf
	strip $(UTILITY)/rmnf
	-chown $(NOTES) $(UTILITY)/rmnf
	-chgrp $(NOTESGRP) $(UTILITY)/rmnf
	chmod 4710 $(UTILITY)/rmnf

$(UTILITY)/nfarchive:	nfarchive
	cp nfarchive $(UTILITY)/nfarchive
	strip $(UTILITY)/nfarchive
	-chown $(NOTES) $(UTILITY)/nfarchive
	-chgrp $(NOTESGRP) $(UTILITY)/nfarchive
	chmod 6755 $(UTILITY)/nfarchive

$(UTILITY)/nfaccess:	nfaccess
	cp nfaccess $(UTILITY)/nfaccess
	strip $(UTILITY)/nfaccess
	-chown $(NOTES) $(UTILITY)/nfaccess
	-chgrp $(NOTESGRP) $(UTILITY)/nfaccess
	chmod 6755 $(UTILITY)/nfaccess

$(UTILITY)/nfmail:	nfmail
	cp nfmail $(UTILITY)/nfmail
	strip $(UTILITY)/nfmail
	-chown $(NOTES) $(UTILITY)/nfmail
	-chgrp $(NOTESGRP) $(UTILITY)/nfmail
	chmod 2755 $(UTILITY)/nfmail

#
#	Save old binaries. Put them all in the utility directory
#	so we don't clutter the bin directories
#

save:	saveuser savenet saveutil

saveuser:
	cp $(BIN)/notes $(UTILITY)/notes.old
	cp $(BIN)/nfpipe $(UTILITY)/nfpipe.old
	cp $(BIN)/nfprint $(UTILITY)/nfprint.old
	cp $(BIN)/nfstats $(UTILITY)/nfstats.old
	cp $(BIN)/checknotes $(UTILITY)/checknotes.old

savenet:
	cp $(NET)/nfxmit $(UTILITY)/nfxmit.old
	cp $(NET)/nfrcv $(UTILITY)/nfrcv.old
	cp $(UTILITY)/newsinput $(UTILITY)/newsinput.old
	cp $(UTILITY)/newsoutput $(UTILITY)/newsoutput.old

saveutil:
	cp $(UTILITY)/mknf $(UTILITY)/mknf.old
	cp $(UTILITY)/rmnf $(UTILITY)/rmnf.old
	cp $(UTILITY)/nfarchive $(UTILITY)/nfarchive.old
	cp $(UTILITY)/nfmail $(UTILITY)/nfmail.old

#
#	User programs
#

main:	main.o $(MAIN) libnfcom.a
	@echo loading main
	@${CC} $(CFLAGS) $(LFLAGS) -o main main.o $(MAIN) \
		libnfcom.a $(TERMCAP)

nfpipe:	nfpipe.o $(NFPIPE) libnfcom.a
	@echo loading nfpipe
	@${CC} $(CFLAGS) $(LFLAGS) -o nfpipe nfpipe.o $(NFPIPE) \
		libnfcom.a $(TERMCAP)

nfstats: nfstats.o $(NFSTATS) libnfcom.a
	@echo loading nfstats
	@${CC} $(CFLAGS) $(LFLAGS) -o nfstats nfstats.o $(NFSTATS) \
		libnfcom.a $(TERMCAP)

nfprint: nfprint.o $(NFPRINT) libnfcom.a
	@echo loading nfprint
	@${CC} $(CFLAGS) $(LFLAGS) -o nfprint nfprint.o $(NFPRINT) \
		libnfcom.a $(TERMCAP)

checknotes: checknotes.o $(CHECKNOTES) libnfcom.a
	@echo loading checknotes
	@${CC} $(CFLAGS) $(LFLAGS) -o checknotes checknotes.o $(CHECKNOTES) \
		libnfcom.a $(TERMCAP)

#
#	Network Transmission and reception
#

nfxmit:	nfxmit.o $(NFXMIT) libnfcom.a
	@echo loading nfxmit
	@${CC} $(CFLAGS) $(LFLAGS) -o nfxmit nfxmit.o $(NFXMIT) \
		libnfcom.a $(TERMCAP)

nfrcv:	nfrcv.o $(NFRCV) libnfcom.a
	@echo loading nfrcv
	@${CC} $(CFLAGS) $(LFLAGS) -o nfrcv nfrcv.o $(NFRCV) \
		libnfcom.a $(TERMCAP)

#
#	Utility programs - create/delete notesfiles, news gateways,
#	and archiving
#

mknf:	mknf.o $(MKNF) libnfcom.a
	@echo loading mknf
	@${CC} $(CFLAGS) $(LFLAGS) -o mknf mknf.o $(MKNF) \
		libnfcom.a $(TERMCAP)

rmnf:	rmnf.o $(RMNF) libnfcom.a
	@echo loading rmnf
	@${CC} $(CFLAGS) $(LFLAGS) -o rmnf rmnf.o $(RMNF) \
		libnfcom.a $(TERMCAP)

newsinput: newsinput.o $(NEWSINPUT) libnfcom.a
	@echo loading newsinput
	@${CC} $(CFLAGS) $(LFLAGS) -o newsinput newsinput.o $(NEWSINPUT) \
		libnfcom.a $(TERMCAP)

newsoutput: newsoutput.o $(NEWSOUTPUT) libnfcom.a
	@echo loading newsoutput
	@${CC} $(CFLAGS) $(LFLAGS) -o newsoutput newsoutput.o $(NEWSOUTPUT) \
		libnfcom.a $(TERMCAP)

nfarchive: nfarchive.o $(NFARCHIVE) libnfcom.a
	@echo loading nfarchive
	@${CC} $(CFLAGS) $(LFLAGS) -o nfarchive nfarchive.o $(NFARCHIVE) \
		libnfcom.a $(TERMCAP)

nfaccess: nfaccess.o $(NFACCESS) libnfcom.a
	@echo loading nfaccess
	@${CC} $(CFLAGS) $(LFLAGS) -o nfaccess nfaccess.o $(NFACCESS) \
		libnfcom.a $(TERMCAP)

nfmail:	nfmail.o $(NFMAIL) libnfcom.a
	@echo loading nfmail
	@${CC} ${CFLAGS} ${LFLAGS} -o nfmail nfmail.o ${NFMAIL} \
		libnfcom.a ${TERMCAP}

#
#	User subroutines
#

libnfcom.a:	$(LIBOBJS)
	ar r libnfcom.a $(LIBOBJS)
	-ranlib libnfcom.a

#
#	Load/Dump tools
#

nfdump:	nfdump.o $(NFDUMP) libnfcom.a
	@echo loading nfdump
	@$(CC) -o nfdump nfdump.o $(NFDUMP) libnfcom.a $(TERMCAP)

nfload: nfload.o $(NFLOAD) libnfcom.a
	@echo loading nfload
	@$(CC) -o nfload nfload.o $(NFLOAD) libnfcom.a $(TERMCAP)

#
#	This should set up some rules for dependencies on
#	the include files.
#

access.o   	: parms.h structs.h
adnote.o   	: parms.h structs.h
adresp.o   	: parms.h structs.h
archiver.o   	: parms.h structs.h
asearch.o   	: parms.h structs.h
bnewshead.o	: parms.h structs.h newsgate.h
buildnf.o	: parms.h structs.h
check.o   	: parms.h structs.h
checknotes.o   	: parms.h structs.h
compress.o   	: parms.h structs.h
control.o   	: parms.h structs.h
cursor.o   	: parms.h structs.h
datein.o   	: parms.h structs.h
delete.o   	: parms.h structs.h
dmpnote.o   	: parms.h structs.h
dmpresp.o   	: parms.h structs.h
dropt.o   	: parms.h structs.h
dsply.o   	: parms.h structs.h
expand.o	: parms.h structs.h
find.o   	: parms.h structs.h
gdate.o   	: parms.h structs.h
getnet.o   	: parms.h structs.h net.h
gname.o   	: parms.h structs.h
gtext.o   	: parms.h structs.h
gtime.o   	: parms.h structs.h
help.o   	: parms.h structs.h
index.o   	: parms.h structs.h
ldmisc.o   	: parms.h structs.h
limindx.o   	: parms.h structs.h
loadem.o   	: parms.h structs.h
lprnote.o   	: parms.h structs.h
lrsp.o   	: parms.h structs.h
mailit.o   	: parms.h structs.h
main.o   	: parms.h structs.h
misc.o   	: parms.h structs.h
miscio.o   	: parms.h structs.h
mknf.o   	: parms.h structs.h
newsdump.o   	: parms.h structs.h newsgate.h
newsgroup.o   	: parms.h structs.h newsgate.h
newsinput.o   	: parms.h structs.h newsgate.h
newsout.o   	: parms.h structs.h newsgate.h
newsoutput.o   	: parms.h structs.h newsgate.h
newspath.o	: newsgate.h
next.o   	: parms.h structs.h
nfabort.o	: parms.h structs.h
nfaccess.o	: parms.h structs.h
nfalias.o   	: parms.h structs.h net.h
nfarchive.o   	: parms.h structs.h
nfcomment.o   	:
nfmail.o	: parms.h structs.h
nfpipe.o   	: parms.h structs.h
nfprint.o   	: parms.h structs.h
nfrcv.o   	: parms.h structs.h net.h
nfsend.o   	: parms.h structs.h net.h
nfstats.o   	: parms.h structs.h
nfxmit.o   	: parms.h structs.h net.h
note.o   	: parms.h structs.h
pagein.o   	: parms.h structs.h
pagemove.o	: parms.h structs.h
pageout.o   	: parms.h structs.h
parsepath.o	: parms.h structs.h newsgate.h
pattern.o   	: parms.h structs.h
permit.o   	: parms.h structs.h
perms.o   	: parms.h structs.h
preptxt.o   	: parms.h structs.h
prtind.o   	: parms.h structs.h
readem.o   	: parms.h structs.h
recsio.o   	: parms.h structs.h
resp.o   	: parms.h structs.h
rmnf.o   	: parms.h structs.h
savtxt.o   	: parms.h structs.h
startup.o	: parms.h structs.h
talkto.o   	: parms.h structs.h
times.o   	: parms.h structs.h
tsearch.o   	: parms.h structs.h
#
nfdump.o	: parms.h structs.h
dump.o		: parms.h structs.h dump.h
nfload.o	: parms.h structs.h
loadnf.o	: parms.h structs.h dump.h
parse.o		: dump.h
loadvars.o	: dump.h
loadproto1.o	: parms.h structs.h dump.h

#
#	this is to make updating the "release" copy easy.
#

dist:	$(CFILES) $(HFILES) $(HLPFILES) $(MISCFILES) Revisions
	${DCP} $? $(DISTDIR)/src/.
	touch dist

Revisions: $(CFILES) $(HFILES) $(MISCFILES)
	grep "Header:" $(CFILES) $(HFILES) $(MISCFILES) > Revisions

#
#	Update the RCS library with this command.
#	Puts modified files into the RCS libraries with
#	the version number specified by "RCSID".
#	Also uses the RCS[IO]FLAGS variables.
#
#	since RCS has a bug in that the -l flag does no update the
#	version number in the copy left available, we check in
#	the files and then check them out again.
#

freeze:	$(CFILES) $(HFILES) $(MISCFILES) $(HLPFILES)
	ci $(RCSIFLAGS) $?
	co $(RCSOFLAGS) $?
	touch freeze

#	Make printouts of the modified routines
#

print:	$(MISCFILES) $(HFILES) $(CFILES) $(HLPFILES)
#	pr -f $? | lpr -b "Notesfile Code"
	imprint -2 -I -P2 $?
	touch print

#	For those using vi, produce a ctags file of the source code.
#

tags:	$(CFILES)
	ctags $(CFILES)

#
#	Lint the mess to catch inconsistencies.
#

lint:
	lint -haxc $(CFLAGS) *.c
