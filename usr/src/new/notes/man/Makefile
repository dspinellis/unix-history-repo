#	Makefile for the notesfile manual pages
#	March 29, 1982
#	Ray Essick, University of Illinois
#	uiucdcs!essick      uiucdcs!notes
#
#	If the "man" pages for your system have been moved, check
#	the MAN variable below.
#

DISTDIR= p:/usr/src/uiuc/notesfiles
DCP=	rcp

SEC1	=  notes.1 nfpipe.1 checknotes.1 nfstats.1 nfprint.1 \
	   autoseq.1 readnotes.1
SEC3	=  nfabort.3 nfcomment.3
SEC8	=  mknf.8 rmnf.8 nfxmit.8 nfrcv.8 newsinput.8 newsoutput.8 \
	   nfaccess.8 nfarchive.8 nfmail.8 nfdump.8 nfload.8
FILES	= $(SEC1) $(SEC3) $(SEC8)
OTHERS	= Makefile getdate.3 rscript
MAN	= /usr/man

all:

clean:

install: isec1 isec3 isec8
	touch install

isec1:	$(SEC1)
	cp $? $(MAN)/man1/.
	touch isec1

isec3:	$(SEC3)
	cp $? $(MAN)/man3/.
	touch isec3

isec8:	$(SEC8)
	cp $? $(MAN)/man8/.
	touch isec8

ar:	$(FILES)
	ar r ../Page.a $(FILES)
	touch ar

dist:	$(FILES) $(OTHERS)
	${DCP} $? $(DISTDIR)/man/.
	touch dist

#	remote man page installation - U of Illinois DCS specific
#
RSYS	= a b d e f g h i p
rinstall: $(SEC1) $(SEC3) $(SEC8)
	rscript $(RSYS)
	touch rinstall
