#	Makefile for some of the utilities that come with the 
#	notesfile package
#
#	This isn't as general as it could/should be, I just copied
#	these definitions from the Makefile in the "src" directory.
#	Since these programs aren't really part of the distribution
#	but rather a set of retrofit/upgrade utilities, I don't
#	feel too bad about it.
#
BIN = /usr/local
MSTDIR = /usr/spool/notes
ARCHDIR = /usr/spool/oldnotes
NET = /usr/bin
NOTES = notes
NOTESUID = 10
ANON = anon
ANONUID = 4
CC = cc
CFLAGS = -DMSTDIR=\"$(MSTDIR)\" -DARCHDIR=\"$(ARCHDIR)\" \
	-DNOTES=\"$(NOTES)\" -DNOTESUID=$(NOTESUID) \
	-DANON=\"$(ANON)\" -DANONUID=$(ANONUID) \
	-DBIN=\"$(BIN)\" -DRCSIDENT
#
#

SRC	= fieldzero.c nfrank.c structsize.c \
	  Makefile
SUBDIR=	seq-cvt
DCP=	/usr/ucb/rcp
DISTDIR= p:/usr/src/uiuc/notesfiles
ALL	= fieldzero nfrank structsize
FIELDZERO = fieldzero.o ../src/recsio.o ../src/misc.o ../src/miscio.o \
	  ../src/perms.o ../src/check.o ../src/gtime.o ../src/gname.o \
	  ../src/cursor.o ../src/startup.o ../src/nfcomment.o \
	  ../src/nfabort.o
#
#	So I can do quick fixes....
#	FAMOUS LAST WORDS:  DO NOT RUN RBEFIX UNLESS YOU ARE RBE.
#			    IT WILL DESTROY YOUR DATA
#
RBEFIX	 = rbefix.o ../src/recsio.o ../src/misc.o ../src/miscio.o \
	  ../src/perms.o ../src/check.o ../src/gtime.o ../src/gname.o \
	  ../src/cursor.o ../src/startup.o ../src/nfcomment.o \
	  ../src/nfabort.o

all:
	@echo "The utility directory isn't normally compiled"

structsize: structsize.o
	$(CC) structsize.o -o structsize

nfrank:	nfrank.c
	$(CC) nfrank.c -o nfrank

fieldzero:	$(FIELDZERO)
	$(CC) -o fieldzero $(FIELDZERO) -ltermcap

#
#	Administration
#

rbefix:	$(RBEFIX)
	$(CC) -o rbefix $(RBEFIX) -ltermcap

RCSIFLAGS = -r1.6.2
RCSOFLAGS = -r1.6.2

freeze:	$(SRC)
	ci $(RCSIFLAGS) $?
	co $(RCSOFLAGS) $?
	touch freeze

dist:	ldist subdist
	touch dist

subdist: $(SUBDIR)
	for i in $(SUBDIR); do \
		(cd $$i; make ${MFLAGS} DCP=${DCP} DISTDIR=${DISTDIR} dist); done

ldist:	$(SRC)
	${DCP} $? ${DISTDIR}/utility/.
	touch ldist

clean:
	for i in ${SUBDIR}; do (cd $$i; make clean); done;
	rm -f *.o $(ALL) Make.errs rbefix

${SUBDIR}:	FRC

FRC:
