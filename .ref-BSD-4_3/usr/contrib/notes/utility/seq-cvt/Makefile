#
#	Source files for sequencer conversion routines
#

CFILES = seqtobinary.c seqtoascii.c
SHFILES = convseq.sh restseq.sh
OTHERS=	Makefile

DCP=	/usr/ucb/rcp
DISTDIR= p:/usr/src/uiuc/notesfiles


all:	seqtobinary seqtoascii convseq.sh restseq.sh

seqtobinary:	seqtobinary.c ../../src/parms.h ../../src/structs.h
	cc -O -o seqtobinary seqtobinary.c

seqtoascii:	seqtoascii.c ../../src/parms.h ../../src/structs.h
	cc -O -o seqtoascii seqtoascii.c

clean:
	rm -f seqtobinary seqtoascii

install:
	install -m 4755 -o notes seqtoascii /usr/local
	install -m 4755 -o notes seqtobinary /usr/local
	install -c convseq.sh /usr/local/convseq
	install -c restseq.sh /usr/local/restseq

dist:	${CFILES} ${SHFILES} ${OTHERS}
	${DCP} $? ${DISTDIR}/utility/seq-cvt/.
	touch dist
