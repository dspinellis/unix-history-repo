#	makefile for the franz lisp manual
#
# sources: lmacs: macros for the franz documents
#	   ch1.n  intro and description of types
#	   ch2.n  data structure access
#	   ch3.n  on arithmetic functions.
#	   ch4.n  special functions.
#	   ...

TROFF=	csh /usr/ucb/vtroff

.SUFFIXES: .n .t .x

INDX= ch2.x ch3.x ch4.x ch5.x ch6.x ch7.x

all: ch0.o ch1.o ch2.o ch3.o ch4.o ch5.o ch6.o ch7.o findex

tall: ch0.t ch1.t ch2.t ch3.t ch4.t ch5.t ch6.t ch7.t tindex



.n.o:
	tbl lmacs $*.n | ${TROFF} -me 2> $*.x

.n.t:
	tbl lmacs $*.n | csh /usr/ucb/nettroff -me 2> $*.x

.n.x:
	tbl lmacs $*.n | ${TROFF} -me -z 2> $*.x


ch1.o: ch1.n
	tbl lmacs $*.n | ${TROFF} -me

findex: ${INDX}
	echo ".Ib" > index
	sort +3 -o index index ${INDX}
	${TROFF} -me lmacs index

tindex:
	sort +3 index -o index
	csh /usr/ucb/nettroff -me lmacs index

clean:
	rm index *.x
