#
#  Makefile for INGRES header files
#
#	@(#)Makefile	7.1	2/5/81
#

SRCS=	access.h aux.h batch.h catalog.h func.h \
	ingres.h lock.h opsys.h pv.h range.h resp.h \
	symbol.h trace.h tree.h useful.h version.h \
	pmon.h depend.sed

GET=	sccs get
REL=
PR=	print
VGRIND=	csh /usr/ucb/vgrind
HEADER=	Header Files

sources: $(SRCS)

$(SRCS):
	$(GET) $(REL) SCCS/s.$@

install: sources

new:

print: sources
	$(PR) Makefile *.[hm] *.[csy]

vgrind: sources
	$(VGRIND) -h "$(HEADER) (Release 7)" -n Makefile
	$(VGRIND) -h "$(HEADER) (Release 7)" *.[hm]

clean:
	rm -f *.o
	rm -f core a.out
