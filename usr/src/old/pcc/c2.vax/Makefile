#	@(#)Makefile	4.1	(Berkeley)	82/10/27
#
CFLAGS=-c -d2 -w -DCOPYCODE -DC2
LDFLAGS=
ASDIR = /usr/src/bin/as

SRCS = makefile c2.h c20.c c21.c c22.c

c2:	c20.o c21.o c22.o
	$(CC) $(LDFLAGS) -z -o c2 c2?.o

c20.o:	c20.c c2.h
c21.o:	c21.c c2.h
c22.o:  c22.c c2.h instrs.c2
	$(CC) $(CFLAGS) -R c22.c

instrs.c2: $(ASDIR)/instrs
	(echo FLAVOR C2 ; cat $(ASDIR)/instrs) | awk -f /usr/src/bin/as/instrs > instrs.c2

clean:
	rm -f *.o *.s 

install: c2
	install -s c2 $(DESTDIR)/lib

sources: $(SRCS)

$(ASDIR)/instrs:
	(cd $(ASDIR); sccs get instrs)

$(SRCS):
	sccs get $@
