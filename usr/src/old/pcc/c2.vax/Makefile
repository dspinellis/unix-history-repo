#	@(#)Makefile	4.5	(Berkeley)	87/05/28
#
CFLAGS=-DCOPYCODE -DC2 -O
LDFLAGS=
ASDIR = /usr/src/bin/as/as.${MACHINE}

SRCS = makefile c2.h c20.c c21.c c22.c

c2:	c20.o c21.o c22.o
	$(CC) $(LDFLAGS) -o c2 c2?.o

c20.o:	c20.c c2.h
c21.o:	c21.c c2.h
c22.o:  c22.c c2.h instrs.c2
	$(CC) $(CFLAGS) -R -c c22.c

instrs.c2: $(ASDIR)/instrs
	(echo FLAVOR C2 ; cat $(ASDIR)/instrs) | awk -f $(ASDIR)/instrs > instrs.c2

clean:
	rm -f *.o *.s 

install: c2
	install -s c2 $(DESTDIR)/lib

sources: $(SRCS)

$(ASDIR)/instrs:
	(cd $(ASDIR); sccs get instrs)

$(SRCS):
	sccs get $@
