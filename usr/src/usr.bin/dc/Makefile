#
#	@(#)Makefile	1.1	(Berkeley)	83/02/11
#
SRCS = dc.c dc.h
all: dc

dc:	dc.c dc.h
	cc -O dc.c -o dc

install: dc
	install -s dc $(DESTDIR)/usr/bin
clean:
	rm -f *.o

sources: srcs
srcs: $(SRCS)
	sccs get $@
