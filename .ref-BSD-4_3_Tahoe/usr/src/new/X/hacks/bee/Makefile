#
#  7/5/85 hack  @(#)Makefile	1.2
#
#	main.c	initialization
#	head.c	mail header parsing
#

DESTDIR=/usr/new
HDRS =
INCLUDES = -I../../include
LIBS= ../../Xlib/libX.a
SRCS = main.c
OBJS = main.o

CFLAGS =  $(DFLAGS) $(INCLUDES)

hack:	$(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o hack $(LIBS)

$(OBJS): $(HDRS)

install: hack
	install -c -s error $(DESTROOT)$(DESTDIR)

clean:
	rm -f hack $(OBJS)

sccsinfo:
	sccs info

sccsget:
	sccs get SCCS

print:
	print $(HDRS) $(SRCS)
