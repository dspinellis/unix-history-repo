#

DESTDIR =
INCLUDES = -I../include

CONFDIR = /usr/new
XLIB = ../Xlib/libX.a
CFLAGS = -O $(INCLUDES)
PROG = xshell

OBJS = xshell.o xutils.o

all: $(PROG)

install: all
	install -c $(PROG) $(DESTDIR)$(CONFDIR)
	chmod 755 $(DESTDIR)$(CONFDIR)/$(PROG)

clean: 
	rm -f $(OBJS) $(PROG) *~ \#*

igrind:
	igrind $(PROG).c

$(PROG): $(OBJS) ../Xlib/Xlib.h
	$(CC) $(CFLAGS) -o $(PROG) $(OBJS) $(XLIB)

xshell.o:	scallopshell.h	
