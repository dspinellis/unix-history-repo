#
# Copyright, 1986, Massachusetts Institute of Technology.
#	xcons - trivial program to snarf console output and redirect it.
#
#

DESTDIR =
INCLUDES = -I../include

CONFDIR = /usr/new
CFLAGS = -O $(INCLUDES)
PROG = xcons

OBJS = $(PROG).o

all: $(PROG)

install: all
	install -c $(PROG) $(DESTDIR)$(CONFDIR)
	chmod 755 $(DESTDIR)$(CONFDIR)/$(PROG)

clean: 
	rm -f $(PROG).o $(PROG) *~ \#*

igrind:
	igrind $(PROG).c

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o $(PROG) $(OBJS)

