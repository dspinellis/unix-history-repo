CFLAGS = -O
USEROBJS = lookup.o PasswordLookup_client.o
SRVROBJS = PasswordLookup.o PasswordLookup_server.o
LIBS = -lcr
DESTDIR = /usr/new/lib/courier

all:	lookup PasswordLookup

lookup:	$(USEROBJS)
	cc -o lookup $(USEROBJS) $(LIBS)

PasswordLookup:	$(SRVROBJS)
	cc -o PasswordLookup $(SRVROBJS) $(LIBS)

$(USEROBJS) $(SRVROBJS):	PasswordLookup.h

PasswordLookup.h PasswordLookup_server.c PasswordLookup_client.c: PasswordLookup.cr
	courier PasswordLookup.cr

install:	all
	install -s PasswordLookup $(DESTDIR)

clean:
	-rm -f *.o PasswordLookup_*.c PasswordLookup.h lookup PasswordLookup
