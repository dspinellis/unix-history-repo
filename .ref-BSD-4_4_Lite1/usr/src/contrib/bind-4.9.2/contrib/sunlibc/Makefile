# Simple, but effective way of recompling the 

MAKE=make
SHELL=/bin/sh

# These are important to be the way they are.  Make sure you know what
# you are doing if you change them.
LDS=:
CC=cc -DBSD -pic

BUILDDIR=../../native.b

NETOBJ=getnetbyaddr.o getnetbyname.o getnetent.o getnetnamadr.o
RESOBJ=gethnamaddr.o herror.o inet_addr.o res_comp.o res_debug.o \
res_init.o res_mkquery.o res_query.o res_send.o sethostent.o $(NETOBJ)
COMPATOBJ=strerror.o

MFLAGS= "LDS=${LDS}" "SUBDIRS=res compat" "CC=$(CC)"

all: force objects copy

force: FRC
	(cd $(BUILDDIR); $(MAKE) $(MFLAGS) clean)

objects: FRC
	(cd $(BUILDDIR); pwd; $(MAKE) $(MFLAGS) all)

copy: FRC
	for f in $(RESOBJ); do \
		cp $(BUILDDIR)/res/$$f .; \
	done
	for f in $(COMPATOBJ); do \
		cp $(BUILDDIR)/compat/lib/$$f .; \
	done

clean: FRC
	$(RM) *.o

FRC:
