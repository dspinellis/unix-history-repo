# ----------------------------------------------------------------------
# Adapt the installation directories to your local standards.
# ----------------------------------------------------------------------

BINDIR = /usr/local/bin
MANDIR = /usr/local/man/man1
 
# ----------------------------------------------------------------------
# Special compilation options are needed only on a few platforms.
# ----------------------------------------------------------------------

#if defined(_AIX)
#	SYSDEFS = -D_BSD -D_BSD_INCLUDES -U__STR__ -DBIT_ZERO_ON_LEFT
#endif
 
#if defined(hpux)
#	SYSDEFS = -DSYSV_SETVBUF
#endif
 
#if defined(ultrix) && You are using the default ultrix <resolv.h>
#	SYSDEFS = -DULTRIX_RESOLV
#endif

SYSDEFS =

# ----------------------------------------------------------------------
# Compilation definitions.
# ----------------------------------------------------------------------

DEFS =

CFLAGS = -O $(DEFS) $(SYSDEFS)

CC = /bin/cc

# ----------------------------------------------------------------------
# Files.
# ----------------------------------------------------------------------

HDRS = type.h
SRCS = host.c send.c
OBJS = host.o send.o
PROG = host
MANS = host.1

FILES = Makefile $(HDRS) $(SRCS) $(MANS)

# ----------------------------------------------------------------------
# libresolv.a should contain the resolver library of BIND 4.8.2 or later.
# Link it in only if your default library is different.
# libnet.a contains the getnet...() getserv...() getproto...() calls.
# It is safe to leave it out and use your default library.
# ----------------------------------------------------------------------

LIBS = ../resolver/libresolv.a
LIBS = -lresolv
LIBS = -lresolv -lnet

# ----------------------------------------------------------------------
# Rules for installation.
# ----------------------------------------------------------------------

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) -o $(PROG) $(OBJS) $(LIBS)

install: $(PROG)
	install -c -m 755 -s $(PROG) $(BINDIR)

clean:
	rm -f $(PROG) $(OBJS) *.o a.out core host.tar host.tar.Z

man:
	install -c -m 444 host.1 $(MANDIR)

# ----------------------------------------------------------------------
# host may be called with alternative names, querytype names and "zone".
# ----------------------------------------------------------------------

ABBREVIATIONS = mx ns soa zone

links:
	for i in $(ABBREVIATIONS) ; do \
		(cd $(BINDIR) ; ln -s $(PROG) $$i) ; \
	done

# ----------------------------------------------------------------------
# Rules for maintenance.
# ----------------------------------------------------------------------

lint:
	lint $(SRCS)

llint:
	lint $(SRCS) -lresolv

dist:
	tar cf host.tar $(FILES)
	compress host.tar

newversion:
	cp -p host.c.dist host.c.old
	cp -p send.c.dist send.c.old
	cp -p host.c      host.c.dist
	cp -p send.c      send.c.dist
	@(echo "enter version" ; read x ; echo $$x >version)
	touch patch.`cat version`
	-diff -c host.c.old host.c >>patch.`cat version`
	-diff -c send.c.old send.c >>patch.`cat version`
