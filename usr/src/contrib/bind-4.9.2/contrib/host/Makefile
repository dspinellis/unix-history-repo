#	@(#)Makefile            e07@nikhef.nl (Eric Wassenaar) 930919

# ----------------------------------------------------------------------
# Adapt the installation directories to your local standards.
# ----------------------------------------------------------------------

# This is where the host executable will go.
DESTBIN = /usr/local/bin

# This is where the host manual page will go.
DESTMAN = /usr/local/man

BINDIR = $(DESTBIN)
MANDIR = $(DESTMAN)/man1

# ----------------------------------------------------------------------
# Special compilation options may be needed only on a few platforms.
# See also the header file port.h for portability issues.
# ----------------------------------------------------------------------

#if defined(_AIX)
SYSDEFS = -D_BSD -D_BSD_INCLUDES -U__STR__ -DBIT_ZERO_ON_LEFT
#endif
 
#if defined(ultrix) && You are using the default ultrix <resolv.h>
SYSDEFS = -DULTRIX_RESOLV
#endif
 
#if defined(solaris) && You do not want to use BSD compatibility mode
SYSDEFS = -DSYSV
#endif
 
#if defined(solaris) && You are using its default broken resolver library
SYSDEFS = -DNO_YP_LOOKUP
#endif

SYSDEFS =

# ----------------------------------------------------------------------
# Configuration definitions.
# ----------------------------------------------------------------------

#if defined(BIND_48) && You want to use the default bind res_send()
CONFIGDEFS = -DBIND_RES_SEND
#endif

#if defined(BIND_49) && You want to use the special host res_send()
CONFIGDEFS = -DHOST_RES_SEND
#endif

# This is the default in either case if you compile stand-alone.
CONFIGDEFS = -DHOST_RES_SEND

# ----------------------------------------------------------------------
# Compilation definitions.
# ----------------------------------------------------------------------

DEFS = $(CONFIGDEFS) $(SYSDEFS)

COPTS =
COPTS = -O

CFLAGS = $(COPTS) $(DEFS)

# Select your favorite compiler.
CC = cc
CC = /usr/ucb/cc			#if defined(solaris) && BSD
CC = /bin/cc

# ----------------------------------------------------------------------
# Linking definitions.
# libresolv.a should contain the resolver library of BIND 4.8.2 or later.
# Link it in only if your default library is different.
# libnet.a contains the getnet...() getserv...() getproto...() calls.
# It is safe to leave it out and use your default library.
# ----------------------------------------------------------------------

RES = ../res/libresolv.a
RES = -lresolv

COMPLIB =
COMPLIB = -lnet

LIBS = -lsocket -lnsl			#if defined(solaris) && not BSD
LIBS =

LIBRARIES = $(RES) $(COMPLIB) $(LIBS)

LDFLAGS =

# ----------------------------------------------------------------------
# Compatibility for compilation via the BIND master Makefile.
# ----------------------------------------------------------------------

# redefined by bind
INCL = .
COMPINCL = .
INCS = -I$(INCL) -I$(COMPINCL)

# redefined by bind
CDEBUG = $(COPTS) $(CONFIGDEFS)
CDEFS = $(SYSDEFS) $(INCS)
CFLAGS = $(CDEBUG) $(CDEFS)

# ----------------------------------------------------------------------
# Miscellaneous definitions.
# ----------------------------------------------------------------------

MAKE = make $(MFLAGS)

# This assumes the BSD install.
INSTALL = install -c

# Grrr
SHELL = /bin/sh

# ----------------------------------------------------------------------
# Files.
# ----------------------------------------------------------------------

HDRS = type.h exit.h port.h
SRCS = host.c send.c vers.c
OBJS = host.o send.o vers.o
PROG = host
MANS = host.1
DOCS = RELEASE_NOTES

FILES = Makefile $(DOCS) $(HDRS) $(SRCS) $(MANS)

# ----------------------------------------------------------------------
# Rules for installation.
# ----------------------------------------------------------------------

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) $(LDFLAGS) -o $(PROG) $(OBJS) $(LIBRARIES)

install: $(PROG)
	$(INSTALL) -m 755 -s $(PROG) $(BINDIR)

man: $(MANS)
	$(INSTALL) -m 444 host.1 $(MANDIR)

clean:
	rm -f $(PROG) $(OBJS) *.o a.out core host.tar host.tar.Z

# ----------------------------------------------------------------------
# host may be called with alternative names, querytype names and "zone".
# A few frequently used abbreviations are handy.
# ----------------------------------------------------------------------

ABBREVIATIONS = a ns cname soa wks ptr hinfo mx txt	# standard
ABBREVIATIONS = mb mg mr minfo				# deprecated
ABBREVIATIONS = md mf null				# obsolete
ABBREVIATIONS = rp afsdb x25 isdn rt nsap nsap-ptr	# new
ABBREVIATIONS = uinfo uid gid unspec			# nonstandard
ABBREVIATIONS = maila mailb any				# filters

ABBREVIATIONS = mx ns soa zone

links:
	for i in $(ABBREVIATIONS) ; do \
		(cd $(BINDIR) ; ln -s $(PROG) $$i) ; \
	done

# ----------------------------------------------------------------------
# Rules for maintenance.
# ----------------------------------------------------------------------

lint:
	lint $(DEFS) $(SRCS)

llint:
	lint $(DEFS) $(SRCS) -lresolv

print:
	lpr -J $(PROG) -p Makefile $(DOCS) $(HDRS) $(SRCS)

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

depend:
	mkdep $(DEFS) $(SRCS)

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.
