# Generated automatically from Makefile.in by configure.
# This is the Makefile for Taylor UUCP
#
# The file Makefile.in should be processed by configure to generate
# Makefile.  If you want to generate Makefile by hand, you must find
# all variables surrounded by @ and replace them with the correct
# value (e.g. gcc -O must be replaced by something like cc or gcc).
#
# Once you have done that, you should check the definitions at the top
# of this file to make sure that they are reasonable for your system.

# Prefix directory for installation directories.
prefix = /usr

# The user name that should own the resulting executables, several of
# which are suid.
owner = uucp

# Where to install uucico and uuxqt, and where to read configuration files
# from by default.  If you use this definition, $(prefix)/lib must exist.
libdir = $(prefix)/libexec/uucp

# Where to install uucp, uux, uuchk, uustat, uuname and uulog.
bindir = $(prefix)/bin

# Where to install man pages.  Section 1 for user programs, 8 for daemons.
man1dir = $(prefix)/share/man/cat1
man1ext = .0
man8dir = $(prefix)/share/man/cat8
man8ext = .0

# Where to install the info files.
infodir = $(prefix)/info

# The directory to look in for new style configuration files (when
# using HAVE_TAYLOR_CONFIG).  Note that by default this is different
# from libdir, unlike traditional UUCP packages.
newconfigdir = /etc/uucp

# The directory to look in for BNU (when using HAVE_BNU_CONFIG) or
# V2 (when using HAVE_V2_CONFIG) style configuration files.
oldconfigdir = /etc/uucp

# If you don't want to compile tstuu, which requires BSD style ptys
# and the Taylor configuration files, comment out the following line
# The tstuu program is only used to test the package; it is described
# further in the documentation.
TSTUU = tstuu

#
# The next few lines are set up by the configuration script.  You may
# want to look them over.
#

# If you do not have the mkdir system call, undefine the following three
# lines.  This will install uudir as an suid root program.  This is
# necessary because invoking /bin/mkdir from an suid program will
# leave the directories owned by the wrong user.
# UUDIR = uudir
# uudirdir = $(libdir)/util
# UUDIRFLAGS = -DUUDIR_PROGRAM=\"$(uudirdir)/$(UUDIR)\"

# Source directory and, if necessary, VPATH
srcdir = .


# Define programs and flags
CC = gcc -O
CFLAGS = -g
LDFLAGS = 
LIBS = 
ALLOCA = 

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = /usr/bin/install -c -m 644

LN_S = ln -s

#
# Nothing else to configure
#

SHELL = /bin/sh

VERSION = 1.03

MORECFLAGS = -I$(srcdir) -I. -DLIBDIR=\"$(libdir)\" -DNEWCONFIGLIB=\"$(newconfigdir)\" -DOLDCONFIGLIB=\"$(oldconfigdir)\" -DOWNER=\"$(owner)\" $(UUDIRFLAGS)

PROGRAMS = uucico uuxqt uux uucp uuchk uustat uuname uulog

UUOBJS = uucico.o prot.o protg.o protf.o prott.o prote.o config.o v2.o bnu.o \
	file.o util.o sysinf.o prtinf.o time.o log.o chat.o port.o tcp.o \
	parse.o copy.o sys1.o sys2.o sys3.o sys4.o version.o getopt.o \
	$(ALLOCA)
XQTOBJS = uuxqt.o config.o v2.o bnu.o util.o sysinf.o prtinf.o log.o \
	copy.o sys1.o sys3.o sys5.o version.o getopt.o $(ALLOCA)
UUXOBJS = uux.o config.o v2.o bnu.o util.o sysinf.o prtinf.o log.o \
	copy.o sys1.o sys3.o sys6.o version.o getopt.o $(ALLOCA)
UUCPOBJS = uucp.o config.o v2.o bnu.o util.o sysinf.o prtinf.o log.o \
	copy.o sys1.o sys3.o sys6.o version.o getopt.o $(ALLOCA)
UUCHKOBJS = uuchk.o config.o v2.o bnu.o util.o sysinf.o prtinf.o time.o \
	log.o sys1.o version.o getopt.o $(ALLOCA)
UUSTATOBJS = uustat.o config.o v2.o bnu.o util.o sysinf.o prtinf.o \
	log.o parse.o copy.o sys1.o sys3.o sys4.o sys5.o sys7.o version.o \
	getopt.o $(ALLOCA)
UUNAMEOBJS = uuname.o config.o v2.o bnu.o util.o sysinf.o prtinf.o \
	log.o sys1.o version.o getopt.o $(ALLOCA)
UULOGOBJS = uulog.o config.o v2.o bnu.o util.o sysinf.o prtinf.o \
	log.o sys1.o version.o getopt.o $(ALLOCA)
TSTOBJS = tstuu.o version.o getopt.o $(ALLOCA)

ALLOBJS = uucico.o uuxqt.o uux.o uucp.o uuchk.o uustat.o uuname.o uulog.o \
	uudir.o tstuu.o prot.o protg.o protf.o prott.o prote.o config.o v2.o \
	bnu.o file.o util.o sysinf.o prtinf.o time.o log.o chat.o port.o \
	tcp.o parse.o copy.o sys1.o sys2.o sys3.o sys4.o sys5.o sys6.o \
	sys7.o version.o getopt.o

all: $(PROGRAMS) $(TSTUU) $(UUDIR)

install: $(PROGRAMS) uucp.info $(UUDIR)
	-mkdir $(libdir) $(bindir)
	-ln $(libdir)/uucico $(libdir)/uucico.old
	-ln $(libdir)/uuxqt $(libdir)/uuxqt.old
	rm -f $(libdir)/uucico $(libdir)/uuxqt
	$(INSTALL_PROGRAM) uucico $(libdir)/uucico
	$(INSTALL_PROGRAM) uuxqt $(libdir)/uuxqt
	$(INSTALL_PROGRAM) uuchk $(libdir)/uuchk
	chown $(owner) $(libdir)/uucico $(libdir)/uuxqt $(libdir)/uuchk
	chmod 4555 $(libdir)/uucico $(libdir)/uuxqt
	-if test "$(UUDIR)X" != "X"; then if test ! -d $(uudirdir); then mkdir $(uudirdir); fi; chown $(owner) $(uudirdir); chmod 100 $(uudirdir); $(INSTALL_PROGRAM) uudir $(uudirdir)/uudir; chown root $(uudirdir)/uudir; chmod 4555 $(uudirdir)/uudir; else true; fi
	-ln $(bindir)/uux $(bindir)/uux.old
	-ln $(bindir)/uucp $(bindir)/uucp.old
	-ln $(bindir)/uustat $(bindir)/uustat.old
	-ln $(bindir)/uuname $(bindir)/uuname.old
	-ln $(bindir)/uulog $(bindir)/uulog.old
	rm -f $(bindir)/uux $(bindir)/uucp $(bindir)/uustat 
	rm -f $(bindir)/uuname $(bindir)/uulog
	$(INSTALL_PROGRAM) uux $(bindir)/uux
	$(INSTALL_PROGRAM) uucp $(bindir)/uucp
	$(INSTALL_PROGRAM) uustat $(bindir)/uustat
	$(INSTALL_PROGRAM) uuname $(bindir)/uuname
	$(INSTALL_PROGRAM) uulog $(bindir)/uulog
	chown $(owner) $(bindir)/uux $(bindir)/uucp $(bindir)/uustat 
	chown $(owner) $(bindir)/uuname
	chmod 4555 $(bindir)/uux $(bindir)/uucp $(bindir)/uustat 
	chmod 4555 $(bindir)/uuname

uninstall:
	rm -f $(libdir)/uucico $(libdir)/uuxqt
	-cp $(libdir)/uucico.old $(libdir)/uucico
	-cp $(libdir)/uuxqt.old $(libdir)/uuxqt
	-chown $(owner) $(libdir)/uucico $(libdir)/uuxqt $(libdir)/uuchk
	-chmod 4555 $(libdir)/uucico $(libdir)/uuxqt
	rm -f $(bindir)/uux $(bindir)/uucp $(bindir)/uustat
	rm -f $(bindir)/uuname $(bindir)/uulog
	-cp $(bindir)/uux.old $(bindir)/uux
	-cp $(bindir)/uucp.old $(bindir)/uucp
	-cp $(bindir)/uustat.old $(bindir)/uustat
	-cp $(bindir)/uuname.old $(bindir)/uuname
	-cp $(bindir)/uulog.old $(bindir)/uulog
	-chown $(owner) $(bindir)/uux $(bindir)/uucp $(bindir)/uustat 
	-chown $(owner) $(bindir)/uuname
	-chmod 4555 $(bindir)/uux $(bindir)/uucp $(bindir)/uustat 
	-chmod 4555 $(bindir)/uuname

uucico: $(UUOBJS)
	$(CC) $(LDFLAGS) -o uucico $(UUOBJS) $(LIBS)

uuxqt: $(XQTOBJS)
	$(CC) $(LDFLAGS) -o uuxqt $(XQTOBJS) $(LIBS)

uux: $(UUXOBJS)
	$(CC) $(LDFLAGS) -o uux $(UUXOBJS) $(LIBS)

uucp: $(UUCPOBJS)
	$(CC) $(LDFLAGS) -o uucp $(UUCPOBJS) $(LIBS)

uuchk: $(UUCHKOBJS)
	$(CC) $(LDFLAGS) -o uuchk $(UUCHKOBJS) $(LIBS)

uustat: $(UUSTATOBJS)
	$(CC) $(LDFLAGS) -o uustat $(UUSTATOBJS) $(LIBS)

uuname: $(UUNAMEOBJS)
	$(CC) $(LDFLAGS) -o uuname $(UUNAMEOBJS) $(LIBS)

uulog: $(UULOGOBJS)
	$(CC) $(LDFLAGS) -o uulog $(UULOGOBJS) $(LIBS)

tstuu: $(TSTOBJS)
	$(CC) $(LDFLAGS) -o tstuu $(TSTOBJS) $(LIBS)

uudir: uudir.o
	$(CC) $(LDFLAGS) -o uudir uudir.o $(LIBS)

.c.o:
	$(CC) -c $(CFLAGS) $(MORECFLAGS) $<

.SUFFIXES: .unx

sysdep.h: sysh.unx
	rm -f sysdep.h
	$(LN_S) sysh.unx sysdep.h

.unx.c:
	rm -f $@
	$(LN_S) $< $@

clean:
	rm -f $(ALLOBJS) uucico uuxqt uux uucp uuchk tstuu
	rm -f uucp-$(VERSION).tar.Z
	rm -rf uucp-$(VERSION)

# The distribution targets are mostly for my own use; they are not
# expected to work on other systems.

dist: uucp.info conf.h-dist
	-rm -rf uucp-$(VERSION)
	-rm -f taylor-uucp-$(VERSION).tar taylor-uucp-$(VERSION).tar.Z
	mkdir uucp-$(VERSION)
	ln `cat MANIFEST` uucp-$(VERSION)
	rm -f uucp-$(VERSION)/policy.h uucp-$(VERSION)/Makefile.in
	cp policy.h Makefile.in uucp-$(VERSION)
	chmod 0644 uucp-$(VERSION)/policy.h uucp-$(VERSION)/Makefile.in
	mkdir uucp-$(VERSION)/contrib
	ln contrib/* uucp-$(VERSION)/contrib
	tar -cvf taylor-uucp-$(VERSION).tar uucp-$(VERSION)
	compress taylor-uucp-$(VERSION).tar

uucp.info: uucp.texi
	makeinfo uucp.texi

conf.h-dist: conf.h
	cp conf.h conf.h-dist

conf.h Makefile: configure
	configure

Makefile: Makefile.in

configure: configure.in
	autoconf

# GNU standards want these targets

mostlyclean: clean

realclean: clean
	rm conf.h

TAGS:
	etags *.h *.c

# Header file dependencies.  These are maintained by hand.

uucico.o: uucp.h conf.h policy.h system.h prot.h port.h sysdep.h uutime.h
uucico.o: getopt.h
uuxqt.o: uucp.h conf.h policy.h system.h getopt.h sysdep.h
uux.o: uucp.h conf.h policy.h system.h getopt.h sysdep.h
uucp.o: uucp.h conf.h policy.h system.h getopt.h sysdep.h
uuchk.o: uucp.h conf.h policy.h port.h sysdep.h system.h uutime.h getopt.h
uustat.o: uucp.h conf.h policy.h system.h sysdep.h getopt.h
uuname.o: uucp.h conf.h policy.h system.h sysdep.h getopt.h
uulog.o: uucp.h conf.h policy.h system.h sysdep.h getopt.h
tstuu.o: uucp.h conf.h policy.h sysdep.h getopt.h
uudir.o: uucp.h conf.h policy.h sysdep.h
prot.o: uucp.h conf.h policy.h system.h prot.h port.h sysdep.h
protg.o: uucp.h conf.h policy.h system.h prot.h port.h sysdep.h
protf.o: uucp.h conf.h policy.h prot.h port.h sysdep.h system.h
prott.o: uucp.h conf.h policy.h prot.h port.h sysdep.h system.h
prote.o: uucp.h conf.h policy.h prot.h port.h sysdep.h system.h
config.o: uucp.h conf.h policy.h system.h sysdep.h
v2.o: uucp.h conf.h policy.h port.h sysdep.h
bnu.o: uucp.h conf.h policy.h port.h sysdep.h
file.o: uucp.h conf.h policy.h system.h
util.o: uucp.h conf.h policy.h
sysinf.o: uucp.h conf.h policy.h system.h port.h sysdep.h uutime.h
time.o: uucp.h conf.h policy.h uutime.h
log.o: uucp.h conf.h policy.h system.h
chat.o: uucp.h conf.h policy.h system.h port.h sysdep.h
port.o: uucp.h conf.h policy.h port.h sysdep.h
tcp.o: uucp.h conf.h policy.h port.h sysdep.h system.h
prtinf.o: uucp.h conf.h policy.h port.h sysdep.h
parse.o: uucp.h conf.h policy.h
copy.o: uucp.h conf.h policy.h system.h sysdep.h
sys1.o: uucp.h conf.h policy.h system.h sysdep.h
sys2.o: uucp.h conf.h policy.h system.h port.h sysdep.h
sys3.o: uucp.h conf.h policy.h system.h sysdep.h
sys4.o: uucp.h conf.h policy.h system.h sysdep.h
sys5.o: uucp.h conf.h policy.h system.h sysdep.h
sys6.o: uucp.h conf.h policy.h system.h sysdep.h
sys7.o: uucp.h conf.h policy.h system.h sysdep.h
version.o: uucp.h conf.h policy.h
getopt.o: getopt.h
alloca.o: uucp.h conf.h policy.h
