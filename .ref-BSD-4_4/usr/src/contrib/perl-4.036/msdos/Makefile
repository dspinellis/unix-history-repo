#
# Makefile for compiling Perl under MS-DOS
#
# Needs a Unix compatible make.
# This makefile works for an initial compilation.  It does not
# include all dependencies and thus is unsuitable for serious
# development work.  But who would do serious development under
# MS-DOS?
#
# By Diomidis Spinellis, March 1990
#

# Source files
SRC = array.c cmd.c cons.c consarg.c doarg.c doio.c dolist.c dump.c \
eval.c form.c hash.c perl.y perly.c regcomp.c regexec.c \
stab.c str.c toke.c util.c msdos.c popen.c directory.c

# Object files
OBJ = perl.obj array.obj cmd.obj cons.obj consarg.obj doarg.obj doio.obj \
dolist.obj dump.obj eval.obj form.obj hash.obj perly.obj regcomp.obj \
regexec.obj stab.obj str.obj toke.obj util.obj msdos.obj popen.obj \
directory.obj

# Files in the MS-DOS distribution
DOSFILES=config.h dir.h director.c glob.c makefile msdos.c popen.c readme.msd \
changes.dds wishlist.dds patches manifest

# Yacc flags
YFLAGS=-d

# Manual pages
MAN=perlman.1 perlman.2 perlman.3 perlman.4

CC=cc
# Cflags for the files that break under the optimiser
CPLAIN=-AL -DCRIPPLED_CC
# Cflags for all the rest
CFLAGS=$(CPLAIN) -Ox
# Destination directory for executables
DESTDIR=\usr\bin

# Deliverables
all: perl.exe perl.1 glob.exe

perl.exe: $(OBJ)
	echo array+cmd+cons+consarg+doarg+doio+dolist+dump+ >perl.arp
	echo eval+form+hash+perl+perly+regcomp+regexec+ >>perl.arp
	echo stab+str+toke+util+msdos+popen+directory+\lib\setargv >>perl.arp
	echo perl.exe >>perl.arp
	echo nul >>perl.arp
	echo /stack:32767 /NOE >>perl.arp
	link @perl.arp

glob.exe: glob.c
	$(CC) glob.c \lib\setargv.obj -link /NOE

array.obj: array.c
cmd.obj: cmd.c
cons.obj: cons.c perly.h
consarg.obj: consarg.c
	$(CC) $(CPLAIN) -c consarg.c
doarg.obj: doarg.c
doio.obj: doio.c
dolist.obj: dolist.c
dump.obj: dump.c
eval.obj: eval.c evalargs.xc
form.obj: form.c
hash.obj: hash.c
perl.obj: perl.y
perly.obj: perly.c
regcomp.obj: regcomp.c
regexec.obj: regexec.c
stab.obj: stab.c
str.obj: str.c
toke.obj: toke.c
util.obj: util.c
	$(CC) $(CPLAIN) -c util.c
perly.h: perl.obj
	mv ytab.h perly.h
directory.obj: directory.c
popen.obj: popen.c
msdos.obj: msdos.c

perl.1: $(MAN)
	nroff -man $(MAN) >perl.1

install: all
	exepack perl.exe $(DESTDIR)\perl.exe
	exepack glob.exe $(DESTDIR)\glob.exe

clean:
	rm -f *.obj *.exe perl.1 perly.h perl.arp

tags:
	ctags *.c *.h *.xc

dosperl:
	mv $(DOSFILES) ../perl30.new

doskit:
	mv $(DOSFILES) ../msdos
