#
# Makefile for compiling Perl under OS/2
#
# Needs a Unix compatible make.
# This makefile works for an initial compilation.  It does not
# include all dependencies and thus is unsuitable for serious
# development work.  Hey, I'm just inheriting what Diomidis gave me.
#
# Originally by Diomidis Spinellis, March 1990
# Adjusted for OS/2 port by Raymond Chen, June 1990
#

# Source files
SRC = array.c cmd.c cons.c consarg.c doarg.c doio.c dolist.c dump.c \
eval.c form.c hash.c perl.y perly.c regcomp.c regexec.c \
stab.c str.c toke.c util.c os2.c popen.c director.c suffix.c mktemp.c

# Object files
OBJ = perl.obj array.obj cmd.obj cons.obj consarg.obj doarg.obj doio.obj \
dolist.obj dump.obj eval.obj form.obj hash.obj perly.obj regcomp.obj \
regexec.obj stab.obj str.obj toke.obj util.obj os2.obj popen.obj \
director.obj suffix.obj mktemp.obj

# Files in the OS/2 distribution
DOSFILES=config.h director.c dir.h makefile os2.c popen.c suffix.c \
mktemp.c readme.os2

# Yacc flags
YFLAGS=-d

# Manual pages
MAN=perlman.1 perlman.2 perlman.3 perlman.4

CC=cl
# CBASE = flags everybody gets
# CPLAIN = flags for modules that give the compiler indigestion
# CFLAGS = flags for milder modules
# PERL = which version of perl to build
#
# For preliminary building:  No optimization, DEBUGGING set, symbols included.
#CBASE=-AL -Zi -G2 -Gs -DDEBUGGING
#CPLAIN=$(CBASE) -Od
#CFLAGS=$(CBASE) -Od
#PERL=perlsym.exe

# For the final build:  Optimization on, symbols stripped.
CBASE=-AL -Zi -G2 -Gs -DDEBUGGING
CPLAIN=$(CBASE) -Olt
CFLAGS=$(CBASE) -Oeglt
PERL=perl.exe

# Destination directory for executables
DESTDIR=\usr\bin

# Deliverables
#
all: $(PERL) glob.exe

perl.exe: $(OBJ) perl.arp
	link @perl.arp,perl,nul,/stack:32767 /NOE;
	exehdr /nologo /newfiles /pmtype:windowcompat perl.exe >nul

perlsym.exe: $(OBJ) perl.arp
	link @perl.arp,perlsym,nul,/stack:32767 /NOE /CODE;
	exehdr /nologo /newfiles /pmtype:windowcompat perlsym.exe >nul

perl.arp:
	echo array+cmd+cons+consarg+doarg+doio+dolist+dump+ >perl.arp
	echo eval+form+hash+perl+perly+regcomp+regexec+stab+suffix+ >>perl.arp
	echo str+toke+util+os2+popen+director+\c600\lib\setargv >>perl.arp

glob.exe: glob.c
	$(CC) glob.c setargv.obj -link /NOE
	exehdr /nologo /newfiles /pmtype:windowcompat glob.exe >nul

array.obj: array.c
	$(CC) $(CPLAIN) -c array.c
cmd.obj: cmd.c
cons.obj: cons.c perly.h
consarg.obj: consarg.c
#	$(CC) $(CPLAIN) -c consarg.c
doarg.obj: doarg.c
doio.obj: doio.c
dolist.obj: dolist.c
dump.obj: dump.c
eval.obj: eval.c evalargs.xc
	$(CC) /B2c2l /B3c3l $(CFLAGS) -c eval.c
form.obj: form.c
hash.obj: hash.c
perl.obj: perl.y
perly.obj: perly.c
regcomp.obj: regcomp.c
regexec.obj: regexec.c
stab.obj: stab.c
	$(CC) $(CPLAIN) -c stab.c
str.obj: str.c
suffix.obj: suffix.c
toke.obj: toke.c
	$(CC) /B3c3l $(CFLAGS) -c toke.c
util.obj: util.c
#	$(CC) $(CPLAIN) -c util.c
perly.h: ytab.h
	cp ytab.h perly.h
director.obj: director.c
popen.obj: popen.c
os2.obj: os2.c

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
	mv $(DOSFILES) ../os2
