# CKUKER.MAK, Version 2.04, 31 July 85
#
# -- Makefile to build C-Kermit for Unix and Unix-like systems --
# set up to build 4.4 version by default
#
# Read instructions below before proceeding, then
# rename this file to "makefile" or "Makefile", and then:
#
# for Berkeley Unix 4.4, "make bsd44"
# for Berkeley Unix 4.x, "make bsd"
# for Berkeley Unix 2.9 (PDP-11), "make bsd29"
# for AT&T 3Bx systems, "make att3bx"
# for AT&T generic System III/System V, "make sys3" or "make sys3nid"
# for Bell Unix Version 7 (aka 7th Edition), "make v7"
# for Microsoft Xenix (/286, PC/AT, etc), "make xenix"
# for Interactive System III (PC/IX) on PC/XT, "make pcix"
# for Interactive Sys III on other systems, "make is3"
# for DEC Pro-350 with Pro/Venix V1.x, "make provx1"
# for DEC Pro-350 with Pro/Venix V2.0 (Sys V), "make sys3nid" 
# for DEC Pro-380 with Pro/Venix V2.0 (Sys V), "make sys3" or "make sys3nid"
# for NCR Tower 1632, OS 1.02, "make tower1"
# for NCR Tower 1632 with System V, "make sys3"
# for Fortune 16:32, For:Pro 1.7, "make ft17"
# for Valid Scaldstar, "make valid"
# for BBN C/70 with IOS 2.0, "make c70"
# for Amdahl UTS 2.4 on IBM 370 series & compatible mainframes, "make uts24"
#
##############################################################################
#
# Notes:
#
#  In many cases, the -O (optimize) compiler switch is omitted.  Feel free
#  to add it if you trust your optimizer.  The ckuus2.c module, in particular,
#  tends to make optimizers blow up.
#
#  "make bsd" should produce a working C-Kermit for both 4.1 and 4.2bsd
#  on VAX, SUN, and Pyramid computers.
#
#  Either "make sys3" or "make sys3nid" tendss to produce a working version on
#  any ATT System III or System V system, including Motorola Four Phase, Callan
#  Unistar, Cadmus, NCR Tower, HP9836 Series 200, Plexus, Masscomp/RTU,
#  Heurikon, etc etc (for exceptions, see below; AT&T 3Bx systems have their
#  own entry).  As far as C-Kermit goes, there is no functional difference
#  between ATT System III and System V, so there is no need for a separate
#  "make sys5" entry.
#
#  "make sys3nid" is equivalent to "make sys3" but leaves out the -i option,
#  which is used indicate that separate instruction and data (text) spaces are
#  to be used, as on a PDP-11.  Some systems don't support this option, others
#  may require it.  If one of these options doesn't work on your System III
#  or System V system, try the other.
#
#  For version 7, several variables must be defined to the values
#  associated with your system.  BOOTNAME=/edition7 is the kernel image on
#  okstate's Perkin-Elmer 3230.  Others will probably be /unix.  PROCNAME=proc
#  is the name of the structure assigned to each process on okstate's system.
#  This may be "_proc" or some other variation.  See <sys/proc.h> for more info
#  on your systems name conventions.  NPROCNAME=nproc is the name of a
#  Kernal variable that tells how many "proc" structures there are.  Again
#  this may be different on your system, but nproc will probably be somewhere.
#  The variable NPTYPE is the type of the nproc variable -- int, short, etc.
#  which can probably be gleaned from <sys/param.h>.
#  The definition of DIRECT is a little more in depth.  If nlist() returns,
#  for "proc" only, the address of the array, then you should define DIRECT
#  as it is below.  If however, nlist() returns the address of a pointer to
#  the array, then you should give DIRECT a null definition (DIRECT= ).  The
#  extern declaration in <sys/proc.h> should clarify this for you.  If it
#  is "extern struct proc *proc", then you should NOT define DIRECT.  If it
#  is "extern struct proc proc[]", then you should probably define DIRECT as
#  it is below.  See ckuv7.hlp for further information.
#
#  For 2.9bsd, the makefile uses pcc rather than cc for compiles; that's what
#  the CC and CC2 definitions are for.  2.9 support basically follows the 4.2
#  path, except with a normal file system (file names 14 chars max).
#
#  The v7 and 2.9bsd versions assume I&D space on a PDP-11.  When building
#  C-Kermit for v7 on a PDP-11, you should probably add the -i option to
#  the link flags.  Without I&D space, overlays would probably have to be
#  used (or code mapping a`la Pro/Venix if that's available).
#
#  Other systems require some special treatment:
#
#  For HP9000 series 500, use "make sys3nid", but
#  1. In ckutio.c, don't #include <sys/file.h> or ioctl.h.
#  2. In ckufio.c, don't #include <sys/file.h>.
#
#  For Ridge32 (ROS3.2), use "make sys3", but
#  1. Use "CFLAGS = -DUXIII -i -O" "LNKFLAGS = -i"
#  2. Don't #include <sys/file.h> in cku[tf]io.c.
#
#  For Whitechapel MG-1 Genix 1.3, use "make bsd", but
#  1. In ckufio.c, have zkself() return 0 or call getpid, rather than getppid.
#  2. Wart reportedly can't process ckcpro.w; just work directly from ckcpro.c.
#
#  For Altos 986 with Xenix 3.0, use "make sys3", but
#  1. Get rid of any "(void)"'s (they're only there for Lint anyway)
#  2. In ckcdeb.h, define CHAR to be "char" rather than "unsigned char".
#
##############################################################################
#
#  V7-specific variables.
#  These are set up for Perkin-Elmer 3230 V7 Unix:
# 
PROC=proc
DIRECT=
NPROC=nproc
NPTYPE=int
BOOTFILE=/edition7
#
# ( For TRS-80 Xenix, use PROC=_proc, DIRECT=-DDIRECT, NPROC=_Nproc, 
#   NPTYPE=short, BOOTFILE=/xenix )
#
###########################################################################
#
#  Compile and Link variables:
#
LNKFLAGS=
CC= cc
CC2= cc
#
###########################################################################

# added for 4.3 bsd release; remove lines through the next comment
# to regenerate the generic form
CFLAGS= -O -DBSD4 -DDEBUG -DTLOG

all:	bsd44
	mv wermit kermit

.SUFFIXES: .1 .0

.1.0:
	../../man/manroff $*.1 > $*.0

install: ckuker.0
	install -s -o bin -g bin -m 755 kermit $(DESTDIR)/usr/new/kermit
	install -c -o bin -g bin -m 444 ckuker.0 $(DESTDIR)/usr/new/man/cat1/kermit.0

# end of 4.3 specific

#
# Dependencies Section:
#
make: 
	@echo 'Make what?  You must tell which system to make C-Kermit for.'

wermit: ckcmai.o ckucmd.o ckuusr.o ckuus2.o ckuus3.o ckcpro.o ckcfns.o \
		 ckcfn2.o ckucon.o ckutio.o ckufio.o ckudia.o ckuscr.o
	$(CC2) -o wermit ckcmai.o ckutio.o ckufio.o ckcfns.o \
		 ckcfn2.o ckcpro.o ckucmd.o ckuus2.o ckuus3.o ckuusr.o \
		 ckucon.o ckudia.o ckuscr.o $(LNKFLAGS)

ckcmai.o: ckcmai.c ckcker.h ckcdeb.h

ckuusr.o: ckuusr.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h

ckuus2.o: ckuus2.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h

ckuus3.o: ckuus3.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h

ckucmd.o: ckucmd.c ckucmd.h ckcdeb.h

ckcpro.o: ckcpro.c ckcker.h ckcdeb.h

ckcpro.c: ckcpro.w wart
	./wart ckcpro.w ckcpro.c

ckcfns.o: ckcfns.c ckcker.h ckcdeb.h

ckcfn2.o: ckcfn2.c ckcker.h ckcdeb.h

ckufio.o: ckufio.c ckcker.h ckcdeb.h

ckutio.o: ckutio.c ckcdeb.h

ckucon.o: ckucon.c ckcker.h ckcdeb.h

wart: ckwart.o
	$(CC) $(LNKFLAGS) -o wart ckwart.o

ckwart.o: ckwart.c

ckudia.o: ckudia.c ckcker.h ckcdeb.h

ckuscr.o: ckuscr.c ckcker.h ckcdeb.h
#
###########################################################################
#
# Make commands for specific systems:
#

bsd44:
	make wermit "CFLAGS= -DBSD4 -DDEBUG -DTLOG" "LNKFLAGS= -lcompat"
#
#Berkeley Unix 4.1 or 4.2 (and presumably also 4.3)
bsd:
	make wermit "CFLAGS= -DBSD4 -DDEBUG -DTLOG"



#Berkeley Unix 2.8, 2.9 for PDP-11s with I&D space
bsd29:
	make wermit "CFLAGS= -DV7 -DDEBUG -DTLOG" "LNKFLAGS= -i" "CC= pcc " \
		"CC2= cc"


#Version 7 Unix
v7:
	make wermit "CFLAGS=-DV7 -DDEBUG -DTLOG -DPROCNAME=\\\"$(PROC)\\\" \
	-DBOOTNAME=\\\"$(BOOTFILE)\\\" -DNPROCNAME=\\\"$(NPROC)\\\" \
	-DNPTYPE=$(NPTYPE) $(DIRECT)"

#In case they type "make sys5"...
sys5:
	make sys3


#Generic ATT System III or System V (with I&D space)
sys3:
	make wermit "CFLAGS = -DUXIII -DDEBUG -DTLOG -i -O" "LNKFLAGS = -i"


#Generic ATT System III or System V (no I&D space)
sys3nid:
	make wermit "CFLAGS = -DUXIII -DDEBUG -DTLOG -O" "LNKFLAGS ="


#AT&T 3B-series computers running System V
#  Only difference from sys3 is lock file stuff...
att3bx:
	make wermit "CFLAGS = -DUXIII -DATT3BX -DDEBUG -DTLOG -i -O" \
		"LNKFLAGS = -i"


#Microsoft "Xenix/286" e.g. for IBM PC/AT
xenix:
	make wermit "CFLAGS= -DXENIX -DUXIII -DDEBUG -DTLOG -F 3000 -i" \
		"LNKFLAGS = -F 3000 -i"


#PC/IX, Interactive Corp System III for IBM PC/XT
pcix:
	make wermit \
	"CFLAGS= -DPCIX -DUXIII -DISIII -DDEBUG -DTLOG -Dsdata=sdatax -O -i" \
		"LNKFLAGS = -i"


#Interactive Corp System III port in general --
is3:
	make wermit \
		"CFLAGS = -DISIII -DUXIII -DDEBUG -DTLOG -Ddata=datax -O -i" \
		"LNKFLAGS = -i"


#DEC Pro-3xx with Pro/Venix V1.0 or V1.1
# Requires code-mapping on non-I&D-space 11/23 processor, plus some
# fiddling to get interrupt targets into resident code section.
provx1:
	make wart "CFLAGS= -DPROVX1" "LNKFLAGS= "
	make wermit "CFLAGS = -DPROVX1 -DDEBUG -DTLOG -md780" \
		"LNKFLAGS= -u _sleep -lc -md780"


#NCR Tower 1632, OS 1.02
tower1:
	make wermit "CFLAGS= -DDEBUG -DTLOG -DTOWER1"

#Fortune 16:32, For:Pro 1.7 (mostly like 4.1bsd)
ft17:
	make wermit "CFLAGS= -DDEBUG -DTLOG -DBSD4 -DFT17"


#Valid Scaldstar
#Berkeleyish, but need to change some variable names.
valid:
	make wermit "CFLAGS= -DBSD4 -Dcc=ccx -DFREAD=1"


#Amdahl UTS 2.4 on IBM 370 series compatible mainframes.
#Mostly like V7, but can't do initrawq() buffer peeking.
uts24:
	make wermit "CFLAGS=-DV7 -DDEBUG -DTLOG -DPROCNAME=\\\"$(PROC)\\\" \
	-DUTS24 -DBOOTNAME=\\\"$(BOOTFILE)\\\" -DNPROCNAME=\\\"$(NPROC)\\\" \
	-DNPTYPE=$(NPTYPE) $(DIRECT)"


#BBN C/70 with IOS 2.0
#Mostly Berkeley-like, but with some ATTisms
c70:
	make wermit "CFLAGS= -DBSD4 -DC70 -DDEBUG -DTLOG"

clean:
	rm -f *.o ckcpro.c wart wermit core make.out xerrs errs kermit
