# CKUKER.MAK, Sat Feb  8 23:17:01 1992
#
CKVER= "5A(179) BETA"
#
# 386bsd-full has all the available features compiled in
# 386bsd-minimal has only the required set for a minimum size executable
#
default : 386bsd-full
#
# -- Makefile to build C-Kermit 5A for UNIX and UNIX-like systems --
#
# Author: Frank da Cruz, Columbia University Center for Computing Activities
# 612 West 115th Street, New York, NY 10025, USA.  Phone (212) 854-5126.
# e-mail: fdc@watsun.cc.columbia.edu, fdc@columbia.edu, or FDCCU@CUVMA.BITNET.
#
# Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
# York.  Permission is granted to any individual or institution to use this
# software as long as it is not sold for profit.  This copyright notice must be
# retained.  This software may not be included in commercial products without
# written permission of Columbia University.
#
# WARNING: This is a Beta test release.  Use at your own risk.
#
# WARNING #2: This is a huge makefile, and it contains nested makes.
# Some make programs run out of memory.  If this happens to you (as it will,
# for example, for 2.xBSD on PDP-11s), edit away all the parts that do not 
# apply to your system and try again.
#
#   C-Kermit can also be built for many other systems not supported by
#   this makefile, including the Apple Macintosh, the Commodore Amiga,
#   OS-9, IBM OS/2, DEC VAX/VMS.  See the ckaaaa.hlp file for information.
#   Support was also added to C-Kermit for some other systems like Data
#   General AOS/VS and Apollo Aegis, but the system-dependent modules for
#   these versions need a lot of work to bring them up to 5A level.
#
# INSTALLATION NOTES:
#
# Rename this file to "makefile" or "Makefile" if necessary.  Pick out the
# make entry most appropriate for your UNIX system from the list below and
# then give the appropriate "make" command, for example "make bsd", "make
# sys5r3", "make posix".  For more detailed installation instructions,
# read the file ckuins.doc.  For descriptions of known problems and
# limitations, read the file ckuker.bwr (the "beware file").
#
# Some of the implementations listed below are untested for v5A:
# + Marks those that have been tested successfully.
# - Marks those that are known not to work.
# ? Marks those yet untested.
#
# Those marked with + are not guaranteed to work.  The "+" means that C-Kermit
# 5A was built successfully and tested to some degree without showing obvious
# problems, but not necessarily in the current edit.  That is, something that
# has been done to the program in recent edits might have broken a previously
# working version.
#
# Please report failures (preferably with fixes) or successes to the author.
#
# MAKE COMMANDS FOR DIFFERENT UNIX VERSIONS:
#
# + for Alliant FX/8 with Concentrix 4.1 or later, "make bsdlck"
# + for Altos 486, 586, 986 with Xenix 3.0, "make altos"
# ? for Amdahl UTS 2.4 on IBM 370 series & compatible mainframes, "make uts24"
# + for Amdahl UTSV IBM 370 series & compatible mainframes, "make utsv"
# + for Amdahl mainframes with with UNIX System V R 5.2.6b 580, "make sys3"
# ? for Apollo DOMAIN/IX, "make bsd" or "make sys3", for desired environment
# ? for Apollo DOMAIN/IX, if the above fails, try "make apollobsd"
# + for Apollo with SR10.0 or later, BSD environment, "make sr10-bsd"
# + for Apollo with SR10.0 or later, System V environment, "make sr10-s5r3"
# ? for Apollo with straight Aegis using native Aegis i/o,
#     give "cc" commands for each module, then "bind" to link them together.
# + for Apple Macintosh II with A/UX, "make aux" or "make auxufs"
# ? for Apple Macintosh with Minix 1.5.10, "make minix68k" or "make minixc68"
# + for AT&T 6300 PLUS, "make att6300" or (with no debugging) "make att6300nd"
# + for AT&T 6386 WGS Unix PC, "make sys5r3"
# + for AT&T 3B2, 3B20 systems, "make att3bx"
# + for AT&T 3B1, 7300 Unix PC, "make sys3upc", "sys3upcg" (gcc), "sys3upcold"
# + for AT&T System III/System V R2 or earlier, "make sys3" or "make sys3nid"
# + for AT&T System III/System V with Honey DanBer UUCP, "make sys3hdb"
# + for AT&T System V on DEC VAX, "make sys3" or "make sys5r3"
# + for AT&T System V R3, use "make sys5r3".  This is different from the above.
# + for AT&T System V R4, "make sys5r4", "make sys5r4sx", or "make sys5r4nx",
#     or if the ANSI C function prototyping makes trouble, add -DNOANSI,
#     as in "sys5r4sxna" entry (used with AT&T System V/386 R 4.0 V 2.1.
# + for AT&T System V R4 with Wollongong TCP/IP, "make sys5r4net", ...
# + for Atari ST with Minix ST 1.5.10.3, "make minix68k" or "make minixc68"
# ? for BBN C/70 with IOS 2.0, "make c70"
# + for Bell Unix Version 7 (aka 7th Edition), "make v7" (but see below)
# + for Berkeley Unix 2.4, "make v7" (but read v7 material below)
# ? for Berkeley Unix 2.9 (DEC PDP-11 or Pro-3xx), "make bsd29"
# + for Berkeley Unix 2.10 "make bsd210" (but read the makefile entry first)
# + for Berkeley Unix 2.11 "make bsd210" (but read the makefile entry first)
# + for Berkeley Unix 4.1, "make bsd41"
# + for Berkeley Unix 4.2, "make bsd" (tested with 4.2 and 4.3)
# + for Berkeley Unix 4.2 or 4.3 with HoneyDanBer UUCP, "make bsdhdb"
# + for Berkeley Unix 4.3, "make bsd43" (uses acucntrl program for locks)
# + for Berkeley Unix 4.3 without acucntrl program, "make bsdlck" or "make bsd"
# + for Berkeley Unix 4.3-Tahoe, same as 4.3 BSD.
# ? for Berkeley Unix 4.3-Reno, same as 4.3 BSD.
# ? for Berkeley Unix 4.4, maybe "make posix"?
# ? for Cadmus, "make sys3"
# ? for Callan, "make sys3"
# ? for CDC VX/VE 5.2.1 Sys V emulation, "make vxve"
# ? for Charles River Data Systems 680x0 systems with Unos, "make sys3nid"
# ? for CIE Systems 680/20 with Regulus, "make cie"
# + for Commodore Amiga 3000UX Sys V R4, "make sys5r4sx"
# + for Commodore Amiga 3000UX Sys V R4 and TCP/IP, "make svr4amiganet"
# ? for Commodore Amiga with Minix 1.5.10, "make minix68k" of "make minixc68"
# + for Convergent with CTIX Sys V R2, "make sys5"
# + for Convex C1, "make convex"
# + for Convex C210 with Convex/OS 8, "make convex8"
# + for Convex C2 with Convex/OS 9.1, "make convex9"
# + for Cray X/MP or Y/MP with Unicos System V R3, "make cray"
# + for Cyber 910 (Silicon-Graphics Iris) with Irix 3.3, "irix33"
# + for Data General Aviion with Sys V R4 (DG UX 5.40), "make dgux540"
#     (compile ckwart separately if necessary)
# + for Data General Aviion, earlier UNIX versions,
#     "make sys5r3" (maybe compile ckwart separately)
# ? for Data General MVxxxxx with DG/UX, ???
# + for DEC VAX (or DECstation?) with Ultrix 1.x "make bsd"
# + for DEC VAX (or DECstation?) with Ultrix 2.x "make du2"
# + for DEC VAX (or DECstation?) with Ultrix 3.0, 3.1, "make du3"
# + for DECstation (or VAX) with Ultrix 4.0 or 4.1, "make du4"
# + for DECstation (or VAX) with Ultrix 4.x and Kanji support, "make du4kanji"
# + for DECstation (or VAX) with Ultrix 4.2, "make du42"
# + for DECstation (or VAX) with Ultrix 4.2, Sys V R4 world, "make du42s5r4"
# + for DECstation (or VAX) with Ultrix 4.x, POSIX world, "make posix"
# + for DECstation (or VAX) with Ultrix 4.0 or 4.1, "make du4"
# ? for DEC Pro-350 with Pro/Venix V1.x, "make provx1"
# ? for DEC Pro-350 with Pro/Venix V2.0 (Sys V), "make sys3nid" 
# ? for DEC Pro-380 with Pro/Venix V2.0 (Sys V), "make sys3" or "make sys3nid"
# ? for DEC Pro-380 with 2.9, 2.10, or 2.11 BSD, "make bsd29" or "make bsd210"
# + for Dell UNIX Issue 2.1 (= AT&T Sys V/386 R4.3 + fixes), "make sys5r4sxtcp"
# + for Dell UNIX Issue 2.2 (= AT&T Sys V/386 R4.4 + fixes), "make sys5r4sxtcp"
# + for DIAB DS90 with DNIX (any version) create an empty <sys/file.h>
#     if this file does not already exist (or add -DNOFILEH to the make entry).
# + for DIAB DS90 or LUXOR ABC-9000 with pre-5.2 DNIX, add "getcwd" to libc.a
#     (search below for "getcwd"), then "make dnixold".
# + for DIAB DS90 with DNIX 5.2 (Sys V.2) or earlier, "make dnix" or "dnixnd".
# + for DIAB DS90 with DNIX 5.3 (Sys V.3), "make dnix5r3"
# + for DIAB DS90 with DNIX 5.3 (Sys V.3) and TCP/IP, "make dnix5r3net"
# ? for DIAB DS90 with DNIX 5.3 (Sys V.3), ANSI C, "make dnix5r3ansi"
# + for Encore Multimax 310, 510 with UMAX 4.2, "make umax42"
# + for Encore Multimax 310, 510 with UMAX 4.3, "make umax43"
# + for Encore Multimax 310, 510 with UMAX V 2.2, use Berkeley cc, "make bsd"
# + for Encore 88K with UMAX V 5.2, "make encore88k"
# + for ESIX System V R4.0.3 with TCP/IP support, "make esixr4"
# ? for Fortune 32:16, For:Pro 1.8, "make ft18"
# + for Fortune 32:16, For:Pro 2.1, "make ft21"
# + for FPS 500 with FPX 4.1, "made bsd"
# ? for Heurikon, "make sys3"
# ? for HP-3000, MPE/ix, "make posix"?
# + for HP-9000 Series, HP-UX < 6.5, without long filenames, no job control,
#   "make hpuxpre65"
# + for HP-9000 Series, HP-UX pre-7.0, without long filenames, "make hpux"
# ? for HP-9000 Series, HP-UX 7.0 or later, no long filenames, "make hpux7sf"
# + for HP-9000 Series with HP-UX Sys V R2, BSD long names, "make hpuxlf"
# + for HP-9000 Series with HP-UX Sys V R2, dirent long names, "make hpuxde"
# + for HP-9000 Series with HP-UX Sys V R3, "make hpuxs5r3"
# + for HP-9000 Series with HP-UX 7.0, TCP/IP, long filenames, "make hpux70lfn"
# + for HP-9000 Series with HP-UX 8.0, TCP/IP, long filenames, "make hpux80"
# ? for IBM 370 Series with IX/370, "make ix370"
# + for IBM 370 Series with AIX/370 3.0, "make aix370"
# + for IBM PC/AT & compatibles with Mark Williams Coherent OS,
#     command-line version only, "make coherent".
# ? for IBM PC/AT & compatibles with Mark Williams Coherent OS,
#     minimum interactive version, "make coherentmi"
# ? for IBM PC/AT & compatibles with original MINIX, "make minix".
# + for IBM PC/AT & compatibles with MINIX, new compiler, "make minixnew"
# + for IBM PC family, 386-based, with MINIX/386, "make minix386"
#     or if you have GNU CC, "make minix386gcc"
# + for IBM PS/2 with PS/2 AIX 1.0, 1.1, or 1.2, "make ps2aix"
# + for IBM PS/2 with PS/2 AIX 3.0, "make ps2aix3"
# + for IBM RISC System/6000 with AIX 3.0, "make rs6000"
# + for IBM RISC System/6000 with AIX 3.1, "make rs6000"
# ? for IBM RISC System/6000 with AIX 3.2 (OSF/AES compliant), "???"
# ? for IBM RT PC with AIX 2.1, "make sys3"
# + for IBM RT PC with AIX 2.2.1, "make rtaix" (special lockfile handling)
# ? for IBM RT PC with ACIS 4.2, "make bsd"
# + for IBM RT PC with ACIS 4.3, "make rtacis" or "make bsd KFLAGS=-DNOANSI"
# + for Intel 302 with Bell Tech Sys V/386 R3.2, "make sys5r3"
# ? for Intel Xenix/286, "make sco286"
# ? for Interactive System III (PC/IX) on PC/XT, "make pcix"
# ? for Interactive System III on other systems, "make is3"
# + for Interactive 386/ix 2.x, "make is5r3"
# + for Interactive 386/ix 2.x with Ethernet, "make is5r3net" or "is5r3netg"
# + for Luxor ABC-9000 (DIAB DS-90) with pre-5.2 DNIX, add "getcwd" to libc.a
#     (search below for "getcwd"), then "make dnixold".
# ? for Masscomp RTU AT&T System III, "make rtu"
# + for Masscomp/Concurrent with RTU 4.0 or later, BSD environment, "make 
#     rtubsd", "make rtubsd2", "make rtubsd3" (depending on where ndir.h 
#     is stored, see entries below).
# ? for Masscomp/Concurrent with RTU 4.0 or later, System V environment,
#     "make rtus5" (System V R2) or "make rtus5r3" (System V R3 or later).
# + for Microport SV/AT (System V R2), "make mpsysv"
# + for Microport SVR4 2.2, "make sys5r4"
# ? for Microsoft,IBM Xenix (/286, PC/AT, etc), "make xenix" or "make sco286"
# + for MIPS machine with AT&T System V R3.0, "make mips"
# + for Modcomp 9730, Real/IX, "make sys5r3" (or modify to use gcc = GLS cc)
# ? for Motorola Four Phase, "make sys3" or "make sys3nid"
# + for Motorola Delta System V/68 R3, "make sv68r3"
# + for Motorola Delta System V/88 R32, "make sv88r32"
# ? for NCR Tower 1632, OS 1.02, "make tower1"
# + for NCR Tower 1632 with System V, "make sys3"
# + for NCR Tower 32, OS Release 1.xx.xx, "make tower32-1"
# + for NCR Tower 32, OS Release 2.xx.xx, "make tower32-2"
# + for NCR Tower 32, OS Releases based on Sys V R3, "make tower32"
# + for NCR Tower 32, OS Releases based on Sys V R3 with gcc "make tower32g"
# + for NeXT, "make next"
# + for OkiStation 7300 Series, "make sys5r4sxtcp" 
# + for Olivetti X/OS R.2.3, "make xos23"
# + for PFU Compact A Series SX/A TISP V10/E50 (Japan), "make sxae50"
# ? for Plexus, "make sys3"
# + for Pyramid, "make bsd" or "make pyramid"
# + for POSIX on anything, "make posix" (but adjustments might be necessary).
# + for POSIX on SUNOS 4.1 or later, "make sunposix"
# ? for Ridge 32 (ROS3.2), "make ridge32"
# ? for Samsung MagicStation, "make sys5r4"
# ? for SCO Xenix 2.2.1 with development system 2.2 on 8086/8 "make sco86"
# + for SCO Xenix/286 2.2.1 with development system 2.2 on 80286, "make sco286"
#     NOTE: reportedly this makefile is too long for SCO Xenix/286 make, but it
#     works with "makeL", or if some of the other make entries are edited out.
# + for SCO Xenix/386 2.2.2, "make sco386"
# + for SCO Xenix/386 2.3.x, "make sco3r2" or "make sco3r2x"
# + for SCO UNIX/386 3.2.0 or 3.2.1, "make sco3r2" or "make sco3r2x"
# + for SCO UNIX/386 3.2.2, "make sco3r22"
# + for SCO UNIX/386 3.2.2 with SCO TCP/IP, "make sco3r22net"
# + for SCO Xenix/386 or UNIX/386 with Excelan TCP/IP, "make sco3r2net"
# + for SCO Xenix 2.3.x with Racal InterLan TCP/IP, "make sco3r2netri"
# + for SCO Xenix 2.3.x with SCO (Lachman) TCP/IP, "make sco3r2lai"
# + for other Unix varieties with Racal Interlan TCP/IP, read sco3r2netri entry
# + for Sequent with DYNIX/PTX 1.2.1, "make dynix12"
# + for Sequent with DYNIX/PTX 1.3 with TCP/IP, "make dynix13"
# + for Sequent Balance 8000 or B8 with DYNIX 3.0.xx, "make dynix3"
# + for Sequent Symmetry S81 with DYNIX 3.0.xx, "make dynix3"
# + for Sequent DYNIX 3.1.xx, "make dynix31"
# + for Silicon Graphics Iris System V IRIX 3.2 or earlier, "make iris"
# + for Silicon Graphics Sys V R3 with IRIX 3.3 or later, "make sys5r3"
# + for Silicon Graphics Iris Indigo with IRIX 4.0, "make irix40"
# + for Solbourne 4/500 with OS/MP 4 "make sunos4"
# + for Solbourne 4/500 with OS/MP 4.1 "make sunos41"
# + for SONY NEWS with NEWS-OS 4.01C, "make sonynews"
# + for Sperry (Unisys) 5000-80, "make sys5r3"
# + for Stardent 1520, "make sys5r3"
# + for SUN with pre-4.0 SUNOS versions, "make bsd" (or appropriate variant)
# + for SUN with SUNOS 4.0, BSD environment, "make sunos4"
# + for SUN with SUNOS 4.0, BSD, with SunLink X.25, make sunos4x25
# + for SUN with SUNOS 4.0, AT&T Sys V R3 environment, "make sunos4s5"
# + for SUN with SUNOS 4.1 or 4.1.1, BSD environment, "make sunos41" 
# + for SUN with SUNOS 4.1, BSD, with SunLink X.25, "make sunos41x25"
# + for SUN with SUNOS 4.1, 4.1.1, AT&T Sys V R3 environment, "make sunos41s5" 
# + for SUN with SUNOS 4.1 or later, POSIX environment, "make sunposix"
# + for Tandy 16/6000 with Xenix 3.0, "make trs16"
# + for Tektronix 6130/4132/43xx (e.g.4301) with UTek OS, "make utek"
# + for Tri Star Flash Cache with Esix SVR3.2, "make sys5r3"
# ? for Unistar, "make sys3"
# ? for Valid Scaldstar, "make valid"
# ? for Whitechapel MG01 Genix 1.3, "make white"
# ? for Zilog ZEUS 3.21, "make zilog"
#
# The result should be a runnable program called "wermit" in the current 
# directory.  After satisfactory testing, you can rename wermit to "kermit" 
# and put it where users can find it.
#
# To remove intermediate and object files, "make clean".
# To run lint on the source files, "make lintsun", "make lintbsd",
# "make lints5", as appropriate.
#
##############################################################################
#
# NOTES FOR V7 AND 2.X BSD (BASED ON VERSION 4E OF C-KERMIT):
#
# For Unix Version 7, several variables must be defined to the values
# associated with your system.  BOOTNAME=/edition7 is the kernel image on
# okstate's Perkin-Elmer 3230.  Others will probably be /unix.  PROCNAME=proc
# is the name of the structure assigned to each process on okstate's system.
# This may be "_proc" or some other variation.  See <sys/proc.h> for more
# info on your systems name conventions.  NPROCNAME=nproc is the name of a
# kernel variable that tells how many "proc" structures there are.  Again
# this may be different on your system, but nproc will probably be somewhere.
# The variable NPTYPE is the type of the nproc variable -- int, short, etc.
# which can probably be gleaned from <sys/param.h>.  The definition of DIRECT
# is a little more complicated.  If nlist() returns, for "proc" only, the
# address of the array, then you should define DIRECT as it is below.  If
# however, nlist() returns the address of a pointer to the array, then you
# should give DIRECT a null definition (DIRECT= ).  The extern declaration in
# <sys/proc.h> should clarify this for you.  If it is "extern struct proc
# *proc", then you should NOT define DIRECT.  If it is "extern struct proc
# proc[]", then you should probably define DIRECT as it is below.  See
# ckuv7.hlp for further information.
#
# For 2.9 BSD, the makefile may use pcc rather than cc for compiles; that's
# what the CC and CC2 definitions are for (the current version of the
# makefile uses cc for both; this was tested in version 4E of C-Kermit and
# worked OK on the DEC Pro 380, but all bets are off for version 5A).  2.9
# support basically follows the 4.1 path.  Some 2.9 systems use "dir.h" for
# the directory header file, others will need to change this to "ndir.h".
# There are also newer entries for 2.10 BSD, which need testing and probably
# refinement, particularly a new entry that builds the program with overlays
# for DEC PDP-11s.
#
# The v7 and 2.9bsd versions assume I&D space on a PDP-11.  When building
# C-Kermit for v7 on a PDP-11, you should probably add the -i option to the
# link flags.  Without I&D space, overlays will be necessary (if available),
# or code segment mapping (a`la Pro/Venix if that's available).
#
##############################################################################
#
# V7-specific variables.
# These are set up for Perkin-Elmer 3230 V7 Unix:
# 
PROC=proc
DIRECT=
NPROC=nproc
NPTYPE=int
BOOTFILE=/edition7
#
# ( For old Tandy TRS-80 Model 16A or 6000 V7-based Xenix, use PROC=_proc,
#   DIRECT=-DDIRECT, NPROC=_Nproc, NPTYPE=short, BOOTFILE=/xenix )
#
###########################################################################
#
#  Compile and Link variables:
#
#  EXT is the extension (file type) for object files, normally o.
#  See MINIX entry for what to do if another filetype must be used.
#
EXT=o
#LNKFLAGS=
SHAREDLIB=
CC= sync;cc
CC2= sync;cc
SHELL=/bin/sh
#
###########################################################################
#
# Easy installation. Modify this to suit your own computer's file organization
# and permissions.  If you don't have write access to the destination
# directories, "make install" will fail.

WERMIT = makewhat
DESTDIR =
BINDIR = /usr/local/bin
MANDIR = /usr/man/manl
MANEXT = l
ALL = $(WERMIT)

all: $(ALL)

install: $(ALL)
	cp wermit $(DESTDIR)$(BINDIR)/kermit
	strip $(DESTDIR)$(BINDIR)/kermit
	chmod 755 $(DESTDIR)$(BINDIR)/kermit
	cp ckuker.nr $(DESTDIR)$(MANDIR)/kermit.$(MANEXT)
	chmod 644 $(DESTDIR)$(MANDIR)/kermit.$(MANEXT)

makewhat:
	@echo 'make what?  You must tell which system to make C-Kermit for.'
	@echo Examples:  make bsd, make sys3, make sunos4, etc.

###########################################################################
#
# Dependencies Section:

wermit:	ckcmai.$(EXT) ckucmd.$(EXT) ckuusr.$(EXT) ckuus2.$(EXT) ckuus3.$(EXT) \
		ckuus4.$(EXT) ckuus5.$(EXT) ckuus6.$(EXT) ckuus7.$(EXT) \
		ckuusx.$(EXT) ckuusy.$(EXT) ckcpro.$(EXT) ckcfns.$(EXT) \
		ckcfn2.$(EXT) ckcfn3.$(EXT) ckuxla.$(EXT) ckucon.$(EXT) \
		ckutio.$(EXT) ckufio.$(EXT) ckudia.$(EXT) ckuscr.$(EXT) \
		ckcnet.$(EXT)
	$(CC2) $(LNKFLAGS) -o wermit ckcmai.$(EXT) ckutio.$(EXT) \
		ckufio.$(EXT) ckcfns.$(EXT) ckcfn2.$(EXT) ckcfn3.$(EXT) \
		ckuxla.$(EXT) ckcpro.$(EXT) ckucmd.$(EXT) ckuus2.$(EXT) \
		ckuus3.$(EXT) ckuus4.$(EXT) ckuus5.$(EXT) ckuus6.$(EXT) \
		ckuus7.$(EXT) ckuusx.$(EXT) ckuusy.$(EXT) ckuusr.$(EXT) \
		ckucon.$(EXT) ckudia.$(EXT) ckuscr.$(EXT) ckcnet.$(EXT) $(LIBS)

#Malloc Debugging version

mermit:	ckcmdb.$(EXT) ckcmai.$(EXT) ckucmd.$(EXT) ckuusr.$(EXT) ckuus2.$(EXT) \
		ckuus3.$(EXT) ckuus4.$(EXT) ckuus5.$(EXT) ckuus6.$(EXT) \
		ckuus7.$(EXT) ckuusx.$(EXT) ckuusy.$(EXT) ckcpro.$(EXT) \
		ckcfns.$(EXT) ckcfn2.$(EXT) ckcfn3.$(EXT) ckuxla.$(EXT) \
		ckucon.$(EXT) ckutio.$(EXT) ckufio.$(EXT) ckudia.$(EXT) \
		ckuscr.$(EXT) ckcnet.$(EXT)
	$(CC2) $(LNKFLAGS) -o mermit ckcmdb.$(EXT) ckcmai.$(EXT) \
		ckutio.$(EXT) ckufio.$(EXT) ckcfns.$(EXT) ckcfn2.$(EXT) \
		ckcfn3.$(EXT) ckuxla.$(EXT) ckcpro.$(EXT) ckucmd.$(EXT) \
		ckuus2.$(EXT) ckuus3.$(EXT) ckuus4.$(EXT) ckuus5.$(EXT) \
		ckuus6.$(EXT) ckuus7.$(EXT) ckuusx.$(EXT) ckuusy.$(EXT) \
		ckuusr.$(EXT) ckucon.$(EXT) ckudia.$(EXT) ckuscr.$(EXT) \
		ckcnet.$(EXT) $(LIBS)

# Here is an example of building Kermit with overlays for a small machine,
# Like a PDP-11 without separate I&D space. This example comes from a 
# submission by Steven M Schultz <sms@wlv.imsd.contel.com>.

ovwermit: ckcmai.$(EXT) ckucmd.$(EXT) ckuusr.$(EXT) ckuus2.$(EXT) \
	ckuus3.$(EXT) ckuus4.$(EXT) ckuus5.$(EXT) ckcpro.$(EXT) \
	ckcfns.$(EXT) ckcfn2.$(EXT) ckcfn3.$(EXT) ckuxla.$(EXT) \
	ckucon.$(EXT) ckutio.$(EXT) ckufio.$(EXT) ckudia.$(EXT) \
	ckuscr.$(EXT) ckcnet.$(EXT) ckuus6.$(EXT) ckuus7.$(EXT) ckuusx.$(EXT) \
	ckuusy.$(EXT) ckustr.o strings.o
	ar x /lib/libc.a getpwent.o ctime.o ndbm.o
	$(CC2) $(LNKFLAGS) -o wermit ckcmai.$(EXT) \
		ckutio.$(EXT) ckufio.$(EXT) ckcfns.$(EXT) ckcfn2.$(EXT) \
		ckcfn3.$(EXT) \
		 -Z ckuxla.$(EXT) ckcpro.$(EXT) ckucmd.$(EXT) ckuus2.$(EXT) \
		    ckuus3.$(EXT) \
		 -Z ckuus4.$(EXT) ckuus5.$(EXT) ckuusr.$(EXT) ckuus6.$(EXT) \
		    ctime.o \
		 -Z ckuus7.$(EXT) ckcfn3.$(EXT) ckudia.$(EXT) ckuscr.$(EXT) \
		    ckcnet.$(EXT) ckuusy.$(EXT) \
		 -Z ckuusx.$(EXT) ckucon.$(EXT) getpwent.o ndbm.o \
		 -Y ckustr.o strings.o $(LIBS)

strings.o: strings
	xstr
	cc -c xs.c
	mv -f xs.o strings.o
	rm -f xs.c

# Dependencies for each module...
#
# The following almost makes this work with gcc on the Sun-4.
# .SUFFIXES: .c .$(EXT)
# .$(EXT).c: ;	$(CC) $(CFLAGS) -c $<

ckcmai.$(EXT): ckcmai.c ckcker.h ckcdeb.h ckcsym.h ckcasc.h ckcnet.h

ckcpro.$(EXT): ckcpro.c ckcker.h ckcdeb.h ckcasc.h

ckcpro.c: ckcpro.w wart ckcdeb.h ckcasc.h ckcker.h
	./wart ckcpro.w ckcpro.c

ckcfns.$(EXT): ckcfns.c ckcker.h ckcdeb.h ckcsym.h ckcasc.h ckcxla.h \
		ckuxla.h

ckcfn2.$(EXT): ckcfn2.c ckcker.h ckcdeb.h ckcsym.h ckcasc.h ckcxla.h ckuxla.h

ckcfn3.$(EXT): ckcfn3.c ckcker.h ckcdeb.h ckcsym.h ckcasc.h ckcxla.h \
		ckuxla.h

ckuxla.$(EXT): ckuxla.c ckcker.h ckcdeb.h ckcxla.h ckuxla.h

ckuusr.$(EXT): ckuusr.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h ckcxla.h ckuxla.h \
		ckcasc.h ckcnet.h

ckuus2.$(EXT): ckuus2.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h ckcxla.h ckuxla.h \
		ckcasc.h

ckuus3.$(EXT): ckuus3.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h ckcxla.h ckuxla.h \
		ckcasc.h ckcnet.h

ckuus4.$(EXT): ckuus4.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h ckcxla.h ckuxla.h \
		ckcasc.h ckcnet.h ckuver.h

ckuus5.$(EXT): ckuus5.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h ckcasc.h

ckuus6.$(EXT): ckuus6.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h ckcasc.h

ckuus7.$(EXT): ckuus7.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h ckcxla.h ckuxla.h \
		ckcasc.h ckcnet.h

ckuusx.$(EXT): ckuusx.c  ckcker.h ckuusr.h ckcdeb.h ckcasc.h

ckuusy.$(EXT): ckuusy.c  ckcker.h ckcdeb.h ckcasc.h

ckucmd.$(EXT): ckucmd.c ckcasc.h ckucmd.h ckcdeb.h

ckufio.$(EXT): ckufio.c ckcdeb.h ckuver.h

ckutio.$(EXT): ckutio.c ckcdeb.h ckcnet.h ckuver.h

ckucon.$(EXT): ckucon.c ckcker.h ckcdeb.h ckcasc.h ckcnet.h

ckcnet.$(EXT): ckcnet.c ckcdeb.h ckcker.h ckcnet.h

wart: ckwart.$(EXT)
	$(CC) $(LNKFLAGS) -o wart ckwart.$(EXT) $(LIBS)

ckcmdb.$(EXT): ckcmdb.c ckcdeb.h

ckwart.$(EXT): ckwart.c

ckudia.$(EXT): ckudia.c ckcker.h ckcdeb.h ckucmd.h ckcasc.h

ckuscr.$(EXT): ckuscr.c ckcker.h ckcdeb.h ckcasc.h

###########################################################################
#
# Entries to make C-Kermit for specific systems.
#
# Put the ones that need short makefiles first (read instructions for
# BSD 2.10 for hints...)

#Berkeley UNIX 2.10 and 2.11 on the DEC PDP-11.
#
#2.10BSD and 2.11BSD (the latter to a larger extent) are the same as 4.3BSD
#but without a large address space.  The BSD29 define should *not* be used.
#This entry doesn't work because "make" runs out of memory on the PDP-11,
#because of the size of this makefile.  A separate makefile, ckubs2.mak, is
#provided, which should be identical with this makefile, but with all the
#parts stripped away that are not relevant to this entry.  Use it like this:
#
#  mv ckubs2.mak makefile     -- or --     make -f ckubs2.mak bsd210
#  make bsd210
#
#If you don't have the ckubs2.mak file, edit this makefile to remove 
#everything below this entry.  Here's an easy way:
#
#  grep -n ^.BSD210END makefile    (notice the line number)
#  head -x makefile > ckubs2.mak   (use the same line number in place of x)
#  mv ckubs2.mak makefile
#  make bsd210
#
#NOTE: A string extraction method is used to put approx. 9kb of strings into
#a file. The module ckustr.c should be edited, if necessary, to change the
#pathname of the string file to where this file will reside (or just define
#the environment variable KSTR to be the pathname for the string file before
#running "make"), e.g. "setenv KSTR `pwd`kermit5.sr".  After make is finished,
#the file kermit5.sr should be moved to the where ckustr.c has been told to
#look for it.
#
bsd210:
	@echo Making C-Kermit $(CKVER) for 2.10BSD with overlays
	@echo -n "Be sure to install kermit5.sr with the same pathname"
	@echo " specified in ckustr.c"
	chmod +x ckustr.sed
	make ovwermit "CFLAGS= -O -DBSD43 -DLCKDIR -DNODEBUG -DNOTLOG \
	-DNOCSETS -DNOHELP -DNOSCRIPT -DNOSPL -DNOXMIT $(KFLAGS) \
	-Dgethostname=gethnam" "LNKFLAGS= -i " "CC= ./ckustr.sed " "CC2= cc"

#BSD210END

#Apple Mac II, A/UX
#Warning, if "send *" doesn't work, try the auxufs makefile entry below.
aux:
	@echo Making C-Kermit $(CKVER) for Macintosh A/UX...
	make wermit "CFLAGS = -DAUX -DDYNAMIC -DTCPSOCKET $(KFLAGS) -i -O" \
		"LNKFLAGS = -i"

#Apple Mac II, A/UX, but with ufs file volumes, uses <dirent.h>.
auxufs:
	@echo Making C-Kermit $(CKVER) for Macintosh A/UX...
	make wermit "CFLAGS = -DAUX -DDYNAMIC -DTCPSOCKET -DDIRENT $(KFLAGS) \
		-i -O" "LNKFLAGS = -i"

#Berkeley Unix 4.1
bsd41:
	@echo Making C-Kermit $(CKVER) for 4.1BSD...
	make wermit "CFLAGS= -DBSD41" "LIBS = -ljobs"

#Berkeley 4.2, 4.3, also Ultrix-32 1.x, 2.x, 3.x, many others
# Add -O, -DDYNAMIC, -s, etc, if they work.
bsd:
	@echo Making C-Kermit $(CKVER) for 4.2BSD...
	make wermit "CFLAGS= -DBSD4 -DTCPSOCKET $(KFLAGS)"

# 386bsd Berkeley 4.3
386bsd-full:
	@echo Making C-Kermit $(CKVER) for 386bsd 4.3BSD...
	make wermit "CFLAGS= -O -fstrength-reduce -DDYNAMIC -DBSD43 -DPOSIX \
	-DTCPSOCKET -DLOCK_DIR=\\\"/var/spool/lock\\\" $(KFLAGS)"

# 386bsd Berkeley 4.3, minimum size
386bsd-minimal:
	@echo Making C-Kermit $(CKVER) for 386bsd 4.3BSD...
	make wermit "CFLAGS= -O -fstrength-reduce -DDYNAMIC -DBSD43 -DPOSIX \
	-DNODIAL -DNOHELP -DNODEBUG -DNOTLOG -DNOSCRIPT -DNOCSETS -DNOICP \
	-DLOCK_DIR=\\\"/var/spool/lock\\\" $(KFLAGS)" "LNKFLAGS = -s"

#Berkeley 4.2, 4.3, minimum size
bsdm:
	@echo Making C-Kermit $(CKVER) for 4.2BSD...
	make wermit "CFLAGS= -O -DBSD4 -DDYNAMIC -DNODIAL -DNOHELP -DNODEBUG \
	-DNOTLOG -DNOSCRIPT -DNOCSETS -DNOICP $(KFLAGS)" "LNKFLAGS = -s"

#Berkeley Unix with HoneyDanBer UUCP
bsdhdb:
	@echo Making C-Kermit $(CKVER) for 4.2BSD with HDB UUCP...
	make wermit "CFLAGS= -DHDBUUCP -DBSD4 -DTCPSOCKET $(KFLAGS)"

#Berkeley Unix 4.3 with acucntrl program
bsd43:
	@echo Making C-Kermit $(CKVER) for 4.3BSD with acucntrl...
	make wermit "CFLAGS= -DBSD43 -DACUCNTRL -DTCPSOCKET $(KFLAGS) -O"

#Berkeley Unix 4.2 or 4.3 with lock directory /usr/spool/uucp/LCK/LCK..ttyxx,
#but without acucntrl program
bsdlck:
	@echo Making C-Kermit $(CKVER) for 4.2BSD, /usr/spool/uucp/LCK/...
	make wermit "CFLAGS= -DLCKDIR -DBSD4 -DTCPSOCKET $(KFLAGS)"

#Tektronix 6130, 4132, 43xx with UTek OS, /usr/spool/uucp/LCK./...
utek:
	@echo Making C-Kermit $(CKVER) for 4.2BSD/UTek
	make wermit "CFLAGS= -O -DLCKDIR -DBSD4 -DTCPSOCKET -DDYNAMIC \
	-DUTEK -DDCLPOPEN -DLOCK_DIR=\\\"/usr/spool/uucp/LCK.\\\" $(KFLAGS)"

#Mark Williams Coherent on IBM PC/AT family.
#There is a 64K limit on program size, so this is a command-line only version.
coherent:
	make "CFLAGS = -O -DCOHERENT -DDYNAMIC -DNOANSI -DNOICP -DNOSETKEY \
	-DNOCSETS -DNOHELP -DNODIAL -DNOSCRIPT -DNODEBUG -DNOTLOG -DNOXMIT \
	-DNOMSEND -DNOFRILLS -DNOSYSIOCTLH $(KFLAGS) -VSUVAR" wermit

#Mark Williams Coherent on IBM PC/AT family.
#This is an attempt to make a "minimum interactive" version - no scripts,
#no character sets, no help, no dial, no debug/transaction logging, no
#transmit, msend, mail, type, etc.
coherentmi:
	make "CFLAGS = -O -DCOHERENT -DDYNAMIC -DNOANSI -DNOSETKEY -DNOSHOW \
	-DNOCSETS -DNOHELP -DNODIAL -DNOSCRIPT -DNODEBUG -DNOTLOG -DNOXMIT \
	-DNOMSEND -DNOFRILLS -DNOSYSIOCTLH $(KFLAGS) -VSUVAR" wermit

#DEC Ultrix 2.x
# Add -O, -DDYNAMIC, -s, etc, if they work.
du2:
	@echo Making C-Kermit $(CKVER) for Ultrix 2.x ...
	make wermit "CFLAGS= -DBSD4 -DTCPSOCKET -DDU2 $(KFLAGS)"

#DEC Ultrix 3.1 (and probably also 3.0)
du3:
	@echo Making C-Kermit $(CKVER) for Ultrix 3.x...
	make wermit "CFLAGS= -DBSD4 -DTCPSOCKET -DDIRENT -DSIG_V \
	-DDYNAMIC $(KFLAGS) -O" "LNKFLAGS = -s"

#Name changed from ds4 to du4, should work on VAXes as well as DECstations.
ds4:
	make du4

#DEC Ultrix 4.0 or 4.1 on DECstation, VAXstation, VAX, etc.
du4:
	@echo Making C-Kermit $(CKVER) for Ultrix 4.0 or 4.1...
	make wermit "CFLAGS= -DBSD4 -DTCPSOCKET -DSIG_V -DDYNAMIC \
	$(KFLAGS) -Olimit 1000" "LNKFLAGS = -s"

#DEC Ultrix 4.2 on DECstation, VAXstation, VAX, etc.
#Like du4, except new C compiler supports -O2 optimization.
du42:
	@echo Making C-Kermit $(CKVER) for Ultrix 4.2...
	make wermit "CFLAGS= -DBSD4 -DTCPSOCKET -DSIG_V -DDYNAMIC \
	$(KFLAGS) -O2 -Olimit 1000" "LNKFLAGS = -s"

#DEC Ultrix 4.2 on DECstation, VAXstation, VAX, etc, System V R4 environment
du42s5r4:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4 on Ultrix...'
	make wermit "CFLAGS = -O2 -Olimit 1000 -DSVR4 -DDIRENT -DHDBUUCP \
	-DDYNAMIC -DTCPSOCKET $(KFLAGS)" "LNKFLAGS = -s"



#Sequent DYNIX/PTX 1.2.1
dynix12:
	@echo Making C-Kermit $(CKVER) for Sequent DYNIX/PTX 1.2.1...
	make wermit "CFLAGS= -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DPTX \
	-DPID_T=pid_t -DUID_T=uid_t -DGID_T=gid_t $(KFLAGS) -i -O" \
	"LNKFLAGS = -i"

#Sequent DYNIX/PTX 1.3
dynix13:
	@echo Making C-Kermit $(CKVER) for Sequent DYNIX/PTX 1.3 TCP/IP...
	make wermit "CFLAGS= -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DPTX  -O \
	-DPID_T=pid_t -DUID_T=uid_t -DGID_T=gid_t -DTCPSOCKET $(KFLAGS) -i" \
	"LNKFLAGS = -i" "LIBS = -lsocket -linet -lnsl"

#Sequent Dynix 3.0.x
dynix3:
	@echo Making C-Kermit $(CKVER) for Sequent Dynix 3.0.x...
	make wermit "CFLAGS= -DBSD43 -DACUCNTRL -DTCPSOCKET -O \
	-DUID_T=int -DGID_T=int $(KFLAGS)"

#Sequent Dynix 3.1.x
dynix31:
	@echo Making C-Kermit $(CKVER) for Sequent Dynix 3.1.x...
	make wermit "CFLAGS= -O -DDYNAMIC -DDCLPOPEN -DLCKDIR -DBSD4 \
	-DTCPSOCKET $(KFLAGS)"

#Encore, UMAX 4.3 (BSD) but without acucntrl program.
encore:
	make umax43

#Encore, UMAX 4.3 (BSD) but without acucntrl program.
umax43:
	@echo Making C-Kermit $(CKVER) for Encore UMAX 4.3...
	make PARALLEL=4 wermit \
	"CFLAGS= -DBSD43 -DENCORE -DDYNAMIC -DTCPSOCKET $(KFLAGS) -O"

#Encore, UMAX 4.2 (BSD)
umax42:
	@echo Making C-Kermit $(CKVER) for Encore UMAX 4.2...
	make PARALLEL=4 wermit \
	"CFLAGS= -DBSD4 -DENCORE -DDYNAMIC -DTCPSOCKET $(KFLAGS) -O"

#Encore 88K UMAX 5.3 with TCP/IP support
encore88K:
	@echo 'Making C-Kermit $(CKVER) for Encore 88K UMAX V, TCP/IP...'
	make wermit "CFLAGS = -q ext=pcc -DSVR3 -DTCPSOCKET -DDIRENT \
	-DHDBUUCP -DDYNAMIC $(KFLAGS) -O" "LNKFLAGS ="

#Berkeley Unix 2.8, 2.9 for PDP-11s with I&D space, maybe also Ultrix-11???
#C-Kermit(5A) is simply too large (even turning off almost every feature
#available) to run without both I&D space plus overlays.  The old comment
#suggested running 'pcc' but that won't help.  Changing 'cc' to 'ckustr.sed'
#will cause a string extraction to be done, saving D space by moving strings
#to a file.
bsd29:
	@echo Making C-Kermit $(CKVER) for 2.8 or 2.9BSD.
	@echo Read the makefile if you have trouble with this...
	make ovwermit "CFLAGS= -DBSD29 -DNODEBUG -DNOTLOG -DNOCSETS -DNOHELP \
	-DNOSCRIPT -DNOSPL -DNOXMIT -DNODIAL $(KFLAGS)" \
	"LNKFLAGS= -i -lndir" "CC= cc " "CC2= cc"

#Convex C1 with Berkeley Unix
convex:
	@echo Making C-Kermit $(CKVER) for Convex C1 / BSD...
	make wermit "CFLAGS= -DBSD4 $(KFLAGS) -Dmsleep=mnap"

#Convex C210 with Convex/OS 8
convex8:
	@echo Making C-Kermit $(CKVER) for Convex C210 with OS 8
	make wermit \
	"CFLAGS= -DBSD4 -DTCPSOCKET -DNODEBUG -DDIRENT -DNOFILEH \
	-DDYNAMIC $(KFLAGS) -DSIG_V -Dmsleep=mnap"

#Convex C2 with Convex OS 9.1 (should also work with 8.1 or later)
#with ANSI C compiler, uses BSD 4.3 uucp lockfile convention.
convex9:
	@echo Making C-Kermit $(CKVER) for Convex C210 with OS 9.1
	make wermit \
	"CFLAGS= -DPOSIX -DDIRENT -DNOFILEH -DTCPSOCKET -DDYNAMIC \
	-D__STDC__ -DLCKDIR -Dmsleep=mnap -O -ext -tm c1" $(KFLAGS) \
	"LNKFLAGS = -ext"

#Cray X-MP or Y-MP UNICOS System V R3
#Maybe the -i link option should be removed.
cray:
	@echo 'Making C-Kermit $(CKVER) for Cray X/Y-MP UNICOS System V R3...'
	make wermit NPROC=1 "CFLAGS= -DSVR3 -DDIRENT -DHDBUUCP $(KFLAGS) \
	-i -O" "LNKFLAGS = -i"

#NeXT
#Uses shared library to make executable program about 80K smaller.
#Remove "LIBS = -lsys_s" if this causes trouble.
next:
	@echo Making C-Kermit $(CKVER) for NeXT...
	make wermit \
	"CFLAGS= -DNEXT -DNOSETBUF -DTCPSOCKET -DDYNAMIC -DLCKDIR \
	$(KFLAGS) -O -w" "LIBS = -lsys_s"

#NeXT version built with C-Kermit malloc debugger.
nextmd:
	@echo Making C-Kermit $(CKVER) for NeXT with malloc debug...
	make mermit \
	"CFLAGS= -DNEXT -DTCPSOCKET -DDYNAMIC -DLCKDIR -DNOSETBUF -O -w \
	-Dmalloc=dmalloc -Dfree=dfree -DMDEBUG" "LIBS = -lsys_s"

#nextg - Gets useful error messages out of GNU cc.
# can't use -ansi here for some reason it clobbers definition of SIGQUIT (?)
nextg:
	@echo Making C-Kermit $(CKVER) for NeXT...
	make wermit "CFLAGS= -DNEXT -DTCPSOCKET -DDYNAMIC -DLCKDIR -DKANJI \
	-DNOSETBUF $(KFLAGS) -Wall -O" \
	"LIBS = -lsys_s"

#NeXT minimum interactive
nextmi:
	@echo Minimum interactive
	make nextg "KFLAGS=-DNOSPL -DNOXMIT -DNOMSEND -DNOFRILLS \
	-DNODIAL -DNOHELP -DNODEBUG -DNOTLOG -DNOSCRIPT -DNOCSETS \
	-DNOSETKEY -DNOSETBUF -UTCPSOCKET" "LNKFLAGS = -s"

#POSIX
posix:
	@echo 'Making C-Kermit $(CKVER) for POSIX, no UUCP lockfile support...'
	make wermit "CFLAGS= -DPOSIX -DNOUUCP $(KFLAGS) -O"

#Pyramid 9810 USx 4.4b-881028
pyramid:
	@echo Making C-Kermit $(CKVER) for Pyramid OSx 4.4b...
	make wermit "CFLAGS= -DBSD4 -DTCPSOCKET -DDYNAMIC -O $(KFLAGS)" \
	"LNKFLAGS = -s"

#SONY NEWS, NEWS-OS 4.01C
sonynews:
	@echo Making C-Kermit $(CKVER) for SONY NEWS-OS 4.01C...
	make wermit "CFLAGS= -DBSD43 -DKANJI -DACUCNTRL \
	-DTCPSOCKET -O"

#SUNPOSIX
sunposix:
	@echo Making C-Kermit $(CKVER) for POSIX...
	make wermit "CC= /usr/5bin/cc " \
	"CFLAGS= -DPOSIX -DHDBUUCP -DVOID=void -O"

#IBM's AIX 3.0 on IBM 370 mainframe, tested on AIX F44 thru F50.
aix370:
	@echo Making C-Kermit $(CKVER) for IBM System/370 AIX 3.0...
	make wermit "CFLAGS= -DAIX370 -DTCPSOCKET -DLCKDIR -DDIRENT -DYNAMIC \
	$(KFLAGS)" "LIBS = -lbsd"

#IBM's AIX 3.0 on IBM PS/2, tested on AIX F44 thru F50.
#This is exactly the same as AIX370 except for the version herald.
#(Note, apparently AIX 3.0 for PS/2's was never formally released).
ps2aix3:
	@echo Making C-Kermit $(CKVER) for IBM PS/2 AIX 3.0...
	make wermit "CFLAGS= -DAIXPS2 -DTCPSOCKET -DLCKDIR -DDIRENT -DYNAMIC \
	$(KFLAGS)" "LIBS = -lbsd"

#IBM AIX 3.0 for RISC System/6000.
rs6000:
	@echo Making C-Kermit $(CKVER) for IBM AIX 3.0 or 3.1, RS/6000...
	make wermit \
	"CFLAGS= -DAIXRS -DTCPSOCKET -DSVR3 -DDIRENT -DDYNAMIC -DCK_ANSIC -O \
	$(KFLAGS)" "LNKFLAGS = -s"

#SUN OS version 4.0, BSD environment, has saved original euid feature.
sunos4:
	@echo Making C-Kermit $(CKVER) for SUNOS 4.0, BSD environment...
	make wermit "CFLAGS= -O -DSUNOS4 -DDIRENT -DTCPSOCKET -DSAVEDUID \
	-DDYNAMIC $(KFLAGS)"

#As above, but with SunLink X.25 support
sunos4x25:
	@echo SunLink X.25 support
	make sunos4 "KFLAGS=$(KFLAGS) -DSUNX25"

#SUN OS version 4.1, BSD environment, has saved original euid feature.
#Uses Honey DanBer UUCP.  Requires presence of /usr/spool/locks directory.
# /var/spool/ should be a symbolic link to  /usr/spool/.
sunos41:
	@echo Making C-Kermit $(CKVER) for SUNOS 4.1 / BSD...
	make wermit \
	"CFLAGS= -O -DSUNOS41 -DHDBUUCP -DDIRENT -DTCPSOCKET -DNOSETBUF \
	-DSAVEDUID -DDYNAMIC $(KFLAGS)"

#SUNOS 4.1 with SunLink X.25 support
sunos41x25:
	@echo SunLink X.25 support
	make sunos41 "KFLAGS=$(KFLAGS) -DSUNX25"

#SUNOS 4.1 with Kanji support.
sunos41k:
	@echo Kanji support
	make sunos41 "KFLAGS=$(KFLAGS) -DKANJI"

#As above, but without Cyrillic support.
sunos41knc:
	@echo Kanji, no Cyrillic
	make sunos41 "KFLAGS=$(KFLAGS) -DKANJI -DNOCYRIL"

#SUN OS version 4.1, BSD environment, no character set translation.
# Otherwise just like sunos41.
sunos41ncs:
	@echo No character sets
	make sunos41 "KFLAGS=$(KFLAGS) -DNOCSETS"

#SUN OS version 4.1, BSD environment, no DIAL command.
# Otherwise just like sunos41.
sunos41xd:
	@echo No DIAL command
	make sunos41 "KFLAGS=$(KFLAGS) -DNODIAL"

#SUN OS version 4.0, BSD environment, minimum size...
sunos4m:
	@echo Minimum size
	make sunos4 "KFLAGS=$(KFLAGS) -DNODIAL -DNOHELP -DNODEBUG -DNOTLOG \
	-DNOSCRIPT -DNOCSETS -DNOICP -DNOMSEND" "LNKFLAGS = -s"

#SUN OS version 4.1, BSD environment, minimum size...
sunos41m:
	@echo Minimum size
	make sunos41 "KFLAGS=$(KFLAGS) -DNODIAL -DNOHELP -DNODEBUG -DNOTLOG \
	-DNOSCRIPT -DNOCSETS -DNOICP -DNOMSEND -UTCPSOCKET" "LNKFLAGS = -s"

#SUN OS version 4.0, BSD environment, minimum size w/command parser.
sunos4mi:
	@echo Minimum interactive
	make sunos4 "KFLAGS=-DNOSPL -DNOXMIT -DNOMSEND -DNOFRILLS \
	-DNODIAL -DNOHELP -DNODEBUG -DNOTLOG -DNOSCRIPT -DNOCSETS \
	-DNOSETKEY -DNOSHOW -UTCPSOCKET $(KFLAGS)" "LNKFLAGS = -s"

#SUN OS version 4.1 or later, BSD environment, minimum size w/command parser.
sunos41mi:
	@echo Minimum interactive
	make sunos41 "KFLAGS=-DNOSPL -DNOXMIT -DNOMSEND -DNOFRILLS \
	-DNODIAL -DNOHELP -DNODEBUG -DNOTLOG -DNOSCRIPT -DNOCSETS \
	-DNOSHOW -DNOSETKEY -UTCPSOCKET $(KFLAGS)" "LNKFLAGS = -s"

#SUN OS version 4.1, BSD environment, no network support.
sunos41nn:
	@echo Making C-Kermit $(CKVER) for SUNOS 4.1, BSD, no networks...
	make sunos41 "KFLAGS=$(KFLAGS) -UTCPSOCKET"

#SUN OS version 4.1 or later, BSD, no debugging log.
sunos41nd:
	@echo No debugging...
	make sunos41 "KFLAGS=$(KFLAGS) -DNODEBUG"

#SUN OS version 4.1 or later, BSD, no transaction log.
sunos41nt:
	@echo No transaction log...
	make sunos41 "KFLAGS=$(KFLAGS) -DNOTLOG"

#SUN OS version 4.1 or later, BSD, no frills.
sunos41nf:
	@echo No frills...
	make sunos41 "KFLAGS=$(KFLAGS) -DNOFRILLS"

#SUN OS version 4.1 or later, BSD, no command-line options.
sunos41nl:
	@echo No command-line options...
	make sunos41 "KFLAGS=$(KFLAGS) -DNOCMDL"

#SUN OS version 4.1 or later, BSD, no job control.
sunos41nj:
	@echo No job control...
	make sunos41 "KFLAGS=$(KFLAGS) -DNOJC"

#SUNOS 4.1 with malloc debugger
sunos41md:
	@echo Making C-Kermit $(CKVER) for SUNOS 4.1 malloc debug...
	make mermit \
	"CFLAGS= -O -DSUNOS41 -DHDBUUCP -DDIRENT -DTCPSOCKET \
	-DSAVEDUID -DDYNAMIC $(KFLAGS) -Dmalloc=dmalloc -Dfree=dfree -DMDEBUG"

#SUN OS version 4.0, System V R3 environment (-i option omitted).
sunos4s5:
	@echo Making C-Kermit $(CKVER) for SUNOS 4.0, System V R3...
	@echo Ignore harmless complaints about redefinition of symbols...
	make wermit "CC= /usr/5bin/cc " \
	"CFLAGS = -DDIRENT -DSUN4S5 -DYNAMIC $(KFLAGS) -O"

#SUN OS version 4.1 or later, System V R3 environment (-i option omitted).
#Like sunos4s5, but SUNOS 4.1 has switched to HDB UUCP lockfile conventions.
sunos41s5:
	@echo Making C-Kermit $(CKVER) for SUNOS 4.1 System V R3...
	@echo Ignore harmless complaints about redefinition of symbols...
	make wermit "CC= /usr/5bin/cc " \
	"CFLAGS = -DSUN4S5 -DDIRENT -DHDBUUCP -DDYNAMIC $(KFLAGS) -O"

#SUN OS version 4.1 or later, gcc, profiling with gprof, no debugging.
#To get profile, "make sunos4p" (on SUN), then "./wermit".  After running
#wermit, "gprof ./wermit | lpr" or whatever to get execution profile.
sunos41p:
	@echo Making C-Kermit $(CKVER) for SUNOS 4.x with profiling...
	make wermit "CC= gcc " \
	"CFLAGS= -DSUNOS41 -DNODEBUG -DSAVEDUID -DDIRENT -DTCPSOCKET \
	-DDYNAMIC $(KFLAGS) -pg" "LNKFLAGS = -pg"

#Apollo with Domain SR10.0 or later, BSD environment
#Reportedly, it might also help to add '-A,systype=bsd4.3' to CFLAGS.
#Reportedly, there is also a problem with getc & putc macros that can
#be handled by using '#ifdef apollo' somewhere to redefine them???
#On the other hand, other reports indicate that it works fine as-is.
#NOTE: This entry was previously like this:
#	make wermit "CFLAGS= -DNOFILEH -DBSD4 $(KFLAGS) -Uaegis \
#	-DTCPSOCKET -U__STDC__"
#Recent reports (Dec 91) indicate SR10 has an ANSI-compliant C compiler,
#in addition to an older one that claimed to be ANSI-compliant but wasn't.
#The following make entry (plus checks that are made in ckcdeb.h) detect
#which compiler is used and define the CK_ANSIC or NOANSI flags accordingly.
sr10-bsd:
	@echo Making C-Kermit $(CKVER) for Apollo SR10.0 / BSD ...
	make wermit "CFLAGS= -O -DBSD43 -DTCPSOCKET -Uaegis $(KFLAGS)"

#Apollo with Domain SR10.0 or later, System V R3 environment.
#Don't use the optimizer (-O), it causes problems at runtime.
sr10-s5r3:
	@echo Making C-Kermit $(CKVER) for Apollo SR10.0 / Sys V R3 ...
	make wermit "CFLAGS= -DNOFILEH -DSVR3 $(KFLAGS) -Uaegis -U__STDC__"

#Apollo Domain/IX (untested, try this if sr10-bsd doesn't work)
# (Can we add -DTCPSOCKET here?)
apollobsd:
	@echo Making C-Kermit $(CKVER) for Apollo Domain/IX...
	make wermit "CC= /bin/cc " "CC2= /bin/cc " \
	"CFLAGS= -DNOFILEH -DBSD4 $(KFLAGS) -Uaegis"

#Version 7 Unix (see comments above)
v7:
	@echo Making C-Kermit $(CKVER) for UNIX Version 7.
	@echo Read the makefile if you have trouble with this...
	make wermit "CFLAGS=-DV7 -DPROCNAME=\\\"$(PROC)\\\" \
	-DBOOTNAME=\\\"$(BOOTFILE)\\\" -DNPROCNAME=\\\"$(NPROC)\\\" \
	-DNPTYPE=$(NPTYPE) $(DIRECT) -DO_RDWR=2 -DO_NDELAY=0 -DO_SCCS_ID \
	$(KFLAGS)"

#AT&T UNIX System V R3, signal() is void rather than int.
#Uses dirent.h and Honey DanBer uucp.
#Add the -i link option if necessary.
sys5r3:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R3...'
	make wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC $(KFLAGS) \
	-O" "LNKFLAGS ="

#AT&T UNIX System V R3, for 3B computers with Wollongong TCP/IP.
sys5r3net3b:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX SVR3/3B/Wollongong...'
	make wermit \
	"CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DWOLLONGONG $(KFLAGS) \
	-O" "LIBS= -lnet -lnsl_s" "LNKFLAGS ="

#AT&T UNIX System V R3, signal() is void rather than int.
#Uses dirent.h and Honey DanBer uucp, has <termiox.h>.
#Has <termiox.h> for RTS/CTS flow control.
sys5r3tx:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R3...'
	make wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DTERMIOX $(KFLAGS) -i -O" "LNKFLAGS ="

#AT&T UNIX System V R3, signal() is void rather than int.
#Uses dirent.h and Honey DanBer uucp, has <termiox.h>.
#Has <sys/termiox.h> for RTS/CTS flow control.
sys5r3sx:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R3...'
	make wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DSTERMIOX $(KFLAGS) -i -O" "LNKFLAGS ="

#AT&T UNIX System V R4.
#Has <termiox.h>.
sys5r4:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4...'
	make wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DTERMIOX $(KFLAGS)" "LNKFLAGS = -s"

#AT&T UNIX System V R4 with Wollongong TCP/IP.
#Has <termiox.h>.
sys5r4net:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4...'
	make wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DTERMIOX -DWOLLONGONG $(KFLAGS)" "LNKFLAGS = -s"

#AT&T UNIX System V R4.
#Has <sys/termiox.h>.
sys5r4sx:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4...'
	make wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DSTERMIOX $(KFLAGS)" "LNKFLAGS = -s"

#AT&T UNIX System V R4.
#Has <sys/termiox.h>, regular Berkeley sockets library, i.e. in.h and inet.h
#are not misplaced in sys (rather than netinet and arpa, respectively).
#Uses ANSI C constructs, <sys/termiox.h>, etc etc. 
sys5r4sxtcp:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4...'
	make wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DSTERMIOX -DTCPSOCKET $(KFLAGS)" \
	"LIBS= -lsocket -lnsl" "LNKFLAGS = -s"

#Smallest possible version of above
s5r4m:
	@echo Minimum size
	make sys5r4sx "KFLAGS=$(KFLAGS) -DNODIAL -DNOHELP -DNODEBUG -DNOTLOG \
	-DNOSCRIPT -DNOCSETS -DNOICP -DNOMSEND -UTCPSOCKET" "LNKFLAGS = -s"

#Smallest possible interactive version of above
s5r4mi:
	@echo Minimum interactive
	make sys5r4sx "KFLAGS=-DNOSPL -DNOXMIT -DNOMSEND -DNOFRILLS -DNOSHOW \
	-DNODIAL -DNOHELP -DNODEBUG -DNOTLOG -DNOSCRIPT -DNOCSETS -DNOSETKEY \
	-UTCPSOCKET $(KFLAGS)" "LNKFLAGS = -s"

#AT&T UNIX System V R4, has <sys/termiox.h>
#ANSI C function prototyping disabled.
sys5r4sxna:
	@echo No ANSI C prototyping...
	make sys5r4sx "KFLAGS=$(KFLAGS) -DNOANSI"

#Commodore Amiga with AT&T UNIX System V R4 and TCP/IP support.
#Has <sys/termiox.h>.
svr4amiganet:
	@echo 'Making C-Kermit $(CKVER) for Amiga SVR4 + TCP/IP...'
	make wermit "CC=gcc" "CC2=gcc" \
	"CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC -DSTERMIOX \
	-DTCPSOCKET $(KFLAGS)" "LNKFLAGS = -s" "LIBS = -lsocket -lnsl"

#ESIX SVR4.0.3 with TCP/IP support.
#Has <sys/termiox.h>, ANSI C function prototyping disabled.
esixr4:
	@echo 'Making C-Kermit $(CKVER) for ESIX SVR4 + TCP/IP...'
	make wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC -DNOANSI \
	-DSTERMIOX -DTCPSOCKET $(KFLAGS)" "LNKFLAGS = -s" \
	"LIBS = -lsocket -lnsl"

#AT&T UNIX System V R4.
#Has <sys/termiox.h>, Wollongong TCP/IP.
sys5r4sxnet:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4...'
	make wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DSTERMIOX -DWOLLONGONG $(KFLAGS)" "LNKFLAGS = -s"

#AT&T UNIX System V R4, no <termio.x> or <sys/termio.x>.
sys5r4nx:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4...'
	make wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	$(KFLAGS)" "LNKFLAGS = -s"

#AT&T UNIX System V R4, no <termio.x> or <sys/termio.x>, has Wollongong TCP/IP.
sys5r4nxnet:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4...'
	make wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DWOLLONGONG $(KFLAGS)" "LNKFLAGS = -s"

#Data General UX 5.40 (= System V R4) for DG Aviion, with TCP/IP support.
dgux540:
	@echo 'Making C-Kermit $(CKVER) for DG Aviion UX 5.40...'
	make wermit "CFLAGS = -O -DDGUX540 -DDIRENT -DHDBUUCP \
	-DDYNAMIC -DSTERMIOX -DTCPSOCKET $(KFLAGS)" "LNKFLAGS = -s"

#Silicon Graphics System V R3 with BSD file system (IRIS)
iris:
	@echo Making C-Kermit $(CKVER) for Silicon Graphics IRIX pre-3.3...
	make wermit "CFLAGS = -O -DSVR3 -DLONGFN  $(KFLAGS) \
	-I/usr/include/bsd" "LIBS = -lbsd"

#Silicon Graphics IRIS System V R3
irix33:
	@echo 'Making C-Kermit $(CKVER) for Silicon Graphics IRIX 3.3...'
	make wermit \
	"CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC $(KFLAGS) -O" \
	"LNKFLAGS ="

#Silicon Graphics Iris Indigo with IRIX 4.0.0
#Strict ANSI C compilation, TCP/IP support included
irix40:
	@echo 'Making C-Kermit $(CKVER) for Silicon Graphics IRIX 4.0...'
	make wermit \
	"CFLAGS = -DIRIX40 -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DPWID_T=uid_t \
	-DCK_ANSIC -DTCPSOCKET $(KFLAGS) -O -I/usr/include/bsd"

#In case they type "make sys5"...
sys5:
	make sys3

#Generic ATT System III or System V (with I&D space)
sys3:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System III'
	@echo 'or System V R2 or earlier...'
	make wermit "CFLAGS = -DATTSV $(KFLAGS) -i -O" "LNKFLAGS = -i"

#Generic ATT System III or System V R2 or earlier, "no void":
#special entry to remove "Illegal pointer combination" warnings.
sys3nv:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System III'
	@echo 'or System V R2 or earlier...'
	make wermit "CFLAGS = -DATTSV -Dvoid=int  $(KFLAGS) -i -O" \
	"LNKFLAGS = -i"

#Generic ATT System III or System V (no I&D space)
sys3nid:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System III'
	@echo 'or System V R2 or earlier, no I&D space...'
	make wermit "CFLAGS = -DATTSV $(KFLAGS) -O" "LNKFLAGS ="

#AT&T 7300/Unix PC systems, sys3 but special handling for internal modem.
#Link with the shared library -- the conflict with openi in shared library
#is solved with -Dopeni=xopeni
sys3upc:
	@echo 'Making C-Kermit $(CKVER) for AT&T 7300 UNIX PC, shared lib...'
	@echo 'If shared lib causes trouble, use make sys3upcold.'
	make wermit "CFLAGS = -O -DATT7300 -DDYNAMIC -DNOSETBUF $(KFLAGS) \
	-Dopeni=xopeni" \
	"CC2 = ld /lib/crt0s.o /lib/shlib.ifile" "LNKFLAGS = -s"

#AT&T 7300/Unix PC systems, as above, but use gcc.
sys3upcg:
	@echo 'Making C-Kermit $(CKVER) for AT&T 7300 UNIX PC...'
	make wermit "CFLAGS = -O -DATT7300 -DDYNAMIC -DNOSETBUF $(KFLAGS) \
	-Dopeni=xopeni" \
	"CC = gcc" "CC2 = gcc" "LNKFLAGS = -s -shlib"

#AT&T 7300/Unix PC systems, sys3 but special handling for internal modem.
sys3upcold:
	@echo 'Making C-Kermit $(CKVER) for AT&T 7300 UNIX PC...'
	make wermit "CFLAGS = -DATT7300 $(KFLAGS) -O" "LNKFLAGS = -i"

#AT&T 6300 PLUS (warning, -O makes it run out of space).
#NOTE: Remove -DHDBUUCP if not using Honey DanBer UUCP.
att6300:
	@echo 'Making C-Kermit $(CKVER) for AT&T 6300 PLUS...'
	make wermit "CFLAGS = -DATT6300 -DHDBUUCP $(KFLAGS) -O -Ml -i" \
	"LNKFLAGS = -i -Ml"

#AT&T 6300 PLUS with no debugging (about 34K smaller)
# -Optimization saves about 20K too.
att6300nd:
	@echo 'Making C-Kermit $(CKVER) for AT&T 6300 PLUS, no debugging...'
	make wermit \
	"CFLAGS = -DATT6300 -DHDBUUCP -DNODEBUG $(KFLAGS) -O -i -Ml" \
		"LNKFLAGS = -i -Ml"

#AT&T 3B2, 3B20-series computers running AT&T UNIX System V.
#This is just generic System V with Honey DanBer UUCP, so refer to sys3hdb.
att3bx:
	make sys3hdb

#Any System V R2 or earlier with Honey DanBer UUCP (same as above)
sys3hdb:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System III'
	@echo 'or System V R2 or earlier with Honey DanBer UUCP...'
	make wermit "CFLAGS = -DATTSV -DHDBUUCP $(KFLAGS) -i -O" \
	"LNKFLAGS = -i"

#In case they say "make sys5hdb" instead of "make sys3hdb"...
sys5hdb:
	make sys3hdb

#IBM PS/2 with AIX 1.0 (currently in field test as F10A)
#  Reports indicate that -O switch must be omitted.
#  It is also possible that "made bsd" will work (reports welcome).
#  One report recommended "make LIBS=-lbsd bsd" did the trick.
ps2aix:
	@echo 'Making C-Kermit $(CKVER) for IBM AIX 1.0 PS/2...'
	make wermit "CFLAGS = -DATTSV -DPS2AIX10 $(KFLAGS) -i" "LNKFLAGS = -i"

#IBM RT PC with AIX 2.2.1
#This one has unique and strange lockfiles.
rtaix:
	@echo 'Making C-Kermit $(CKVER) for IBM RT PC, AIX 2.2.1...'
	make wermit \
	"CFLAGS = -DATTSV -DRTAIX -DHDBUUCP -DTCPSOCKET $(KFLAGS) -O -w" \
	"LNKFLAGS = -s"

#IBM RT PC with AIX 2.2.1 (BSD 4.3)
# Add -O, -DDYNAMIC, -s, etc, if they work.
rtacis:
	@echo Making C-Kermit $(CKVER) for RT PC with ACIS 2.2.1 = BSD 4.3...
	make wermit "CFLAGS= -DBSD4 -DTCPSOCKET $(KFLAGS) -U__STDC__" \
	"LNKFLAGS = -s"

#HP 9000 series 300, 500, 800, no long filenames and no job control.
#This is certainly only good for HPUX versions earlier than 6.5.
hpuxpre65:
	@echo 'Making C-Kermit $(CKVER) for HP-9000 HP-UX, no long filenames.'
	make wermit "CFLAGS = -DHPUX -DHPUXPRE65 $(KFLAGS) -O" "LNKFLAGS ="

#HP 9000 series 300, 500, 800, no long filenames.
#This is probably only good for HPUX versions earlier than 6.2.
hpux:
	@echo 'Making C-Kermit $(CKVER) for HP-9000 HP-UX, no long filenames.'
	make wermit "CFLAGS = -DHPUX $(KFLAGS) -O" "LNKFLAGS ="

#HP-UX 7.0, no long filenames, no network support.
hpux7sf:
	@echo 'Making C-Kermit $(CKVER) for HP-9000 HP-UX, no long filenames.'
	make wermit "CFLAGS = -DHPUX $(KFLAGS) -DSIG_V -O" "LNKFLAGS ="

#HP 9000 series 300, 500, 800, long filenames (using BSD file system)
# (This one is probably necessary for the Series 300)
hpuxlf:
	@echo 'Making C-Kermit $(CKVER) for HP-9000 HP-UX, long filenames...'
	make wermit "CFLAGS = -DHPUX -DNDIR -DLONGFN -DYNAMIC $(KFLAGS) -O" \
	"LNKFLAGS ="

#HP 9000 series 300, 500, 800, long filenames (using <dirent.h>)
hpuxde:
	@echo 'Making C-Kermit $(CKVER) for HP-9000 HP-UX, long filenames...'
	make wermit "CFLAGS = -DHPUX -DDIRENT -DDYNAMIC $(KFLAGS) -O" \
	"LNKFLAGS ="

#HP 9000 series 300, 500, 800, long filenames, System V R3 or later
# (Does anybody know what is the earliest release of HP-UX based on SVR3?)
hpuxs5r3:
	@echo 'Making C-Kermit $(CKVER) for HP-9000 HP-UX, long filenames...'
	make wermit \
	"CFLAGS = -DHPUX -DSVR3 -DDIRENT -DTCPSOCKET -DDYNAMIC \
	-DNOSETBUF $(KFLAGS) -O" "LNKFLAGS =" "LIBS=-lBSD"

#HP 9000 series 800 HP-UX 7.0, long filenames, network support, HDB uucp
#there must be <arpa/telnet.h> & <arpa/inet.h> present to support this
#configuration.  To use this, you must have bought the ARPA Services
#Product from HP, and you must get the files "telnet.h" and "inet.h"
#from the Berkeley Standard Distribution because (reportedly) they are not
#part of the HP product.
hpux70lfn:
	@echo 'Making C-Kermit $(CKVER) for HP9000/8xx HP-UX V. 7.0'
	@echo 'supporting: long filenames, networking, HDB uucp...'
	make wermit "CFLAGS = -DHPUXDEBUG -DHPUX -DSVR3 -DDIRENT -DLONGFN \
	-DHDBUUCP -DDYNAMIC -DLOCK_DIR=\\\"/usr/spool/uucp\\\" \
	-DNOSETBUF -DTCPSOCKET  $(KFLAGS) -O" "LNKFLAGS = -s" "LIBS = -lBSD"

#HP 9000 HP-UX 8.0
hpux80:
	make hpux70lfn

#Regulus on CIE Systems 680/20
cie:
	@echo 'Making C-Kermit $(CKVER) for CIE Systems 680/20 Regulus...'
	make wermit "CFLAGS = -DATTSV -DNOFILEH -DCIE $(KFLAGS) -O" \
	"LNKFLAGS ="

#Microport SV/AT for IBM PC/AT 286 and clones, System V R2.
#The -O flag may fail on some modules (like ckuus2.c), in which case you 
#should compile them by hand, omitting the -O.  If you get "hash table
#overflow", try adding -DNODEBUG.
#Also, reportedly this compiles better with gcc than with cc.
mpsysv:
	@echo 'Making C-Kermit $(CKVER) for Microport SV/AT 286...'
	make wermit "CFLAGS= -DATTSV $(KFLAGS) -O -Ml" "LNKFLAGS = -Ml"

#Microsoft "Xenix/286" e.g. for IBM PC/AT
xenix:
	@echo 'Making C-Kermit $(CKVER) for Xenix/286'
	make wermit "CFLAGS= -DXENIX -DNOFILEH $(KFLAGS) -Dunix -F 3000 -i" \
	"LNKFLAGS = -F 3000 -i"

#SCO Xenix 2.2.1 for IBM PC, XT, PS2/30, or other 8088 or 8086 machine
#If this doesn't work, try some of the tricks from sco286.
sco86:
	@echo 'Making C-Kermit $(CKVER) for SCO Xenix/86...'
	make wermit \
	"CFLAGS= -DXENIX -DNOFILEH $(KFLAGS) -Dunix -F 3000 -i -M0me" \
	"LNKFLAGS = -F 3000 -i -M0me" "LIBS = -lx"

#SCO Xenix/286 2.2.1, e.g. for IBM PC/AT, PS/2 Model 50, etc.
#Reportedly, this "make" can fail simply because of the size of this
#makefile.  If that happens, use "makeL", or edit out some of the
#other entries.
sco286:
	@echo 'Making C-Kermit $(CKVER) for SCO Xenix/286...'
	@echo 'If make fails, try using makeL.'
	make wermit \
	"CFLAGS= -s -O -LARGE -DXENIX -DNOFILEH -Dunix -DRDCHK -DNAP \
	$(KFLAGS) -F 3000 -i -M2let32" \
	"LIBS = -lx" "LNKFLAGS = -s -O -LARGE -F 3000 -i -M2let32"

#SCO Xenix/286 2.2.1, e.g. for IBM PC/AT, PS/2 Model 50, etc.
#As above, but with HDBUUCP
sco286hdb:
	@echo 'Making C-Kermit $(CKVER) for SCO Xenix/286 with HDB UUCP...'
	@echo 'If make fails, try using makeL.'
	make wermit \
	"CFLAGS= -s -O -LARGE -DXENIX -DNOFILEH -Dunix -DRDCHK -DNAP \
	-DHDBUUCP $(KFLAGS) -F 3000 -i -M2let32" \
	"LIBS = -lx" "LNKFLAGS = -s -O -LARGE -F 3000 -i -M2let32"

#SCO Xenix/386 2.2.2
sco386:
	@echo 'Making C-Kermit $(CKVER) for SCO Xenix/386...'
	make wermit \
	"CFLAGS= -DXENIX -DNOFILEH -Dunix -DRDCHK -DNAP $(KFLAGS) -Otcl -M3e" \
	"LIBS = -lx"

#SCO XENIX/386 2.3.3 with gcc 1.37 or later...
sco386gcc:
	@echo 'Making C-Kermit $(CKVER) for SCO Xenix/386 2.3.3, gcc...'
	make wermit "CC = gcc" "CC2 = gcc" \
	"CFLAGS= -O -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP \
	-DNOJC -DNODEBUG -DDYNAMIC $(KFLAGS) \
	-traditional -fpcc-struct-return -fstrength-reduce \
	-DM_BITFIELDS -DM_COFF -DM_I386 -DM_I86 -DM_I86SM \
	-DM_INTERNAT -DM_SDATA -DM_STEXT -DM_SYS3 -DM_SYS5 \
	-DM_SYSIII -DM_SYSV -DM_WORDSWAP -DM_XENIX \
	-DPWID_T=int " "LNKFLAGS = " "LIBS = -lx"

#SCO UNIX/386 3.2.0, 3.2.1, and SCO Xenix 2.3.x
sco3r2:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	make wermit \
	"CFLAGS= -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP \
	-DNOJC $(KFLAGS) -Otcl" "LIBS = -lc -lx"

# Exactly the same as above, but enables some special SCO-specific code
# that allegedly clears up some problems with HANGUP and with uugetty.
# For satisfactory operation on bidrectional lines that are handled by
# uugetty, you must install the kermit program with owner=group=uucp
# and permission 06755.
sco3r2x:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	make wermit \
	"CFLAGS= -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP -DNOJC \
	-DSCO32 $(KFLAGS) -Otcl" "LIBS = -lc -lx"

#SCO UNIX/386 3.2.0 and SCO Xenix 2.3.x with Excelan TCP/IP support
sco3r2net:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 / Excelan...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	make wermit \
	"CFLAGS= -I/usr/include/exos -DXENIX -DNOFILEH -DHDBUUCP -DRDCHK \
	-DSVR3 -DNAP -DTCPSOCKET -DEXCELAN -DNOJC $(KFLAGS) -Otcl" \
	"LIBS = -lc -lx -lsocket"

#SCO Xenix 2.3.x with Racal InterLan TCP/IP support
# Extra compile flags for other version of Racal InterLan TCP/IP:
# Xenix286/NP621-286, use -Ml -DPARAMH -DINTERLAN -Di286 -DSYSV
# Xenix386/NP621-386, use -DPARAMH -DINTERLAN -Di386 -DSYSV
# ISC386ix/NP622I, use -DSYSV -Di386
# SCO Unix3.2/NP622S, use -DSYSV -Di386 -DSCO_UNIX
# AT&T SVR3.2/NP622A, use -DSYSV -Di386 -DATT
sco3r2netri:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 / Racal InterLan...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following four continued lines into one line.'
	make wermit \
	"CFLAGS= -I/usr/include/interlan -DXENIX -DNOFILEH -DHDBUUCP \
	-DSVR3 -DRDCHK -DNAP -DTCPSOCKET -DPARAMH -DINTERLAN -Di386 -DSYSV \
	-DNOJC $(KFLAGS) -Otcl -M3e" "LIBS = -lc -lx -ltcp"

#SCO UNIX/386 3.2.2 or later (supports POSIX job control)
sco3r22:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 3.2.2...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	make wermit \
	"CFLAGS= -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP \
	-DPWID_T=int $(KFLAGS) -Otcl" "LIBS = -lx"

#SCO UNIX/386 3.2.2 or later with gcc 1.40 or later, POSIX job control
sco3r22gcc:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 3.2.2, gcc'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	make wermit "CC = gcc" \
	"CFLAGS= -O -DPOSIX -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP \
	-traditional -fpcc-struct-return -fstrength-reduce \
	-DM_BITFIELDS -DM_COFF -DM_I386 -DM_I86 -DM_I86SM \
	-DM_INTERNAT -DM_SDATA -DM_STEXT -DM_SYS3 -DM_SYS5 \
	-DM_SYSIII -DM_SYSV -DM_UNIX -DM_WORDSWAP -DM_XENIX -Dunix \
	-DPWID_T=int $(KFLAGS) " "LNKFLAGS = " "LIBS = -lc -lx"

#SCO UNIX/386 3.2.2 or later (supports POSIX job control) with SCO TCP/IP
sco3r22net:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 3.2.2 + TCP/IP...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	make wermit \
	"CFLAGS= -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP -DTCPSOCKET \
	$(KFLAGS) -DPWID_T=int -Otcl" \
	"LNKFLAGS = " "LIBS = -lc -lx -lsocket -lc_s"

# SCO XENIX/386 2.3.3 SysV with SCO TCP/IP
# System V STREAMS TCP developed by Lachman Associates and
# Convergent Technologies.
sco3r2lai:
	@echo 'Making C-Kermit $(CKVER) for SCO XENIX/386 2.3.3 + TCP/IP...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	make wermit \
	"CFLAGS= -DLAI_TCP -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP \
	-DTCPSOCKET -DPWID_T=int $(KFLAGS) -Otcl -i -M3e" \
	"LNKFLAGS = -i" "LIBS = -lx -lsocket

#PC/IX, Interactive Corp System III for IBM PC/XT
pcix:
	@echo 'Making C-Kermit $(CKVER) for PC/IX...'
	make wermit \
	"CFLAGS= -DPCIX -DISIII $(KFLAGS) -Dsdata=sdatax -O -i" "LNKFLAGS = -i"

#Interactive Corp System III port in general --
is3:
	@echo 'Making C-Kermit $(CKVER) for Interactive System III...'
	make wermit \
	"CFLAGS= -DISIII $(KFLAGS) -Ddata=datax -O -i" "LNKFLAGS= -i"

#Interactive UNIX System V R3, signal() is void rather than int.
#Uses dirent.h and Honey DanBer uucp.
#NOTE: One user claims the definitions for UID_T and GID_T are required.
#If they make trouble for you, remove them.  On the other hand, if you
#have trouble with the other is5r3 entries below, try adding these
#definitions.
is5r3:
	@echo 'Making C-Kermit $(CKVER) for Interactive 386/ix or later...'
	make wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -g -DNOCSETS \
	-DUID_T=ushort -DGID_T=ushort -DDYNAMIC -DI386IX $(KFLAGS)" \
	"LNKFLAGS = -g"

#Interactive UNIX System V R3, Posix variant.  Untested.
#Uses dirent.h and Honey DanBer uucp.
is5r3p:
	@echo 'Making C-Kermit $(CKVER) for Interactive 386/ix or later...'
	make wermit "CFLAGS= -DSVR3 -DDIRENT -DHDBUUCP -g -DNOCSETS \
	-DDYNAMIC -DI386IX -DPOSIX $(KFLAGS)" "LNKFLAGS=" "LIBS=-lcposix"

#Interactive UNIX System V R3, signal() is void rather than int.
#Uses dirent.h and Honey DanBer uucp. Needs -linet for net functions.
is5r3net:
	@echo 'Making C-Kermit $(CKVER) for Interactive 386/ix or later...'
	make wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DTCPSOCKET \
	-DI386IX $(KFLAGS) -O" "LIBS = -linet"

#Interactive UNIX System V R3.
#Like is5r3net, but with -g (whatever that is, some like it, some don't).
is5r3netg:
	@echo 'Making C-Kermit $(CKVER) for Interactive 386/ix or later...'
	make wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DTCPSOCKET \
	-DI386IX $(KFLAGS) -g" "LNKFLAGS = -g" "LIBS = -linet"

#Masscomp System III
rtu:
	@echo 'Making C-Kermit $(CKVER) for Masscomp RTU System III...'
	make wermit \
	"CFLAGS= -UFIONREAD -DATTSV $(KFLAGS) -O" "LNKFLAGS =" "LIBS= -ljobs"

#Masscomp/Concurrent RTU 4.0 or later, Berkeley environment.
#Includes <ndir.h> = /usr/include/ndir.h
#Note "LIBS = lndir" might not be necessary because of "ucb make".
rtubsd:
	@echo 'Making C-Kermit $(CKVER) for Masscomp RTU 4.1A...'
	ucb make wermit \
	"CFLAGS= -DBSD4 -DRTU -DNDIR -DHDBUUCP -DTCPSOCKET $(KFLAGS)" \
	"LIBS = -lndir"
	
#Masscomp/Concurrent RTU 4.0 or later, same as above,
#Includes "usr/lib/ndir.h"
#Note "LIBS = -lndir" might not be necessary because of "ucb make".
rtubsd2:
	@echo 'Making C-Kermit $(CKVER) for Masscomp RTU 4.1A...'
	ucb make wermit "CFLAGS= -DBSD4 -DRTU -DXNDIR -DHDBUUCP $(KFLAGS)" \
	"LIBS = -lndir"

#Masscomp/Concurrent RTU 4.0 or later, same as above,
#Includes <sys/ndir.h>
#Note "LIBS = lndir" might not be necessary because of "ucb make".
rtubsd3:
	@echo 'Making C-Kermit $(CKVER) for Masscomp RTU 4.x BSD...'
	ucb make wermit "CFLAGS= -DBSD4 -DRTU -DHDBUUCP $(KFLAGS)" \
	"LIBS = -lndir"

#Masscomp/Concurrent RTU 4.0 or later, System V R2, using <dirent.h>.
#If this gives problems, add back the -DRTU switch.
#If the -DTCPSOCKET gives trouble, remove it.
rtus5:
	@echo 'Making C-Kermit $(CKVER) for Masscomp RTU 4.x...'
	make wermit "CFLAGS= -DATTSV -DHDBUUCP -DDIRENT -DTCPSOCKET -DDYNAMIC \
	-DNOSETBUF $(KFLAGS)"

#Masscomp/Concurrent RTU 4.x, System V R3, using <dirent.h>.
#Use this one if rtus5 gives warnings about pointer type mismatches.
#If this gives problems, add back the -DRTU switch.
rtus5r3:
	@echo 'Making C-Kermit $(CKVER) for Masscomp RTU Sys V R3...'
	make wermit "CFLAGS= -DSVR3 -DHDBUUCP -DDIRENT $(KFLAGS)"

#DEC Pro-3xx with Pro/Venix V1.0 or V1.1
# Requires code-mapping on non-I&D-space 11/23 processor, plus some
# fiddling to get interrupt targets into resident code section.
# This almost certainly doesn't work any more, but there are almost certainly
# no more of these systems in existence.
provx1:
	@echo 'Making C-Kermit $(CKVER) for DEC Pro-3xx, Pro/Venix 1.x...'
	make wart "CFLAGS= -DPROVX1 $(KFLAGS)" "LNKFLAGS= "
	make wermit "CFLAGS = -DPROVX1 -DNOFILEH -md780" \
		"LNKFLAGS= -u _sleep -lc -md780"

#NCR Tower 1632, OS 1.02
tower1:
	@echo 'Making C-Kermit $(CKVER) for NCR Tower 1632, OS 1.02...'
	make wermit "CFLAGS= -DTOWER1 $(KFLAGS)"

#NCR Tower 32, OS Release 1.xx.xx
tower32-1:
	@echo 'Making C-Kermit $(CKVER) for NCR Tower 32 Rel 1 System V R2...'
	make wermit "CFLAGS = -DATTSV -DDYNAMIC $(KFLAGS) -O" "LNKFLAGS = -n"

#NCR Tower 32, OS Release 2.xx.xx
tower32-2:
	@echo 'Making C-Kermit $(CKVER) for NCR Tower 32 Rel 2 System V R2...'
	make wermit "CFLAGS = -DATTSV -DHDBUUCP -DDYNAMIC $(KFLAGS) -O2" \
	"LNKFLAGS = -n"

#NCR Tower 32, OS Releases based on System V R3
#Don't add -DNAP (doesn't work right) or -DRDCHK (not available in libc).
tower32:
	@echo 'Making C-Kermit $(CKVER) for NCR Tower 32 System V R3...'
	make wermit \
	"CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DNOSYSIOCTLH $(KFLAGS) \
	-DUID_T=ushort -DGID_T=ushort -O1"

#NCR Tower 32, OS Releases based on System V R3
tower32g:
	@echo 'Making C-Kermit $(CKVER) for NCR Tower 32 System V R3, gcc...'
	make wermit "CC = gcc" \
	"CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DNOSYSIOCTLH $(KFLAGS) \
	DUID_T=ushort -DGID_T=ushort -O -fstrength-reduce -fomit-frame-pointer"

#Fortune 32:16, For:Pro 1.8 (mostly like 4.1bsd)
ft18:
	@echo 'Making C-Kermit $(CKVER) for Fortune 32:16 For:Pro 1.8...'
	make wermit "CFLAGS= -DNODEBUG -DBSD4 -DFT18 -DNOFILEH $(KFLAGS) \
	-DPID_T=short"

#Fortune 32:16, For:Pro 2.1 (mostly like 4.1bsd)
ft21:
	@echo 'Making C-Kermit $(CKVER) for Fortune 32:16 For:Pro 2.1...'
	make wermit "CFLAGS= -O -DNODEBUG -DBSD4 -DFT21 -DNOFILEH -SYM 800 \
	-DDYNAMIC $(KFLAGS) -DPID_T=short" "LNKFLAGS= -n -s" "LIBS= -lv -lnet"

#Valid Scaldstar
#Berkeleyish, but need to change some variable names.
valid:
	@echo 'Making C-Kermit $(CKVER) for Valid Scaldstar...'
	make wermit \
	"CFLAGS= -DBSD4 -DNODEBUG -DNOTLOG -Dcc=ccx -DFREAD=1 $(KFLAGS)"

#IBM IX/370 on IBM 370 Series mainframes
#Mostly like sys3, but should buffer packets.
ix370:
	@echo 'Making C-Kermit $(CKVER) for IBM IX/370...'
	make wermit "CFLAGS = -DIX370 -DATTSV $(KFLAGS) -i -O" \
	"LNKFLAGS = -i"

#Amdahl UTS 2.4 on IBM 370 series compatible mainframes.
#Mostly like V7, but can't do initrawq() buffer peeking.
uts24:
	@echo 'Making C-Kermit $(CKVER) for Amdahl UTS 2.4...'
	make wermit "CFLAGS=-DV7 -DPROCNAME=\\\"$(PROC)\\\" \
	-DUTS24 -DBOOTNAME=\\\"$(BOOTFILE)\\\" -DNPROCNAME=\\\"$(NPROC)\\\" \
	-DNPTYPE=$(NPTYPE) $(DIRECT) $(KFLAGS)"

#Amdahl UTSV UNIX System V = System V R2 or earlier.
utsv:
	@echo 'Making C-Kermit $(CKVER) for Amdahl UTSV...'
	make wermit "CFLAGS = -DUTSV $(KFLAGS) -i -O" "LNKFLAGS = -i"

#BBN C/70 with IOS 2.0
#Mostly Berkeley-like, but with some ATTisms
c70:
	@echo 'Making C-Kermit $(CKVER) for BBN C/70 IOS 2.0...'
	make wermit "CFLAGS= -DBSD4 -DC70 $(KFLAGS)"

#Zilog ZEUS 3.21
zilog:
	@echo 'Making C-Kermit $(CKVER) for Zilog Zeus 3.21...'
	make wermit "CFLAGS = -DATTSV -DZILOG -DNODEBUG $(KFLAGS) -i -O" \
	"LNKFLAGS = -i -lpw"

#Whitechapel MG-1 Genix 1.3
white:
	@echo 'Making C-Kermit $(CKVER) for Whitechapel MG-1 Genix 1.3...'
	@touch ckcpro.c
	make wermit "CFLAGS= -DBSD4 -Dzkself()=0  $(KFLAGS)"

#Pixel 1000
pixel:
	@echo 'Making C-Kermit $(CKVER) for Pixel 1000...'
	make wermit "CFLAGS= -DBSD4 -Dzkself()=0 $(KFLAGS)"

ptx:
	make dynix12

#CDC VX/VE 5.2.1
vxve:
	@echo 'Making C-Kermit $(CKVER) for CDC VX/VE 5.2.1...'
	make wermit \
	"CFLAGS = -DATTSV -DVXVE -DNODEBUG -DNOTLOG $(KFLAGS) -i -O" \
	"LNKFLAGS = -i"

#Tandy 16/6000 with Xenix 3.0
#Add -DNOxxx options to remove features if program won't load.
trs16:
	@echo 'Making C-Kermit $(CKVER) for Tandy 16/16000, Xenix 3.0...'
	make wermit "CFLAGS = -DATTSV -DTRS16 -DDCLPOPEN $(KFLAGS) -O" \
	"LNKFLAGS = -n -s"

#DIAB DS90 or LUXOR ABC-9000 with pre-5.2 DNIX.  Sys V with nap() and rdchk().
# nd = no opendir(), readdir(), closedir(), etc.
# Some of the modules fail to compile with -O.
dnixnd:
	@echo 'Making C-Kermit $(CKVER) for DIAB DS90 with very old DNIX 5.2.'
	make wermit "CFLAGS = -DATTSV -DNAP -DRDCHK -DDYNAMIC -DDCLPOPEN \
	-U__STDC__ $(KFLAGS)"

#DIAB DS90 with DNIX 5.2.  Sys V with nap() and rdchk().
# This one has opendir(), readdir(), closedir(), etc.
# Some of the modules fail to compile with -O.
dnix:
	@echo 'Making C-Kermit $(CKVER) for DIAB DS90 with old DNIX 5.2...'
	make wermit "CFLAGS = -DATTSV -DNAP -DRDCHK -DDIRENT -DDYNAMIC  \
	-U__STDC__ $(KFLAGS)"

#DIAB DS90 with DNIX 5.3 or later, with HDB UUCP, nap() and rdchk().
dnix5r3:
	@echo 'Making C-Kermit $(CKVER) for DIAB DS90 with DNIX 5.3...'
	@echo 'with Honey DanBer UUCP'
	make wermit "CFLAGS = -DSVR3 -DHDBUUCP -DNAP -DRDCHK -DDIRENT \
	-DDYNAMIC $(KFLAGS) -O"

#DIAB DS90 with DNIX 5.3 or later, with HDB UUCP, nap() and rdchk() + TCP/IP
dnix5r3net:
	@echo 'Making C-Kermit $(CKVER) for DIAB DS90 with DNIX 5.3...'
	@echo 'with Honey DanBer UUCP and TCP/IP'
	make wermit "CFLAGS = -DSVR3 -DHDBUUCP -DNAP -DRDCHK -DDIRENT \
	-DTCPSOCKET -DDYNAMIC $(KFLAGS) -O -I/usr/include/bsd" "LIBS = ln"

#DIAB DS90 with DNIX 5.3 or later, with HDB UUCP, nap() and rdchk(),
#ANSI C compilation and libraries:
dnix5r3ansi:
	@echo 'Making C-Kermit $(CKVER) for DIAB DS90 with DNIX 5.3...'
	@echo 'with ANSI C Honey DanBer UUCP'
	make wermit \
	"CFLAGS = -DSVR3 -DDIAB -DHDBUUCP -DNAP -DRDCHK -DDIRENT -DDYNAMIC \
	-DCK_ANSILIBS -O -X7 -X9 $(KFLAGS)"

#Ridge 32 with ROS 3.2
ridge32:
	@echo 'Making C-Kermit $(CKVER) Ridge 32 ROS 3.2'
	make wermit \
	"CFLAGS = -DATTSV -DNOFILEH -DNODEBUG -DNOTLOG $(KFLAGS) -i -O" \
	"LNKFLAGS = -i"

#Altos 486, 586, or 986 with Xenix 3.0
altos:
	@echo 'Making C-Kermit $(CKVER) for Altos x86 with Xenix 3.0...'
	make wermit \
	"CFLAGS= -DATTSV -DA986 -DNODEBUG -DNOTLOG $(KFLAGS) -i -O" \
	"LNKFLAGS= -i"

#Altos 986 with Xenix 3.0, as above, but command-line only, minimal size.
#For systems with small memories.  It might also be necessary to chop certain
#modules up into smaller pieces, e.g. ckuus3-6, because of symbol table
#overflow.   If this makefile is too big or complex for the Altos, compile
#and link by hand or write shell scripts.
altosc:
	@echo 'Making C-Kermit $(CKVER) for Altos x86 Xenix 3.0, remote...'
	make wermit \
	"CFLAGS= -DATTSV -DA986 -DNODEBUG -DNOTLOG -DNOSCRIPT -DNODIAL \
	-DNOCSETS -DNOANSI -DNOMSEND -DNOSPL -DNOICP $(KFLAGS) -Mm -O" \
	"LNKFLAGS= -Mm -s"

#Altos 986 with Xenix 3.0, as above, but interactive only, minimal size.
altosi:
	@echo 'Making C-Kermit $(CKVER) for Altos x86 Xenix 3.0, local...'
	make wermit \
	"CFLAGS= -DATTSV -DA986 -DNODEBUG -DNOTLOG -DNOSCRIPT -DNODIAL \
	-DNOCSETS -DNOANSI -DNOMSEND -DNOSPL -DNOCMDL -DNOFRILLS -DNOHELP \
	-DNOSETKEY $(KFLAGS) -Mm -O" "LNKFLAGS= -Mm -s"

#MINIX - PC version with 64K+64K limit.
# Reportedly, the linker (asld) can run out of space while linking.  The only
# way around this is to make a copy of libc.a from which all modules that are
# not used by Kermit are removed.  If you have trouble compiling or running
# wart, "touch wart".  If that doesn't help, "touch ckcpro.c".
# The version configured below has no interactive command parser.
# If you can build this version successfully, maybe there will be room for
# a minimal interactive command parser too; try replacing -DNOICP with
# -DNOSPL.
minix:
	@echo 'Making C-Kermit $(CKVER) for MINIX, no command parser...
	@echo 'TOTALLY UNTESTED!'
	make wermit EXT=s \
	"CFLAGS= -DV7 -DMINIX -i -D_MINIX -D_POSIX_SOURCE -DDYNAMIC \
	-DPID_T=pid_t -DUID_T=uid_t -DGID_T=gid_t -DSIG_V \
	-DNOXMIT -DNOMSEND -DNOFRILLS -DNODIAL -DNOHELP -DNODEBUG -DNOTLOG \
	-DNOSCRIPT -DNOCSETS -DNOICP -DNOSETKEY $(KFLAGS)" \
	"LNKFLAGS= -i -T"

#MINIX - PC version with 64K+64K limit, new (as yet unreleased) ACK 2.0 beta C 
#compiler, which outputs .o object files, rather than .s.  But 'make' still
#expects .s files, so must be patched to use .o.  Tested on Minix 1.5.10.
minixnew:
	@echo 'Making C-Kermit $(CKVER) for MINIX (new ACK 2.0 compiler),'
	@echo 'no command parser...  TOTALLY UNTESTED!'
	make wermit \
	"CFLAGS= -DV7 -DMINIX -i -D_MINIX -D_POSIX_SOURCE -DDYNAMIC \
	-DPID_T=pid_t -DUID_T=uid_t -DGID_T=gid_t -DSIG_V -DNODIAL \
	-DNOHELP -DNODEBUG -DNOTLOG -DNOSCRIPT -DNOCSETS -DNOICP $(KFLAGS)" \
	"LNKFLAGS= -i -T"

#MINIX/386 (PC Minix modifed by Bruce Evans in Australia to use 386 addressing)
minix386:
	@echo 'Making C-Kermit $(CKVER) for MINIX/386...'
	@echo 'TOTALLY UNTESTED!'
	make wermit EXT=s \
	"CFLAGS= -DV7 -DMINIX -D_POSIX_SOURCE -DDYNAMIC $(KFLAGS)"

#MINIX/386 (PC Minix modifed by Bruce Evans in Australia to use 386 addressing)
minix386gcc:
	@echo 'Making C-Kermit $(CKVER) for MINIX/386 with gcc...'
	@echo 'TOTALLY UNTESTED!'
	make wermit "CC=gcc -g -O" "CC2=gcc -g" \
	"CFLAGS= -DV7 -DMINIX -D_POSIX_SOURCE -DDYNAMIC $(KFLAGS)"

#MINIX - 68k version with ACK compiler.
# If you have trouble compiling or running wart, "touch wart".
# If it still doesn't work, "touch ckcpro.c".
# The version configured below has many features removed, including
# the TRANSMIT, MSEND, HELP, and SCRIPT commands, international
# character set support, and the entire script programming language.
# Make sure make(1) has (at least) 100000 chmemory!
# But it does have an interactive command parser.
minix68k:
	@echo 'Making C-Kermit $(CKVER) for MINIX 68k with ACK...
	make wermit \
	"CFLAGS= -DV7 -DMINIX -D_MINIX -D_POSIX_SOURCE -DDYNAMIC \
	-DNODIAL -DNOHELP -DNODEBUG -DNOTLOG \
	-DNOSCRIPT -DNOCSETS -DNOSPL $(KFLAGS) \
	-DPID_T=pid_t -DUID_T=uid_t -DGID_T=gid_t -DSIG_V"

#MINIX - 68k version with c68 compiler.
# If you have trouble compiling or running wart, "touch wart" or
# "touch ckcpro.c". Compiling ckudia.c (no -DNODIAL!) might fail. :-(
# Give c68 250000 bytes of stack+heap; make sure make(1) has at least
# 100000 chmemory.  On a 1Mb Atari ST this means that the recursive
# call of make fails due to memory shortage.  Try "make -n minixc68 >makeit",
# followed by ". makeit".  Otherwise, as above.
minixc68:
	@echo 'Making C-Kermit $(CKVER) for MINIX 68k with c68...
	make wermit "CC= cc -c68" \
	"CFLAGS= -DV7 -DMINIX -D_MINIX -D_POSIX_SOURCE -DDYNAMIC \
	-DNODIAL -DNOHELP -DNODEBUG -DNOTLOG \
	-DNOSCRIPT -DNOCSETS -DNOSPL $(KFLAGS) \
	-DPID_T=pid_t -DUID_T=uid_t -DGID_T=gid_t -DSIG_V"

#MIPS machine with AT&T UNIX System V R3.0
mips:
	@echo 'Making C-Kermit $(CKVER) for MIPS AT&T System V R3.0...'
	make wermit "CFLAGS = -DMIPS -DDIRENT -DPID_T=int \
	-DNOJC $(KFLAGS) -DGID_T=gid_t -DUID_T=uid_t -i -O1500"

#Motorola Delta System V/68 R3, signal() is void rather than int.
#Uses dirent.h and Honey DanBer uucp.
#Supports TCPSOCKET
sv68r3:
	@echo 'Making C-Kermit $(CKVER) for Motorola UNIX System V/68 R3...'
	make wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DTCPSOCKET $(KFLAGS) \
	-O" "LNKFLAGS ="

#Motorola Delta System V/88 R32, signal() is void rather than int.
#Uses dirent.h and Honey DanBer uucp.
#Supports TCPSOCKET
sv88r32:
	@echo 'Making C-Kermit $(CKVER) for Motorola UNIX System V/88 R32...'
	make wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DTCPSOCKET $(KFLAGS) \
	-O" "LNKFLAGS ="

#PFU Compact A Series UNIX System V R3, SX/A TISP V10/L50 (Japan)
#Maybe the -i link option should be removed?
sxae50:
	@echo 'Making C-Kermit $(CKVER) for PFU SX/A V10/L50...'
	make wermit \
	"CFLAGS= -DSVR3 -DDIRENT -DsxaE50 -DTCPSOCKET $(KFLAGS) -i -O \
	-DKANJI" "LNKFLAGS= "

#Olivetti X/OS R2.3
xos23:
	@echo 'Making C-Kermit $(CKVER) for Olivetti X/OS R2.3...'
	$(MAKE) wermit \
	'CFLAGS=-ucb -DBSD4 -DTCPSOCKET -DHDBUUCP -DDYNAMIC -DOXOS $(KFLAGS)' \
	"LNKFLAGS=-ucb -s"		

#Clean up intermediate and object files
clean:
	@echo 'Removing object files...'
	-rm -f ckcmai.$(EXT) ckucmd.$(EXT) ckuusr.$(EXT) ckuus2.$(EXT) \
ckuus3.$(EXT) ckuus4.$(EXT) ckuus5.$(EXT) ckcpro.$(EXT) ckcfns.$(EXT) \
ckcfn2.$(EXT) ckcfn3.$(EXT) ckuxla.$(EXT) ckucon.$(EXT) ckutio.$(EXT) \
ckufio.$(EXT) ckudia.$(EXT) ckuscr.$(EXT) ckwart.$(EXT) ckuusx.$(EXT) \
ckuusy.$(EXT) ckcnet.$(EXT) ckuus6.$(EXT) ckuus7.$(EXT) ckcmdb.$(EXT) \
ckcpro.c wart

#Run Lint on this mess for the SUN/BSD version.
lintsun:
	@echo 'Running Lint on C-Kermit $(CKVER) sources for SUNOS version...'
	lint -x -DSUNOS4 -DDIRENT -DTCPSOCKET -DSAVEDUID -DDYNAMIC \
	ck[cu]*.c > ckuker.lint.sun

lintbsd:
	@echo 'Running Lint on C-Kermit $(CKVER) sources for BSD 4.2 version..'
	lint -x -DBSD4 -DTCPSOCKET ck[cu]*.c > ckuker.lint.bsd42

lints5:
	@echo 'Running Lint on C-Kermit $(CKVER) sources for Sys V version...'
	lint -x -DATTSV ck[cu]*.c > ckuker.lint.s5

lintmips:
	@echo 'Running lint on C-Kermit $(CKVER) sources for MIPS version...'
	lint -DMIPS -DDIRENT -DPID_T=int -DGID_T=gid_t \
	-DUID_T=uid_t -DNOJC ck[cu]*.c > ckuker.lint.mips
