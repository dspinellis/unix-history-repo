# CKUKER.MAK, Mon Nov 23 03:27:33 1992
#
CKVER= "5A(188)"
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
# CAREFUL: Don't put the lowercase word "if", "define", or "end" as the first
# word after the "#" comment introducer in the makefile, even if it is
# separated by whitespace.  Some versions of "make" understand these as
# directives.  Uppercase letters remove the danger, e.g. "# If you have..."
#
# WARNING: This is a huge makefile, and it contains nested makes.
# Some "make" programs run out of memory.  If this happens to you, edit
# away all the parts that do not apply to your system and try again.
#
# For 2.10 or 2.11 BSD, use the separate makefile, ckubs2.mak
#
#   C-Kermit can also be built for many other systems not supported by
#   this makefile, including VAX/VMS, Data General AOS/VS, OS/2, the Apple
#   Macintosh, the Commodore Amiga, OS-9, and the Atari ST.  They have their
#   own separate build procedures.  See the file CKAAAA.HLP for information.
#
# INSTALLATION NOTES:
#
# Rename this file to "makefile" or "Makefile" if necessary.  Pick out the
# make entry most appropriate for your UNIX system from the list below and
# then give the appropriate "make" command, for example "make bsd", "make
# sys5r3", "make posix".  For more detailed installation instructions, read
# the files ckuins.doc and ckccfg.doc.  For descriptions of known problems and
# limitations, read the files ckcker.bwr and ckuker.bwr (the "beware files").
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
# CURSES support: If you are going to use C-Kermit for establishing 
# connections (dialed, network, etc), you can configure it produce a
# formatted file transfer display by including the curses library and adding
# CK_CURSES to the CFLAGS for your option.  There are many examples below,
# like rs6000c, du42c, sunos41c, etc.  After building, you still have to
# SET FILE DISPLAY FULLSCREEN to get the formatted screen display.
#
# Please report failures (preferably with fixes) or successes to the author.
#
# MAKE COMMANDS FOR DIFFERENT UNIX VERSIONS:
#
# + for 386BSD (Jolix) 0.0, 0.1, "make 386bsd" (see comments in entry),
#     or (preferably, if it works) "make bsd44" or "make bsd44c".
# + for Alliant FX/8 with Concentrix 4.1 or later, "make bsdlck"
# + for Altos 486, 586, 986 with Xenix 3.0, "make altos"
# + for Altos ACS68000, 8Mhz 68000, UNIX System 3 Rel 2, 512K, "make altos3"
# ? for Amdahl UTS 2.4 on IBM 370 series & compatible mainframes, "make uts24"
# + for Amdahl UTSV IBM 370 series & compatible mainframes, "make utsv"
# + for Amdahl mainframes with with UNIX System V R 5.2.6b 580, "make sys3"
# ? for Apollo DOMAIN/IX, "make bsd" or "make sys3", for desired environment
# ? for Apollo DOMAIN/IX, if the above fails, try "make apollobsd"
# + for Apollo with SR10.0 or later, BSD environment, "make sr10-bsd"
# + for Apollo with SR10.0 or later, System V environment, "make sr10-s5r3"
# ? for Apollo with straight Aegis using native Aegis i/o,
#     give "cc" commands for each module, then "bind" to link them together.
# + for Apple Macintosh II with A/UX, "make aux", "auxgcc" or "auxufs"
# ? for Apple Macintosh with Minix 1.5.10, "make minix68k" or "make minixc68"
# + for AT&T 6300 PLUS, "make att6300" or (with no debugging) "make att6300nd"
# + for AT&T 6386 WGS Unix PC, "make sys5r3"
# + for AT&T 3B2, 3B20 systems, "make att3bx" or "make att3bxc"
# + for AT&T 3B1, 7300 Unix PC, "make sys3upc", "sys3upcg" (gcc), "sys3upcold"
#     or (for fullscreen curses display) "make sys3upcc", "sys3upcgc" (gcc)
#     or (for fullscreen curses display and shared library) "make sys3upcshcc"
# + for AT&T System III/System V R2 or earlier, "make sys3" or "make sys3nid"
# + for AT&T System III/System V with Honey DanBer UUCP, "make sys3hdb"
# + for AT&T System V on DEC VAX, "make sys3" or "make sys5r3"
# + for AT&T System V R3, use "make sys5r3".  This is different from the above.
# + for AT&T System V R4, "make sys5r4", "make sys5r4sx", or "make sys5r4nx",
#     or if the ANSI C function prototyping makes trouble, add -DNOANSI,
#     as in "sys5r4sxna" entry
# + for AT&T System V R4 with Wollongong TCP/IP, "make sys5r4net", ...
# + for AT&T (USL) System V R4.2 ("Destiny") use the sys5r4 entries.
# + for Atari ST with Minix ST 1.5.10.3, "make minix68k" or "make minixc68"
# ? for BBN C/70 with IOS 2.0, "make c70"
# + for Bell UNIX Version 7 (aka 7th Edition), "make v7" (but see below)
# + for BSDI/386, "make bsdiposix"
# + for Berkeley Unix 2.4, "make v7" (but read v7 material below)
# ? for Berkeley Unix 2.9 (DEC PDP-11 or Pro-3xx), "make bsd29"
# ? for Berkeley Unix 2.10, use ckubs2.mak (a separate makefile)
# + for Berkeley Unix 2.11, use ckubs2.mak (a separate makefile)
#     Read the instructions in ckubs2.mak.
#     Rename ckubs2.mak to makefile, then "make bsd210" or "make bsd211".
# + for Berkeley Unix 4.1, "make bsd41"
# + for Berkeley Unix 4.2, "make bsd" (tested with 4.2 and 4.3)
# + for Berkeley Unix 4.2 or 4.3 with HoneyDanBer UUCP, "make bsdhdb"
# + for Berkeley Unix 4.3, "make bsd43" (uses acucntrl program for locks)
# + for Berkeley Unix 4.3 without acucntrl program, "make bsdlck" or "make bsd"
# ? for Berkeley Unix 4.3-Tahoe, same as 4.3 BSD
# ? for Berkeley Unix 4.3-Reno, "make bsd43" or "make bsd44" or "make bsd44c"
# + for Berkeley Unix 4.3-Carson City, "make bsd44" or "make bsd44c"
# + for Berkeley Unix 4.4-Networking/2, "make bsd44" or "make bsd44c"
# + for Berkeley Unix 4.4-Alpha, "make bsd44" or "make bsd44c"
# ? for Bull DPX/2 with BOS/X, "make rs6000"
# ? for Cadmus, "make sys3"
# ? for Callan Unistar, "make sys3"
# ? for CDC VX/VE 5.2.1 Sys V emulation, "make vxve"
# + for Charles River Data Systems Universe 680x0 with UNOS 9.2, maybe
#     also other UNOS versions, "make crds"
# ? for CIE Systems 680/20 with Regulus, "make cie"
# + for Commodore Amiga 3000UX Sys V R4, "make sys5r4sx"
# + for Commodore Amiga 3000UX Sys V R4 and TCP/IP, "make svr4amiganet"
# ? for Commodore Amiga with Minix 1.5.10, "make minix68k" of "make minixc68"
# + for Consensys UNIX SV/386 R4V3, "make sys5r4sx"
# + for Convergent with CTIX Sys V R2, "make sys5"
# + for Convex C1, "make convex"
# + for Convex C210 with Convex/OS 8, "make convex8"
# + for Convex C2 with Convex/OS 9.1, "make convex9"
# + for Cray X/MP or YMP or C90 with UNICOS 6.x (System V R3), "make cray"
# + for Cray X/MP or YMP or C90 with UNICOS 7.x (System V R4), "make cray"
# + for Cyber 910 (Silicon-Graphics Iris) with Irix 3.3, "irix33"
# + for Data General Aviion with Sys V R4 (DG/UX 5.4), "make dgux540"
#     or "make dgux540c" (compile ckwart separately if necessary)
# ? for Data General Aviion with DG/UX 4.30 using Sys V-isms, "make dgux430"
# ? for Data General Aviion with DG/UX 4.30 using BSD-isms, "make dgux430bsd"
# ? for Data General Aviion with DG/UX 4.30, "make sys5r3"
# ? for Data General Aviion, earlier UNIX versions,
#     "make sys5r3" (maybe compile ckwart separately)
# ? for Data General MV systems with DG/UX, ???
# + for Data General MV systems with MV/UX, use AOS/VS C-Kermit
# + for Data General MV systems with AOS/VS, use CKDKER.MAK.
# + for DEC VAX with Ultrix 1.x "make bsd"
# + for DEC VAX with Ultrix 2.x "make du2"
# + for DEC VAX or DECstation with Ultrix 3.0, 3.1, "make du3"
# + for DECstation (or VAX) with Ultrix 4.0 or 4.1, "make du4"
# + for DECstation (or VAX) with Ultrix 4.x and Kanji support, "make du4kanji"
# + for DECstation (or VAX) with Ultrix 4.2, "make du42" or "make du42c"
# + for DECstation (or VAX) with Ultrix 4.2, Sys V R4 world, "make du42s5r4"
# + for DECstation (or VAX) with Ultrix 4.x, POSIX world, "make posix"
# + for DECstation with Ultrix 4.3, "make du42"
# + for DECstation with OSF/1 V1.0, "make dec-osf"
# ? for DEC Alpha AXP with OSF/1, "make dec-osf"
# - for DEC Pro-350 with Pro/Venix V1.x, "make provx1" (5A is too big)
# ? for DEC Pro-350 with Pro/Venix V2.0 (Sys V), "make sys3nid" 
# ? for DEC Pro-380 with Pro/Venix V2.0 (Sys V), "make sys3" or "make sys3nid"
# ? for DEC Pro-380 with 2.9, 2.10, or 2.11 BSD, "make bsd29" or "make bsd210"
# + for Dell UNIX Issue 2.x (= USL Sys V/386 R4.x + fixes), "make dellsys5r4"
#     or "make dellsys5r4c"
# + for DIAB DS90 with DNIX (any version) create an empty <sys/file.h> if
#     this file does not already exist (or add -DNOFILEH to the make entry).
# + for DIAB DS90 or LUXOR ABC-9000 with pre-5.2 DNIX, add "getcwd" to libc.a
#     (see ckuins.doc), then "make dnixold".
# + for DIAB DS90 with DNIX 5.2 (Sys V.2) or earlier, "make dnix" or "dnixnd".
# + for DIAB DS90 with DNIX 5.3 (Sys V.3), "make dnix5r3"
# + for DIAB DS90 with DNIX 5.3 (Sys V.3) and TCP/IP, "make dnix5r3net"
# + for DIAB DS90 with DNIX 5.3 2.2 (Sys V.3), ANSI C, "make dnix5r3ansi"
#     but you have to fix a bug in /usr/include/stdlib.h first:
#     change "extern void free(char *str);" to "extern void free(void *str);"
# + for Encore Multimax 310, 510 with Umax 4.2, "make umax42"
# + for Encore Multimax 310, 510 with Umax 4.3, "make umax43"
# + for Encore Multimax 310, 510 with Umax V 2.2, use Berkeley cc, "make bsd"
# + for Encore 88K with Umax V 5.2, "make encore88k"
# + for ESIX System V R4.0.3 with TCP/IP support, "make esixr4"
# ? for Everex STEP 386/25 Rev G with ESIX Sys V R3.2D, "make sys5r3"
# ? for Fortune 32:16, For:Pro 1.8, "make ft18"
# + for Fortune 32:16, For:Pro 2.1, "make ft21"
# + for FPS 500 with FPX 4.1, "made bsd"
# ? for Harris Night Hawk 88K or 68K with CX/UX pre-6.1, "make sys5r3"
# + for Harris Night Hawk 88K or 68K with CX/UX 6.1 or later, "make cx_ux"
# ? for Heurikon, "make sys3"
# ? for HP-3000, MPE/ix, "make posix"?
# + for HP-9000 Series 500, HP-UX 5.21 with WIN/TCP 1.2 "make hpux500wintcp"
# + for HP-9000 Series, HP-UX < 6.5, without long filenames, no job control,
#     "make hpuxpre65"
# + for HP-9000 Series, HP-UX pre-7.0, without long filenames, "make hpux"
# ? for HP-9000 Series, HP-UX 7.0 or later, no long filenames, "make hpux7sf"
# + for HP-9000 Series with HP-UX Sys V R2, BSD long names, "make hpuxlf"
# + for HP-9000 Series with HP-UX Sys V R2, dirent long names, "make hpuxde"
#     or (to include TCP/IP support) "make hpuxdetcp"
# + for HP-9000 Series with HP-UX Sys V R3, "make hpuxs5r3"
# + for HP-9000 Series with HP-UX 7.0, TCP/IP, long filenames, "make hpux70lfn"
# + for HP-9000 300/400 Series (680x0) with HP-UX 8.0, TCP/IP, "make hpux80"
#      or "make hpux80c"
# + for HP-9000 700/800 Series (PA-RISC), HP-UX 8.0, TCP/IP, "make hpux80pa"
#      or "make hpux80pac"
# + for HP-9000 Series with HP-UX 8.0, no TCP/IP, long filenames,
#      "make hpux80notcp" or "make hpuxde"
# ? for IBM 370 Series with IX/370, "make ix370"
# + for IBM 370 Series with AIX/370 3.0, "make aix370"
# - for IBM PC/AT 286 & compatibles with Mark Williams Coherent OS,
#     command-line-only version, "make coherent" (5A is too big)
# + for IBM PC/AT & compatibles with Mark Williams Coherent OS,
#     minimum interactive version, "make coherentmi" (386 version only)
# + for IBM PC/AT & compatibles with Mark Williams Coherent OS,
#     full interactive version, "make coherentmax" (386 version only)
# - for IBM PC/AT & compatibles with original MINIX, "make minix" (too big)
# + for IBM PC/AT & compatibles with MINIX, new compiler, "make minixnew"
# + for IBM PC family, 386-based, with MINIX/386, "make minix386"
#     or if you have GNU CC, "make minix386gcc"
# + for IBM PS/2 with PS/2 AIX 1.0, 1.1, or 1.2, "make ps2aix"
# ? for IBM PS/2 with PS/2 AIX 1.3, "make ps2aix3"
# + for IBM RISC System/6000 with AIX 3.0 or 3.1 "make rs6000" or
#     "make rs6000c"
# + for IBM RISC System/6000 with AIX 3.2 (OSF/AES compliant), 
#     "make rs6000", "make rs6000c", "make rs6aix32", or "make rs6aix32c"
# ? for IBM RT PC with AIX 2.1, "make sys3"
# + for IBM RT PC with AIX 2.2.1, "make rtaix" (special lockfile handling)
# ? for IBM RT PC with ACIS 4.2, "make bsd"
# + for IBM RT PC with ACIS 4.3, "make rtacis" or "make bsd KFLAGS=-DNOANSI"
# ? for ICL DRS3000 (80486) with DRS/NX, "make iclsys5r4"
# + for ICL DRS6000 (Sparc) with DRS/NX, "make iclsys5r4"
# + for Intel 302 with Bell Tech Sys V/386 R3.2, "make sys5r3"
# ? for Intel Xenix/286, "make sco286"
# ? for Interactive System III (PC/IX) on PC/XT, "make pcix"
# ? for Interactive System III on other systems, "make is3"
# + for Interactive 386/ix 1.0.6 with TCP/IP networking, "make is5r3net2"
# + for Interactive 386/ix 2.0.x, "make is5r3" or (POSIX) "make is5r3p"
# + for Interactive 386/ix 2.0.x with TCP/IP networking, "make is5r3net"
#     or "make is5r3net2"
# + for Interactive UNIX Sys V R3 2.2 with job control, curses, "make is5r3jc"
# + for Interactive UNIX Sys V R3 3.0, "make is5r3jc"
# + for Linux/386, "make linuxgcc2" or "make linuxgcc2net"
# + for Luxor ABC-9000 (DIAB DS-90) with pre-5.2 DNIX, add "getcwd" to libc.a
#     (see ckuins.doc), then "make dnixold".
# ? for Masscomp RTU AT&T System III, "make rtu"
# + for Masscomp/Concurrent with RTU 4.0 or later, BSD environment, "make 
#     rtubsd", "make rtubsd2", "make rtubsd3" (depending on where ndir.h 
#     is stored, see entries below).
# ? for Masscomp/Concurrent with RTU 4.0 or later, System V environment,
#     "make rtus5" (System V R2) or "make rtus5r3" (System V R3 or later).
# + for Microport SV/AT (System V R2), "make mpsysv"
# + for Microport SVR4 2.2, 3.1, or 4.1 "make sys5r4sx"
# ? for Microsoft,IBM Xenix (/286, PC/AT, etc), "make xenix" or "make sco286"
# + for MIPS System with RISC/os (UMIPS) 4.52 = AT&T SVR3, "make mips"
# + for Modcomp 9730, Real/IX, "make sys5r3" (or modify to use gcc = GLS cc)
# ? for Motorola Four Phase, "make sys3" or "make sys3nid"
# + for Motorola Delta System V/68 R3, "make sv68r3"
# + for Motorola Delta System V/68 R3V5, "make sv68r3v5"
# + for Motorola Delta System V/68 R3V5.1, "make sv68r3v51"
# + for Motorola Delta System V/88 R32, "make sv88r32"
# + for Motorola Delta System V/88 R40, "make sv88r40"
# + for Mt Xinu Mach386 on 386/486-based PCs, "make bsd43"
# ? for NCR Tower 1632, OS 1.02, "make tower1"
# + for NCR Tower 1632 or Minitower with System V R2, "make sys3"
#     or "make sys3nv"
# + for NCR Tower 32, OS Release 1.xx.xx, "make tower32-1"
# + for NCR Tower 32, OS Release 2.xx.xx, "make tower32-2"
# + for NCR Tower 32, OS Releases based on Sys V R3, "make tower32"
# + for NCR Tower 32, OS Releases based on Sys V R3 with gcc "make tower32g"
# + for NCR System 3000, AT&T UNIX System V R4 2.0, "make sys5r4sxna"
# + for NeXT, "make next" or "make nextc"
# + for OkiStation 7300 Series, "make sys5r4sxtcp" 
# + for Olivetti LSX-3020 with X/OS R.2.3, "make xos23" or "make xos23c"
# + for PFU Compact A Series SX/A TISP V10/E50 (Japan), "make sxae50"
# ? for Plexus, "make sys3"
# + for Pyramid 9810x (T series) OSx 4.4b, "make pyramid" or "ucb make pyramid"
# + for POSIX on anything, "make posix" (but adjustments might be necessary).
# + for POSIX on SunOS 4.1 or later, "make sunposix"
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
#     or "make sco3r22netc" (curses)
# + for SCO ODT 1.1, "make sco3r22net" or "make sco3r22netc" (curses)
# + for SCO UNIX/386 3.2.4, no network support, "make sco32v4"
# + for SCO UNIX/386 3.2.4 with TCP/IP, "make sco32v4net"
# + for SCO UNIX 3.2 V4.0 with TCP/IP, <dirent.h> for Extended Acer File 
#     System (EAFS), curses, ANSI C compilation, "make sco32v4netc"
# + for SCO ODT 2.0, "make sco32v4net" or "make sco32v4netc"
# + for SCO Xenix/386 or UNIX/386 with Excelan TCP/IP, "make sco3r2net"
#     or (to add curses support) "make sco3r2netc"
# + for SCO Xenix 2.3.x with Racal-InterLan TCP/IP, "make sco3r2netri"
# + for SCO Xenix 2.3.x with SCO (Lachman) TCP/IP, "make sco3r2lai"
# + for other UNIX varieties with Racal Interlan TCP/IP, read sco3r2netri entry
# + for Sequent with DYNIX/PTX 1.2.1, "make dynix12"
# + for Sequent with DYNIX/PTX 1.3 or 1.4 with TCP/IP, "make dynix13"
# + for Sequent Balance 8000 or B8 with DYNIX 3.0.xx, "make dynix3"
#    or "make dynix3noacu"
# + for Sequent Symmetry S81 with DYNIX 3.0.xx, "make dynix3"
# + for Sequent DYNIX 3.1.xx, "make dynix31"
# + for Silicon Graphics Iris System V IRIX 3.2 or earlier, "make iris"
# + for Silicon Graphics Sys V R3 with IRIX 3.3 or later, "make sys5r3"
# + for Silicon Graphics Iris Indigo with IRIX 4.0, "make irix40"
# ? for Solaris 2.0, "make sunsol20" -- should work on any computer that
#    runs Solaris 2.0, since it supposed to be portable.
# + for Solaris 2.1 on a Sun Sparcstation, "make sunos51c"
# + for Solbourne 4/500 with OS/MP 4 "make sunos4"
# + for Solbourne 4/500 with OS/MP 4.1 "make sunos41" or "make sunos41c"
# + for SONY NEWS with NEWS-OS 4.0.1C, "make sonynews"
# + for SONY NEWS with NEWS-OS 4.1.2C, "make sonynews"
# + for Sperry/UNISYS 5000/20, UTS V 5.2 3R1, "make sys5"
# + for Sperry/UNISYS 5000/30/35/50/55, UTS V 5.2 2.01, "make unisys5r2"
# + for Sperry/UNISYS 5000/80 with System V R3, "make sys5r3"
# + for Sperry/UNISYS 5000/95 with System V R3, "make sys5r3"
#     For UNISYS SVR3 it might be necessary to "make sys5r3 KFLAGS=-UDYNAMIC"
# + for Stardent 1520, "make sys5r3"
# + for Sun with pre-4.0 SunOS versions, "make bsd" (or appropriate variant)
# + for Sun with SunOS 4.0, BSD environment, "make sunos4"
# + for Sun with SunOS 4.0, BSD, with SunLink X.25, make sunos4x25
# + for Sun with SunOS 4.0, AT&T Sys V R3 environment, "make sunos4s5"
# + for Sun with SunOS 4.1 or 4.1.1, BSD environment, "make sunos41"
#     or "make sunos41c" (curses) or "make sunos41gcc" (compile with gcc)
# + for Sun with SunOS 4.1, BSD, with SunLink X.25, "make sunos41x25"
#     or "make sunos41x25c" (curses)
# + for Sun with SunOS 4.1, 4.1.1, AT&T Sys V R3 environment, "make sunos41s5" 
# + for Sun with SunOS 4.1, 4.1.1, POSIX environment, "make sunposix"
# + for Sun with SunOS 4.1.2, "make sunos41" or any of its variations.
#     NOTE:  All SUNOS 4.x systems -- Shared libraries are used by default.
#       If this causes problems, add -Bstatic to CFLAGS.
#     NOTE2: When building C-Kermit under SunOS for the BSD universe,
#       but /usr/5bin/cc is ahead of /usr/ucb/cc in your PATH, add
#       "CC=/usr/ucb/cc CC2=/usr/ucb/cc" to the make entry.
#     NOTE3: If an executable built on one type of Sun hardware does not work
#       on another type, rebuild the program from source on the target machine.
# + for Sun with Solaris 1.0 == SUNOS 4.1.2, "make sunos41"
# ? for Sun with SunOS 5.0 == Solaris 2.0, "make sunsol20"
# + for Sun with SunOS 5.1 == Solaris 2.1, "make sunos51"
# ? for Sun with Solaris 2.0, "make sunsol20"
# + for Tandy 16/6000 with Xenix 3.0, "make trs16"
# + for Tektronix 6130/4132/43xx (e.g.4301) with UTek OS, "make utek"
#     or (for models without hardware flow control), "make uteknohwfc"
# + for Tektronix XD88 series with UTekV OS, "make sys5r3"
# + for Tri Star Flash Cache with Esix SVR3.2, "make sys5r3"
# ? for Unistar, "make sys5"
#   for UNISYS ... (See Sperry)
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
# 2.10 and 2.11BSD follow the 4.3BSD path and are build with a special entry,
# bsd210, which uses overlays.
#
# The v7 and 2.9bsd versions assume I&D space on a PDP-11.  When building
# C-Kermit for v7 on a PDP-11, you should probably add the -i option to the
# link flags.  Without I&D space, overlays will be necessary (if available),
# or code segment mapping (a`la Pro/Venix) if that's available.
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
CC= cc
CC2= cc
MAKE= make
SHELL=/bin/sh
#
###########################################################################
#
# Easy installation. Modify this to suit your own computer's file organization
# and permissions.  If you don't have write access to the destination
# directories, "make install" will fail.

WERMIT = makewhat
DESTDIR =
BINDIR = /usr/contrib/bin
MANDIR = /usr/contrib/man/cat1
MANEXT = 0
ALL = $(WERMIT)

all: $(ALL)

install:
	install -s -o bin -g bin -m 555 wermit $(DESTDIR)$(BINDIR)/kermit
	nroff -man ckuker.nr > $(DESTDIR)$(MANDIR)/kermit.$(MANEXT)
	chmod 444 $(DESTDIR)$(MANDIR)/kermit.$(MANEXT)

makewhat:
	@echo 'make what?  You must tell which system to make C-Kermit for.'
	@echo Examples:  make bsd43, make sys5, make sunos41, etc.

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
# Like a PDP-11 without separate I&D space. This example is for 2.11 BSD:

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
		 -Z ckuxla.$(EXT) ckcpro.$(EXT) ckucmd.$(EXT) \
		    ckuus3.$(EXT) \
		 -Z ckuus4.$(EXT) ckuus5.$(EXT) ckuusr.$(EXT) ckuus6.$(EXT) \
		    ctime.o \
		 -Z ckuus7.$(EXT) ckcfn3.$(EXT) ckudia.$(EXT) ckuscr.$(EXT) \
		    ckcnet.$(EXT)  \
		 -Z ckuus2.$(EXT) ckuusx.$(EXT) ckucon.$(EXT) ckuusy.$(EXT) \
		    getpwent.o ndbm.o \
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
# Put the ones that need short makefiles first.

bsd210:
	@echo Please use ckubs2.mak to build C-Kermit $(CKVER) for 2.10BSD.

bsd211:
	@echo Please use ckubs2.mak to build C-Kermit $(CKVER) for 2.11BSD.

#Apple Mac II, A/UX
#Warning, if "send *" doesn't work, try the auxufs makefile entry below.
aux:
	@echo Making C-Kermit $(CKVER) for Macintosh A/UX...
	$(MAKE) wermit "CFLAGS = -DAUX -DDYNAMIC -DTCPSOCKET $(KFLAGS) -i -O" \
		"LNKFLAGS = -i"

#Apple Mac II, A/UX, compiled with gcc
auxgcc:
	@echo Making C-Kermit $(CKVER) for Macintosh A/UX...
	$(MAKE) wermit "CFLAGS = -DAUX -DDYNAMIC -DTCPSOCKET $(KFLAGS) \
	-traditional -i -O" "LNKFLAGS = " "CC = gcc" "CC2 = gcc"

#Apple Mac II, A/UX, but with ufs file volumes, uses <dirent.h>.
auxufs:
	@echo Making C-Kermit $(CKVER) for Macintosh A/UX...
	$(MAKE) wermit "CFLAGS = -DAUX -DDYNAMIC -DTCPSOCKET -DDIRENT \
	$(KFLAGS) -i -O" "LNKFLAGS = -i"

#Berkeley Unix 4.1
bsd41:
	@echo Making C-Kermit $(CKVER) for 4.1BSD...
	$(MAKE) wermit "CFLAGS= -DBSD41" "LIBS = -ljobs"

#Berkeley 4.2, 4.3, also Ultrix-32 1.x, 2.x, 3.x, many others
# Add -O, -DDYNAMIC, -s, etc, if they work.
# If you have a version of BSD but signal() is void rather than int,
# "make bsd KFLAGS=-DSIG_V".
bsd:
	@echo Making C-Kermit $(CKVER) for 4.2BSD...
	$(MAKE) wermit "CFLAGS= -DBSD4 -DTCPSOCKET $(KFLAGS)"

#Berkeley 4.2, 4.3, minimum size
bsdm:
	@echo Making C-Kermit $(CKVER) for 4.2BSD...
	$(MAKE) wermit "CFLAGS= -O -DBSD4 -DDYNAMIC -DNODIAL -DNOHELP \
	-DNODEBUG -DNOTLOG -DNOSCRIPT -DNOCSETS -DNOICP $(KFLAGS)" \
	"LNKFLAGS = -s"

#Berkeley Unix with HoneyDanBer UUCP
bsdhdb:
	@echo Making C-Kermit $(CKVER) for 4.2BSD with HDB UUCP...
	$(MAKE) wermit "CFLAGS= -DHDBUUCP -DBSD4 -DTCPSOCKET $(KFLAGS)"

#Berkeley Unix 4.3 with acucntrl program
bsd43:
	@echo Making C-Kermit $(CKVER) for 4.3BSD with acucntrl...
	$(MAKE) wermit "CFLAGS= -DBSD43 -DACUCNTRL -DTCPSOCKET $(KFLAGS) -O"

#Ditto, with curses support.
bsd43d:
	@echo Making C-Kermit $(CKVER) for 4.3BSD with acucntrl and curses...
	$(MAKE) wermit "CFLAGS= -DBSD43 -DACUCNTRL -DTCPSOCKET -DCK_CURSES \
	$(KFLAGS) -O"  "LIBS=-lcurses -ltermcap"

#Berkeley Unix 4.2 or 4.3 with lock directory /usr/spool/uucp/LCK/LCK..ttyxx,
#but without acucntrl program
bsdlck:
	@echo Making C-Kermit $(CKVER) for 4.2BSD, /usr/spool/uucp/LCK/...
	$(MAKE) wermit "CFLAGS= -DLCKDIR -DBSD4 -DTCPSOCKET $(KFLAGS)"

#Berkeley UNIX 4.4-Alpha, NET/2, etc (Post-Reno), with TCP/IP networking.
#NOTE: This is not a pure POSIX configuration.  Using -DPOSIX instead of
#-DBSD44 prevents any kind of directory-reading (for wildcard expansion),
#and disallows use of ENOTCONN symbol for detecting broken network
#connections, and disallows RTS/CTS flow control, and would also require
#definition of the appropriate UUCP lockfile convention.
bsd44:
	@echo Making C-Kermit $(CKVER) for 4.4BSD...
	$(MAKE) wermit "CFLAGS= -DBSD44 -DDYNAMIC -DTCPSOCKET $(KFLAGS) -O"

#Berkeley UNIX 4.4, as above, but with curses for fullscreen display
bsd44c:
	@echo Making C-Kermit $(CKVER) for 4.4BSD with curses...
	$(MAKE) -f Makefile wermit \
	    "CFLAGS= -DBSD44 -DCK_CURSES -DDYNAMIC -DTCPSOCKET \
	    $(KFLAGS) -O" "LIBS= -lcurses -ltermcap"

#Tektronix 6130, 4319, 4301, etc, with UTek OS, /usr/spool/uucp/LCK./...
#The models that support hardware flow control.
utek:
	@echo 'Making C-Kermit $(CKVER) for 4.2BSD/UTek, hardware flow control'
	$(MAKE) wermit "CFLAGS= -O -DLCKDIR -DBSD4 -DTCPSOCKET -DDYNAMIC \
	-DUTEK -DDCLPOPEN -DLOCK_DIR=\\\"/usr/spool/uucp/LCK.\\\" \
	-DTRMBUFL=2048 -DCK_DTRCTS $(KFLAGS)"


#Tektronix 4315, 4316, 4317 with UTek OS, /usr/spool/uucp/LCK./...
#The models that do not fully support hardware flow control.
uteknohwfc:
	@echo 'Making C-Kermit $(CKVER) for 4.2BSD/UTek, no h/w flow control'
	$(MAKE) wermit "CFLAGS= -O -DLCKDIR -DBSD4 -DTCPSOCKET -DDYNAMIC \
	-DUTEK -DDCLPOPEN -DLOCK_DIR=\\\"/usr/spool/uucp/LCK.\\\" \
	-DTRMBUFL=2048 $(KFLAGS)"

# The following makefile entry should work for any Harris Night Hawk system
# (either 88k or 68k based) running release 6.1 or later of the CX/UX
# operating system. This is a POSIX and ANSI-C compliant system which also
# supports BSD networking. (Earlier CX/UX releases will probably work with
# sys5r3, but this has not been verified).
#
cx_ux:
	@echo Making C-Kermit $(CKVER) for Harris Night Hawk with CX/UX 6.1
	$(MAKE) wermit \
	"CFLAGS=-DPOSIX -DTCPSOCKET -DHDBUUCP -DPID_T=pid_t \
	-DUID_T=uid_t -DGID_T=gid_t -DDYNAMIC $(KFLAGS) -Xa -O3 -g" \
	"LINKFLAGS=-O3"

#Mark Williams Coherent 286 or 386 on IBM PC family.
#There is a 64K limit on program size, so this is a command-line only version.
coherent:
	$(MAKE) "CFLAGS = -O -DCOHERENT -DDYNAMIC -DNOANSI -DNOICP -DNOSETKEY \
	-DNOCSETS -DNOHELP -DNODIAL -DNOSCRIPT -DNODEBUG -DNOTLOG -DNOXMIT \
	-DNOMSEND -DNOFRILLS -DNOSYSIOCTLH $(KFLAGS) -VSUVAR" wermit

#Mark Williams Coherent 386 on IBM PC family.
#This will make a "minimum interactive" version - no scripts,
#no character sets, no help, no dial, no debug/transaction logging, no
#transmit, msend, mail, type, etc.
coherentmi:
	$(MAKE) "CFLAGS = -O -DCOHERENT -DDYNAMIC -DNOANSI -DNOSETKEY \
	-DNOSHOW -DNOCSETS -DNOHELP -DNODIAL -DNOSCRIPT -DNODEBUG -DNOTLOG \
	-DNOXMIT -DNOMSEND -DNOFRILLS -DNOSYSIOCTLH -DNOSERVER -DNOUUCP \
	-DNOSPL -DNOSETBUF -DNOPUSH -DNOMDMHUP -DNOJC -DNOFDZERO -DNOESCSEQ \
	-DNOCMDL $(KFLAGS) -VSUVAR" wermit

#Mark Williams Coherent 386 on IBM PC/AT family.
coherentmax:
	$(MAKE) "CFLAGS = -O -DCOHERENT -DDYNAMIC -DNOANSI \
	-DNOSYSIOCTLH $(KFLAGS) -VSUVAR" "LNKFLAGS = -O -s" wermit

#DEC Ultrix 2.x
# Add -O, -DDYNAMIC, -s, etc, if they work.
du2:
	@echo Making C-Kermit $(CKVER) for Ultrix 2.x ...
	$(MAKE) wermit "CFLAGS= -DBSD4 -DTCPSOCKET -DDU2 $(KFLAGS)"

#DEC Ultrix 3.1 (and probably also 3.0)
du3:
	@echo Making C-Kermit $(CKVER) for Ultrix 3.x...
	$(MAKE) wermit "CFLAGS= -DBSD4 -DTCPSOCKET -DDIRENT -DSIG_V \
	-DDYNAMIC $(KFLAGS) -O" "LNKFLAGS = -s"

#Name changed from ds4 to du4, should work on VAXes as well as DECstations.
ds4:
	$(MAKE) "MAKE=$(MAKE)" du4

#DEC Ultrix 4.0 or 4.1 on DECstation, VAXstation, VAX, etc.
du4:
	@echo Making C-Kermit $(CKVER) for Ultrix 4.0 or 4.1...
	$(MAKE) wermit "CFLAGS= -DBSD4 -DTCPSOCKET -DSIG_V -DDYNAMIC \
	$(KFLAGS) -Olimit 1350" "LNKFLAGS = -s"

#DEC Ultrix 4.2 on DECstation, VAXstation, VAX, etc.
#Like du4, except new C compiler supports -O2 optimization.
du42:
	@echo Making C-Kermit $(CKVER) for Ultrix 4.2...
	$(MAKE) wermit "CFLAGS= -DBSD4 -DTCPSOCKET -DSIG_V -DDYNAMIC \
	$(KFLAGS) -O2 -Olimit 1350" "LNKFLAGS = -s"

#DEC Ultrix 4.2 on DECstation, VAXstation, VAX, etc.
#Like du42, but with curses support added
du42c:
	@echo Making C-Kermit $(CKVER) for Ultrix 4.2...
	$(MAKE) wermit "CFLAGS= -DBSD4 -DTCPSOCKET -DSIG_V -DDYNAMIC \
	-DCK_CURSES $(KFLAGS) -O2 -Olimit 1350" "LNKFLAGS = -s" \
	"LIBS= -lcurses -ltermcap"

#DEC Ultrix 4.2 on DECstation, VAXstation, VAX, etc, System V R4 environment
du42s5r4:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4 on Ultrix...'
	$(MAKE) wermit "CFLAGS = -O2 -Olimit 1350 -DSVR4 -DDIRENT -DHDBUUCP \
	-DDYNAMIC -DTCPSOCKET $(KFLAGS)" "LNKFLAGS = -s"

#DEC OSF/1 V1.0 on DECstation.
dec-osf:
	@echo Making C-Kermit $(CKVER) for DECstation OSF/1 V1.0...
	$(MAKE) wermit "CFLAGS= -DBSD4 -DOSF -D_BSD -DTCPSOCKET -DSIG_V \
	-DDYNAMIC $(KFLAGS) -Olimit 1350" "LNKFLAGS = -s" "LIBS = -lbsd"

#Sequent DYNIX/PTX 1.2.1
dynix12:
	@echo Making C-Kermit $(CKVER) for Sequent DYNIX/PTX 1.2.1...
	$(MAKE) wermit "CFLAGS= -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DPTX \
	-DPID_T=pid_t -DUID_T=uid_t -DGID_T=gid_t $(KFLAGS) -i -O" \
	"LNKFLAGS = -i"

#Sequent DYNIX/PTX 1.3
dynix13:
	@echo Making C-Kermit $(CKVER) for Sequent DYNIX/PTX 1.3 TCP/IP...
	$(MAKE) wermit "CFLAGS= -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DPTX  -O \
	-DPID_T=pid_t -DUID_T=uid_t -DGID_T=gid_t -DTCPSOCKET $(KFLAGS) -i" \
	"LNKFLAGS = -i" "LIBS = -lsocket -linet -lnsl"

#Sequent Dynix 3.0.x
dynix3:
	@echo Making C-Kermit $(CKVER) for Sequent Dynix 3.0.x...
	$(MAKE) wermit "CFLAGS= -DBSD43 -DACUCNTRL -DTCPSOCKET -O \
	-DUID_T=int -DGID_T=int $(KFLAGS)"

#Sequent Dynix 3.0.x, no ACUCNTRL
dynix3noacu:
	@echo Making C-Kermit $(CKVER) for Sequent Dynix 3.0.x...
	$(MAKE) wermit "CFLAGS= -DBSD43 -DLCKDIR -DTCPSOCKET -O \
	-DUID_T=int -DGID_T=int $(KFLAGS)"

#Sequent Dynix 3.1.x
dynix31:
	@echo Making C-Kermit $(CKVER) for Sequent Dynix 3.1.x...
	$(MAKE) wermit "CFLAGS= -O -DDYNAMIC -DDCLPOPEN -DLCKDIR -DBSD4 \
	-DTCPSOCKET $(KFLAGS)"

#Encore, UMAX 4.3 (BSD) but without acucntrl program.
encore:
	$(MAKE) "MAKE=$(MAKE)" umax43 "KFLAGS=$(KFLAGS)"

#Encore, as above, but with curses file transfer display included.
encorec:
	$(MAKE) "MAKE=$(MAKE)" umax43 "KFLAGS=-DCK_CURSES $(KFLAGS)" \
	"LIBS= -lcurses -ltermcap"

#Encore, UMAX 4.3 (BSD) but without acucntrl program.
umax43:
	@echo Making C-Kermit $(CKVER) for Encore UMAX 4.3...
	$(MAKE) "MAKE=$(MAKE)" PARALLEL=4 wermit \
	"CFLAGS= -DBSD43 -DENCORE -DDYNAMIC -DKANJI -DTCPSOCKET $(KFLAGS) -O"

#Encore, UMAX 4.2 (BSD)
umax42:
	@echo Making C-Kermit $(CKVER) for Encore UMAX 4.2...
	$(MAKE) "MAKE=$(MAKE)" PARALLEL=4 wermit \
	"CFLAGS= -DBSD4 -DENCORE -DDYNAMIC -DTCPSOCKET $(KFLAGS) -O"

#Encore 88K UMAX 5.3 with TCP/IP support
encore88K:
	@echo 'Making C-Kermit $(CKVER) for Encore 88K UMAX V, TCP/IP...'
	$(MAKE) wermit "CFLAGS = -q ext=pcc -DSVR3 -DTCPSOCKET -DDIRENT \
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
	$(MAKE) ovwermit \
	"CFLAGS= -DBSD29 -DNODEBUG -DNOTLOG -DNOCSETS -DNOHELP \
	-DNOSCRIPT -DNOSPL -DNOXMIT -DNODIAL $(KFLAGS)" \
	"LNKFLAGS= -i -lndir" "CC= cc " "CC2= cc"

#Convex C1 with Berkeley Unix
convex:
	@echo Making C-Kermit $(CKVER) for Convex C1 / BSD...
	$(MAKE) wermit "CFLAGS= -DBSD4 $(KFLAGS) -Dmsleep=mnap"

#Convex C210 with Convex/OS 8
convex8:
	@echo Making C-Kermit $(CKVER) for Convex C210 with OS 8
	$(MAKE) wermit \
	"CFLAGS= -DBSD4 -DTCPSOCKET -DNODEBUG -DDIRENT -DNOFILEH \
	-DDYNAMIC $(KFLAGS) -DSIG_V -Dmsleep=mnap"

#Convex C2 with Convex OS 9.1 (should also work with 8.1 or later)
#with ANSI C compiler, uses BSD 4.3 uucp lockfile convention.
convex9:
	@echo Making C-Kermit $(CKVER) for Convex C210 with OS 9.1
	$(MAKE) wermit \
	"CFLAGS= -DPOSIX -DCONVEX9 -DNOIEXTEN -DDIRENT -DNOFILEH -DTCPSOCKET \
	-DDYNAMIC -D__STDC__ -DLCKDIR -Dmsleep=mnap -O -ext -tm c1 $(KFLAGS)" \
	"LNKFLAGS = -ext"

#Cray X-MP or Y-MP UNICOS System V R3
#Maybe the -i link option should be removed.
cray:
	@echo 'Making C-Kermit $(CKVER) for Cray X/Y-MP UNICOS System V R3...'
	$(MAKE) wermit NPROC=1 "CFLAGS= -DSVR3 -DDIRENT -DHDBUUCP $(KFLAGS) \
	-i -O" "LNKFLAGS = -i"

#Charles River Data Systems Universe with UNOS Version 9.2
crds:
	@echo 'Making C-Kermit $(CKVER) for Charles River Data Systems...'
	make wermit \
	"CFLAGS = -DATTSV -DNOANSI -DDIRENT -DDYNAMIC -DLONGFN -DTCPSOCKET \
	-DLOCK_DIR=\\\"/usr/spool/uucp\\\" -DNOSETREU \
	-Dsuspend=ksuspend $(KFLAGS) -O" "LNKFLAGS ="

#NeXT
#Uses shared library to make executable program about 80K smaller.
#Remove "LIBS = -lsys_s" if this causes trouble.
next:
	@echo Making C-Kermit $(CKVER) for NeXT...
	$(MAKE) wermit \
	"CFLAGS= -DNEXT -DNOSETBUF -DTCPSOCKET -DDYNAMIC -DLCKDIR \
	$(KFLAGS) -O -w" "LIBS = -lsys_s"

#NeXT
#As above, but with curses support.
nextc:
	@echo Making C-Kermit $(CKVER) for NeXT...
	$(MAKE) wermit \
	"CFLAGS= -DNEXT -DNOSETBUF -DTCPSOCKET -DDYNAMIC -DLCKDIR \
	-DCK_CURSES $(KFLAGS) -O -w" "LIBS = -lsys_s -lcurses -ltermcap"

#NeXT version built with C-Kermit malloc debugger.
nextmd:
	@echo Making C-Kermit $(CKVER) for NeXT with malloc debug...
	$(MAKE) mermit \
	"CFLAGS= -DNEXT -DTCPSOCKET -DDYNAMIC -DLCKDIR -DNOSETBUF -O -w \
	-Dmalloc=dmalloc -Dfree=dfree -DMDEBUG" "LIBS = -lsys_s"

#nextg - Gets useful error messages out of GNU cc.
# can't use -ansi here for some reason it clobbers definition of SIGQUIT (?)
nextg:
	@echo Making C-Kermit $(CKVER) for NeXT...
	$(MAKE) wermit "CFLAGS= -DNEXT -DTCPSOCKET -DDYNAMIC -DLCKDIR -DKANJI \
	-DNOSETBUF $(KFLAGS) -Wall -O" \
	"LIBS = -lsys_s"

#Same, but with curses
nextgc:
	@echo Making C-Kermit $(CKVER) for NeXT...
	$(MAKE) wermit "CFLAGS= -DNEXT -DTCPSOCKET -DDYNAMIC -DLCKDIR -DKANJI \
	-DNOSETBUF -DCK_CURSES $(KFLAGS) -Wall -O" \
	"LIBS = -lsys_s -lcurses -ltermcap"

#NeXT minimum interactive
nextmi:
	@echo Minimum interactive
	$(MAKE) "MAKE=$(MAKE)" nextg \
	"KFLAGS=-DNOSPL -DNOXMIT -DNOMSEND -DNOFRILLS \
	-DNODIAL -DNOHELP -DNODEBUG -DNOTLOG -DNOSCRIPT -DNOCSETS \
	-DNOSETKEY -DNOSETBUF -UTCPSOCKET" "LNKFLAGS = -s"

#POSIX
posix:
	@echo 'Making C-Kermit $(CKVER) for POSIX, no UUCP lockfile support...'
	$(MAKE) wermit "CFLAGS= -DPOSIX -DNOUUCP $(KFLAGS) -O"

#POSIX, BSDI-style.
bsdiposix:
	@echo 'Making C-Kermit $(CKVER) for BSDI/386 POSIX...'
	$(MAKE) wermit "CFLAGS= -DPOSIX -DSETREUID -DPIDSTRING -DUSLEEP \
	-DDYNAMIC -DNOSETBUF -DLOCK_DIR=\\\"/var/spool/uucp\\\" $(KFLAGS) -O"

# make 386bsd 0.0new, posix
# for  386bsd 0.1.24, change /usr/include/termios.h to #define NCCS if
#  _POSIX_SOURCE is #defined. (source: lewine, posix prgmrs guide, o`reilly)
386bsd:
	@echo 'Making C-Kermit $(CKVER) for jolix 386BSD 0.0new and 0.1.24...'
	$(MAKE) wermit "CFLAGS= -DPOSIX -DSETREUID -DPIDSTRING -DUSLEEP \
	-D_386BSD -DCK_CURSES -DTCPSOCKET -DDYNAMIC -DNOSETBUF \
	-DLOCK_DIR=\\\"/var/spool/uucp\\\" \
	$(KFLAGS) -O" "LNKFLAGS = -s" "LIBS = -lcurses -ltermcap"

#Pyramid 9810 OSx 4.4b-881028
pyramid:
	@echo Making C-Kermit $(CKVER) for Pyramid OSx 4.4b...
	$(MAKE) wermit "CFLAGS= -DBSD4 -DTCPSOCKET -DDYNAMIC -O $(KFLAGS)" \
	"LNKFLAGS = -s"

#SONY NEWS, NEWS-OS 4.01C
sonynews:
	@echo Making C-Kermit $(CKVER) for SONY NEWS-OS 4.01C...
	$(MAKE) wermit "CFLAGS= -DBSD43 -DKANJI -DACUCNTRL \
	-DTCPSOCKET -O"

#SUNPOSIX
sunposix:
	@echo Making C-Kermit $(CKVER) for POSIX...
	$(MAKE) wermit "CC= /usr/5bin/cc " \
	"CFLAGS= -DPOSIX -DHDBUUCP -DVOID=void -O"

#IBM's AIX 3.0 on IBM 370 mainframe, tested on AIX F44 thru F50.
aix370:
	@echo Making C-Kermit $(CKVER) for IBM System/370 AIX 3.0...
	$(MAKE) wermit \
	"CFLAGS= -DAIX370 -DTCPSOCKET -DLCKDIR -DDIRENT -DYNAMIC $(KFLAGS)" \
	"LIBS = -lbsd"

#IBM's AIX 1.3 on IBM PS/2, tested on AIX F44 thru F50.
#This is exactly the same as AIX370 except for the version herald.
ps2aix3:
	@echo Making C-Kermit $(CKVER) for IBM PS/2 AIX 3.0...
	$(MAKE) wermit \
	"CFLAGS= -DAIXPS2 -DTCPSOCKET -DLCKDIR -DDIRENT -DYNAMIC $(KFLAGS)" \
	"LIBS = -lbsd"

#IBM AIX 3.0, 3.1, or 3.2 for RISC System/6000.
rs6000:
	@echo Making C-Kermit $(CKVER) for IBM AIX 3.0 or 3.1, RS/6000...
	$(MAKE) wermit \
	"CFLAGS= -DAIXRS -DTCPSOCKET -DSVR3 -DDIRENT -DDYNAMIC -DCK_ANSIC \
	-DNOSETBUF -DCLSOPN -DKANJI -O $(KFLAGS)" "LNKFLAGS = -s"

#IBM AIX 3.0, 3.1, or 3.2 for RISC System/6000, with curses.
rs6000c:
	@echo Making C-Kermit $(CKVER) for IBM AIX 3.0 or 3.1, RS/6000...
	$(MAKE) wermit \
	"CFLAGS= -DAIXRS -DTCPSOCKET -DSVR3 -DDIRENT -DDYNAMIC -DCK_ANSIC \
	-DNOSETBUF -DCLSOPN -DCK_CURSES -DKANJI -O $(KFLAGS)" \
	"LIBS= -lcurses -ltermcap" "LNKFLAGS = -s"

#IBM AIX 3.2 for RISC System/6000.
rs6aix32:
	@echo Making C-Kermit $(CKVER) for IBM AIX 3.2, RS/6000...
	$(MAKE) wermit \
	"CFLAGS= -DAIXRS -DTCPSOCKET -DSVR4 -DDIRENT -DDYNAMIC -DCK_ANSIC \
	-DCLSOPN -DKANJI -O $(KFLAGS)" "LNKFLAGS = -s"

#IBM AIX 3.2 for RISC System/6000.
rs6aix32c:
	@echo Making C-Kermit $(CKVER) for IBM AIX 3.2, RS/6000, curses...
	$(MAKE) wermit \
	"CFLAGS= -DAIXRS -DTCPSOCKET -DSVR4 -DDIRENT -DDYNAMIC -DCK_ANSIC \
	-DCLSOPN -DCK_CURSES -DKANJI -O $(KFLAGS)" "LNKFLAGS = -s" \
	"LIBS=-lcurses"

#SUN OS version 4.0, BSD environment, has saved original euid feature.
# Add "CC=/usr/ucb/cc CC2=/usr/ucb/cc" if necessary.
sunos4:
	@echo Making C-Kermit $(CKVER) for SunOS 4.0, BSD environment...
	$(MAKE) wermit "CFLAGS= -O -DSUNOS4 -DDIRENT -DTCPSOCKET -DSAVEDUID \
	-DKANJI -DDYNAMIC $(KFLAGS)"

#As above, but with SunLink X.25 support
sunos4x25:
	@echo SunLink X.25 support
	$(MAKE) "MAKE=$(MAKE)" sunos4 "KFLAGS=$(KFLAGS) -DSUNX25"

#SUN OS version 4.1, BSD environment, has saved original euid feature.
#Uses Honey DanBer UUCP.  Requires presence of /usr/spool/locks directory.
# /var/spool/ should be a symbolic link to  /usr/spool/.
# ... or 'make wermit "CC= /usr/ucb/cc " \'
sunos41:
	@echo Making C-Kermit $(CKVER) for SunOS 4.1 / BSD...
	$(MAKE) wermit \
	"CFLAGS= -O -DSUNOS41 -DHDBUUCP -DDIRENT -DTCPSOCKET -DNOSETBUF \
	-DKANJI -DSAVEDUID -DDYNAMIC $(KFLAGS)"

#As above, but compile with gcc.  Reportedly gives 24-32K size reduction
#with gcc 2.1 or 2.2.2.  CAUTION: make sure "fixincludes" has been run on
#the include files, so gcc's are in sync with the regular SUN ones!
#Includes the curses library for fullscreen file transfer display.
sunos41gcc:
	@echo Making C-Kermit $(CKVER) for SunOS 4.1/BSD with gcc and curses...
	make wermit "CC= gcc " "CC2= gcc" \
	"CFLAGS= -O -DSUNOS41 -DHDBUUCP -DDIRENT -DTCPSOCKET -DNOSETBUF \
	-DSAVEDUID -DDYNAMIC -DKANJI -DCK_CURSES -DKANJI $(KFLAGS)" \
	"LIBS= -lcurses -ltermcap"

#SUNOS 4.1 as sunos41 above, but also with curses support
sunos41c:
	@echo Curses support
	$(MAKE) "MAKE=$(MAKE)" sunos41 "KFLAGS=$(KFLAGS) -DCK_CURSES" \
	"LIBS= -lcurses -ltermcap"

#SUNOS 4.1 with SunLink X.25 support
sunos41x25:
	@echo SunLink X.25 support
	$(MAKE) "MAKE=$(MAKE)" sunos41 "KFLAGS=$(KFLAGS) -DSUNX25"

#SUNOS 4.1 with SunLink X.25 support
sunos41x25c:
	@echo SunLink X.25 support, curses
	$(MAKE) "MAKE=$(MAKE)" sunos41c "KFLAGS=$(KFLAGS) -DSUNX25"

#The following sunosxxx entries are for testing only. 

#SUN OS version 4.0, BSD environment, minimum size...
sunos4m:
	@echo Minimum size
	$(MAKE) "MAKE=$(MAKE)" sunos4 \
	"KFLAGS=$(KFLAGS) -DNODIAL -DNOHELP -DNODEBUG -DNOTLOG \
	-DNOSCRIPT -DNOCSETS -DNOICP -DNOMSEND" "LNKFLAGS = -s"

#SUN OS version 4.1, BSD environment, minimum size...
sunos41m:
	@echo Minimum size
	$(MAKE) "MAKE=$(MAKE)" sunos41 \
	"KFLAGS=$(KFLAGS) -DNODIAL -DNOHELP -DNODEBUG -DNOTLOG \
	-DNOSCRIPT -DNOCSETS -DNOICP -DNOMSEND -UTCPSOCKET" "LNKFLAGS = -s"

#SUN OS version 4.0, BSD environment, minimum size w/command parser.
sunos4mi:
	@echo Minimum interactive
	$(MAKE) "MAKE=$(MAKE)" sunos4 \
	"KFLAGS=-DNOSPL -DNOXMIT -DNOMSEND -DNOFRILLS \
	-DNODIAL -DNOHELP -DNODEBUG -DNOTLOG -DNOSCRIPT -DNOCSETS \
	-DNOSETKEY -DNOSHOW -UTCPSOCKET $(KFLAGS)" "LNKFLAGS = -s"

#SUN OS version 4.1 or later, BSD environment, minimum size w/command parser.
sunos41mi:
	@echo Minimum interactive
	$(MAKE) "MAKE=$(MAKE)" sunos41 \
	"KFLAGS=-DNOSPL -DNOXMIT -DNOMSEND -DNOFRILLS \
	-DNODIAL -DNOHELP -DNODEBUG -DNOTLOG -DNOSCRIPT -DNOCSETS \
	-DNOSHOW -DNOSETKEY -DNOSERVER -DNOUUCP \
	-DNOSETBUF -DNOPUSH -DNOMDMHUP -DNOJC -DNOFDZERO -DNOESCSEQ \
	-DNOCMDL -UTCPSOCKET $(KFLAGS)" "LNKFLAGS = -s

#SUN OS version 4.1 or later, BSD, no debugging log.
sunos41nd:
	@echo No debugging...
	$(MAKE) "MAKE=$(MAKE)" sunos41 "KFLAGS=$(KFLAGS) -DNODEBUG"

#SUNOS 4.1 with malloc debugger
sunos41md:
	@echo Making C-Kermit $(CKVER) for SunOS 4.1 malloc debug...
	$(MAKE) mermit \
	"CFLAGS= -O -DSUNOS41 -DHDBUUCP -DDIRENT -DTCPSOCKET \
	-DSAVEDUID -DDYNAMIC $(KFLAGS) -Dmalloc=dmalloc -Dfree=dfree -DMDEBUG"

#SUN OS version 4.0, System V R3 environment (-i option omitted).
sunos4s5:
	@echo Making C-Kermit $(CKVER) for SunOS 4.0, System V R3...
	@echo For testing purposes only - NOT for production use.
	@echo For a useable version, make sunos4 instead.
	$(MAKE) wermit "CC= /usr/5bin/cc " \
	"CFLAGS = -DDIRENT -DSUN4S5 -DYNAMIC $(KFLAGS) -O"

#SUN OS version 4.1 or later, System V R3 environment (-i option omitted).
#Like sunos4s5, but SUNOS 4.1 has switched to HDB UUCP lockfile conventions.
sunos41s5:
	@echo Making C-Kermit $(CKVER) for SunOS 4.1 System V R3...
	@echo For testing purposes only - NOT for production use.
	@echo For a useable version, make sunos41 instead.
	$(MAKE) wermit "CC= /usr/5bin/cc " CC2=/usr/5bin/cc \
	"CFLAGS = -DSUN4S5 -DDIRENT -DHDBUUCP -DDYNAMIC $(KFLAGS) -O"

#As above, but with curses support
sunos41s5c:
	@echo Making C-Kermit $(CKVER) for SunOS 4.1 System V R3...
	@echo Curses included.
	@echo For testing purposes only - NOT for production use.
	@echo For a useable version, make sunos41 instead.
	$(MAKE) wermit "CC= /usr/5bin/cc " CC2=/usr/5bin/cc \
	"CFLAGS = -DSUN4S5 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DCK_CURSES -DCK_NEWTERM $(KFLAGS) -O" "LIBS= -lcurses"

#As above, but with curses support AND net support
sunos41s5tcpc:
	@echo Making C-Kermit $(CKVER) for SunOS 4.1 System V R3...
	@echo TCP/IP and curses included.  No debug log.
	@echo For testing purposes only - NOT for production use.
	@echo For a useable version, make sunos41 instead.
	$(MAKE) wermit "CC= /usr/5bin/cc " CC2=/usr/5bin/cc \
	"CFLAGS = -DSUN4S5 -DDIRENT -DHDBUUCP -DDYNAMIC -DNOSETBUF \
	-DNODEBUG -DCK_CURSES -DCK_NEWTERM -DTCPSOCKET $(KFLAGS) -O" \
	"LIBS= -lcurses -lresolv"

#SUN with Solaris 2.0.
#Mostly the same as System V R4.
#The nsl library contains a partab symbol, so Kermit's must be redefined.
sunsol20:
	@echo 'Making C-Kermit $(CKVER) for Sun with Solaris 2.0 and curses...'
	$(MAKE) wermit "CFLAGS = -O -DSVR4 -DSOLARIS -Dpartab=partbl \
	-DDIRENT -DHDBUUCP -DDYNAMIC -DSTERMIOX -DTCPSOCKET -DCK_CURSES \
	$(KFLAGS)" "LIBS= -lsocket -lnsl -lcurses -ltermlib" "LNKFLAGS = -s"

#SunOS 5.1 = Solaris 2.1, Alpha 3.2 = System V Release 4.0 for sparc processors
sunos51c:
	@echo 'Making C-Kermit $(CKVER) for SunOS 5.x....'
	$(MAKE) wermit "CFLAGS = -O -Usun -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DNOCSETS -DCK_CURSES -DNODEBUG -DSTERMIOX $(KFLAGS)" \
	"LNKFLAGS = -s" "LIBS= -ltermlib"

#SUN OS version 4.1 or later, gcc, profiling with gprof, no debugging.
#To get profile, "make sunos4p" (on SUN), then "./wermit".  After running
#wermit, "gprof ./wermit | lpr" (or whatever) to get execution profile.
sunos41p:
	@echo Making C-Kermit $(CKVER) for SunOS 4.x with profiling...
	$(MAKE) wermit "CC= gcc " "CC2= gcc" \
	"CFLAGS= -DSUNOS41 -DNODEBUG -DSAVEDUID -DDIRENT -DTCPSOCKET \
	-DDYNAMIC $(KFLAGS) -pg" "LNKFLAGS = -pg"

#End of sunosxxx test entries.

#Apollo with Domain SR10.0 or later, BSD environment
#Reportedly, it might also help to add '-A,systype=bsd4.3' to CFLAGS.
#Reportedly, there is also a problem with getc & putc macros that can
#be handled by using '#ifdef apollo' somewhere to redefine them???
#On the other hand, other reports indicate that it works fine as-is.
#NOTE: This entry was previously like this:
#	$(MAKE) wermit "CFLAGS= -DNOFILEH -DBSD4 $(KFLAGS) -Uaegis \
#	-DTCPSOCKET -U__STDC__"
#Reports (Dec 91) indicate SR10 has an ANSI-compliant C compiler,
#in addition to an older one that claimed to be ANSI-compliant but wasn't.
#The following make entry (plus checks that are made in ckcdeb.h) detect
#which compiler is used and define the CK_ANSIC or NOANSI flags accordingly.
sr10-bsd:
	@echo Making C-Kermit $(CKVER) for Apollo SR10.0 / BSD ...
	$(MAKE) wermit \
	"CFLAGS= -O -DBSD43 -DTCPSOCKET -DCK_CURSES -Uaegis $(KFLAGS)" \
	"LIBS= -lcurses -ltermcap"

#Apollo with Domain SR10.0 or later, System V R3 environment.
#Don't use the optimizer (-O), it causes problems at runtime.
sr10-s5r3:
	@echo Making C-Kermit $(CKVER) for Apollo SR10.0 / Sys V R3 ...
	$(MAKE) wermit "CFLAGS= -DNOFILEH -DSVR3 $(KFLAGS) -Uaegis -U__STDC__"

#Apollo Domain/IX (untested, try this if sr10-bsd doesn't work)
# (Can we add -DTCPSOCKET here?)
apollobsd:
	@echo Making C-Kermit $(CKVER) for Apollo Domain/IX...
	$(MAKE) wermit "CC= /bin/cc " "CC2= /bin/cc " \
	"CFLAGS= -DNOFILEH -DBSD4 $(KFLAGS) -Uaegis"

#Version 7 Unix (see comments above)
v7:
	@echo Making C-Kermit $(CKVER) for UNIX Version 7.
	@echo Read the makefile if you have trouble with this...
	$(MAKE) wermit "CFLAGS=-DV7 -DPROCNAME=\\\"$(PROC)\\\" \
	-DBOOTNAME=\\\"$(BOOTFILE)\\\" -DNPROCNAME=\\\"$(NPROC)\\\" \
	-DNPTYPE=$(NPTYPE) $(DIRECT) -DO_RDWR=2 -DO_NDELAY=0 -DO_SCCS_ID \
	$(KFLAGS)"

#AT&T UNIX System V R3, signal() is void rather than int.
#Uses dirent.h and Honey DanBer uucp.
#Add the -i link option if necessary.
sys5r3:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R3...'
	$(MAKE) wermit \
	"CFLAGS= -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC $(KFLAGS) -O" \
	"LNKFLAGS="

#AT&T UNIX System V R3, for 3B computers with Wollongong TCP/IP.
sys5r3net3b:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX SVR3/3B/Wollongong...'
	$(MAKE) wermit \
	"CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DWOLLONGONG $(KFLAGS) \
	-O" "LIBS= -lnet -lnsl_s" "LNKFLAGS ="

#AT&T UNIX System V R3, signal() is void rather than int.
#Uses dirent.h and Honey DanBer uucp, has <termiox.h>.
#Has <termiox.h> for RTS/CTS flow control.
sys5r3tx:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R3...'
	$(MAKE) wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DTERMIOX $(KFLAGS) -i -O" "LNKFLAGS ="

#AT&T UNIX System V R3, signal() is void rather than int.
#Uses dirent.h and Honey DanBer uucp, has <termiox.h>.
#Has <sys/termiox.h> for RTS/CTS flow control.
sys5r3sx:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R3...'
	$(MAKE) wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DSTERMIOX $(KFLAGS) -i -O" "LNKFLAGS ="

#AT&T UNIX System V R4.
#Has <termiox.h>.
sys5r4:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4...'
	$(MAKE) wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DTERMIOX $(KFLAGS)" "LNKFLAGS = -s"

#AT&T UNIX System V R4 with Wollongong TCP/IP.
#Has <termiox.h>.
sys5r4net:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4...'
	$(MAKE) wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DTERMIOX -DWOLLONGONG $(KFLAGS)" "LNKFLAGS = -s"

#DELL UNIX System V R4.
#Has <sys/termiox.h>, regular Berkeley sockets library, i.e. in.h and inet.h
#are not misplaced in sys (rather than netinet and arpa, respectively).
#Uses ANSI C constructs, advisory file locking on devices, etc.
#Warning: -DSTERMIOX enables hardware flow control (RTS/CTS), but reportedly
#this does not work with the normal drivers.  However, it might still work
#on non-Dell systems, or even Dell systems with different drivers installed.
dellsys5r4:
	@echo 'Making C-Kermit $(CKVER) for DELL UNIX System V R4...'
	$(MAKE) wermit "CFLAGS = -O -DSVR4 -DDELL_SVR4 -DDIRENT -DHDBUUCP \
	-DDYNAMIC -DTCPSOCKET -DSTERMIOX -DKANJI $(KFLAGS)" \
	"LIBS= -lsocket -lnsl" "LNKFLAGS = -s"

#As above, curses support added...
dellsys5r4c:
	@echo 'Making C-Kermit $(CKVER) for DELL UNIX System V R4...'
	$(MAKE) wermit "CFLAGS = -O -DSVR4 -DDELL_SVR4 -DDIRENT -DHDBUUCP \
	-DDYNAMIC -DTCPSOCKET -DSTERMIOX -DKANJI -DCK_CURSES $(KFLAGS)" \
	"LIBS= -lsocket -lnsl -lcurses -ltermcap" "LNKFLAGS = -s"

#Miminum interactive: As above, but with every conceivable option removed.
dellsys5r4mi:
	@echo 'Making C-Kermit $(CKVER) for DELL UNIX System V R4...'
	@echo 'Minimum-size interactive'
	$(MAKE) wermit "CFLAGS = -O -DSVR4 -DDELL_SVR4 -DDIRENT \
	-DDYNAMIC -UTCPSOCKET -DNOCMDL -DNOSPL -DNOXMIT \
	-DNOMSEND -DNOFRILLS -DNODIAL -DNOHELP -DNODEBUG -DNOTLOG \
	-DNOSCRIPT -DNOCSETS -DNOSHOW -DNOSETKEY -DNOSERVER -DNOUUCP \
	-DNOSETBUF -DNOPUSH -DNOMDMHUP -DNOJC -DNOFDZERO -DNOESCSEQ  \
	$(KFLAGS)" "LNKFLAGS = -s"

#Command-line only version.
dellsys5r4m:
	@echo 'Making C-Kermit $(CKVER) for DELL UNIX System V R4...'
	@echo 'Command-line only'
	$(MAKE) wermit "CFLAGS = -O -DSVR4 -DDELL_SVR4 -DDIRENT -DDYNAMIC \
	-UTCPSOCKET -DNOICP -DNOFRILLS -DNODIAL -DNODEBUG -DNOTLOG -DNOCSETS \
	-DNOSETKEY -DNOESCSEQ -DNOSETBUF -DNOJC -DNOFDZERO $(KFLAGS)" \
	"LNKFLAGS = -s"

#AT&T UNIX System V R4.
#Has <sys/termiox.h>.
sys5r4sx:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4...'
	$(MAKE) wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DSTERMIOX $(KFLAGS)" "LNKFLAGS = -s"

#AT&T UNIX System V R4.
#Has <sys/termiox.h>, regular Berkeley sockets library, i.e. in.h and inet.h
#are not misplaced in sys (rather than netinet and arpa, respectively).
#Uses ANSI C constructs, <sys/termiox.h>, etc etc. 
sys5r4sxtcp:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4...'
	$(MAKE) wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DSTERMIOX -DTCPSOCKET $(KFLAGS)" \
	"LIBS= -lsocket -lnsl" "LNKFLAGS = -s"

#Smallest possible version of above
s5r4m:
	@echo Minimum size
	$(MAKE) "MAKE=$(MAKE)" sys5r4sx \
	"KFLAGS=$(KFLAGS) -DNODIAL -DNOHELP -DNODEBUG -DNOTLOG \
	-DNOSCRIPT -DNOCSETS -DNOICP -DNOMSEND -UTCPSOCKET" "LNKFLAGS = -s"

#Smallest possible interactive version of above
s5r4mi:
	@echo Minimum interactive
	$(MAKE) "MAKE=$(MAKE)" sys5r4sx \
	"KFLAGS=-DNOSPL -DNOXMIT -DNOMSEND -DNOFRILLS -DNOSHOW \
	-DNODIAL -DNOHELP -DNODEBUG -DNOTLOG -DNOSCRIPT -DNOCSETS -DNOSETKEY \
	-UTCPSOCKET $(KFLAGS)" "LNKFLAGS = -s"

#AT&T UNIX System V R4, has <sys/termiox.h>
#ANSI C function prototyping disabled.
sys5r4sxna:
	@echo No ANSI C prototyping...
	$(MAKE) "MAKE=$(MAKE)" sys5r4sx "KFLAGS=$(KFLAGS) -DNOANSI"

#Commodore Amiga with AT&T UNIX System V R4 and TCP/IP support.
#Has <sys/termiox.h>.
svr4amiganet:
	@echo 'Making C-Kermit $(CKVER) for Amiga SVR4 + TCP/IP...'
	$(MAKE) wermit "CC=gcc" "CC2=gcc" \
	"CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC -DSTERMIOX \
	-DTCPSOCKET $(KFLAGS)" "LNKFLAGS = -s" "LIBS = -lsocket -lnsl"

#ESIX SVR4.0.3 with TCP/IP support.
#Has <sys/termiox.h>, ANSI C function prototyping disabled.
esixr4:
	@echo 'Making C-Kermit $(CKVER) for ESIX SVR4 + TCP/IP...'
	$(MAKE) wermit \
	"CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC -DNOANSI \
	-DSTERMIOX -DTCPSOCKET $(KFLAGS)" "LNKFLAGS = -s" \
	"LIBS = -lsocket -lnsl"

#AT&T UNIX System V R4.
#Has <sys/termiox.h>, Wollongong TCP/IP.
sys5r4sxnet:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4...'
	$(MAKE) wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DSTERMIOX -DWOLLONGONG $(KFLAGS)" "LNKFLAGS = -s"

#AT&T UNIX System V R4, no <termio.x> or <sys/termio.x>.
sys5r4nx:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4...'
	$(MAKE) wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	$(KFLAGS)" "LNKFLAGS = -s"

#AT&T UNIX System V R4, no <termio.x> or <sys/termio.x>, has Wollongong TCP/IP.
sys5r4nxnet:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System V R4...'
	$(MAKE) wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DWOLLONGONG $(KFLAGS)" "LNKFLAGS = -s"

#ICL UNIX System V R4.(DRS N/X) version :-
#UNIX System V Release 4.0 ICL DRS 6000
#DRS/NX 6000 SVR4 Version 5  Level 1  Increment 4
#Has <sys/termiox.h>, regular Berkeley sockets library, i.e. in.h and inet.h
#are not misplaced in sys (rather than netinet and arpa, respectively).
#Uses ANSI C constructs, advisory file locking on devices, etc.
iclsys5r4:
	@echo 'Making C-Kermit $(CKVER) for ICL UNIX System V R4 (DRS N/X)'
	$(MAKE) wermit "CFLAGS = -O -DSVR4 -DICL_SVR4 -DDIRENT -DHDBUUCP \
	-DDYNAMIC -DSTERMIOX -DTCPSOCKET $(KFLAGS)" \
	"LIBS= -lsocket " "LNKFLAGS = -s"

#Data General UX 4.30 (= System V R3 ??) for DG Aviion, with TCP/IP support.
dgux430:
	@echo 'Making C-Kermit $(CKVER) for DG Aviion UX 4.31...'
	$(MAKE) wermit "CFLAGS = -O -DDGUX430 -DSVR3 -DDIRENT -DDYNAMIC \
	-DTCPSOCKET $(KFLAGS)"

#Data General UX 4.30 for DG Aviion, with TCP/IP support with BSDisms.
dgux430bsd:
	@echo 'Making C-Kermit $(CKVER) for DG Aviion UX 4.31...'
	$(MAKE) wermit "CFLAGS = -O -DDGUX430 -D_BSD_SOURCE -DBSD4 \
	-DDYNAMIC -DTCPSOCKET $(KFLAGS)"

#Data General UX 5.40 (= System V R4) for DG Aviion, with TCP/IP support.
dgux540:
	@echo 'Making C-Kermit $(CKVER) for DG Aviion UX 5.40...'
	$(MAKE) wermit "CFLAGS = -O -DDGUX540 -DDIRENT -DHDBUUCP \
	-DDYNAMIC -DSTERMIOX -DTCPSOCKET $(KFLAGS)" "LNKFLAGS = -s"

#Data General UX 5.40 (= System V R4) for DG Aviion, with TCP/IP support.
# And curses.
dgux540c:
	@echo 'Making C-Kermit $(CKVER) for DG Aviion UX 5.40...'
	$(MAKE) wermit "CFLAGS = -O -DDGUX540 -DDIRENT -DHDBUUCP \
	-DDYNAMIC -DSTERMIOX -DTCPSOCKET -DCK_CURSES $(KFLAGS)" \
	"LIBS= -lcurses8 -ltermcap" "LNKFLAGS = -s"

#Silicon Graphics System V R3 with BSD file system (IRIS)
iris:
	@echo Making C-Kermit $(CKVER) for Silicon Graphics IRIX pre-3.3...
	$(MAKE) wermit "CFLAGS = -O -DSVR3 -DLONGFN  $(KFLAGS) \
	-I/usr/include/bsd" "LIBS = -lbsd"

#Silicon Graphics IRIS System V R3
irix33:
	@echo 'Making C-Kermit $(CKVER) for Silicon Graphics IRIX 3.3...'
	$(MAKE) wermit \
	"CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC $(KFLAGS) -O" \
	"LNKFLAGS ="

#Silicon Graphics Iris Indigo with IRIX 4.0.0
#Strict ANSI C compilation, TCP/IP support included
irix40:
	@echo 'Making C-Kermit $(CKVER) for Silicon Graphics IRIX 4.0...'
	$(MAKE) wermit \
	"CFLAGS = -DIRIX40 -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DPWID_T=uid_t \
	-DCK_ANSIC -DTCPSOCKET $(KFLAGS) -O -Olimit 1250 -I/usr/include/bsd"

#In case they type "make sys5"...
sys5:
	$(MAKE) "MAKE=$(MAKE)" sys3

#Generic ATT System III or System V (with I&D space)
sys3:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System III'
	@echo 'or System V R2 or earlier...'
	$(MAKE) wermit "CFLAGS = -DATTSV $(KFLAGS) -i -O" "LNKFLAGS = -i"

#Generic ATT System III or System V R2 or earlier, "no void":
#special entry to remove "Illegal pointer combination" warnings.
sys3nv:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System III'
	@echo 'or System V R2 or earlier...'
	$(MAKE) wermit "CFLAGS = -DATTSV -Dvoid=int  $(KFLAGS) -i -O" \
	"LNKFLAGS = -i"

#Generic ATT System III or System V (no I&D space)
sys3nid:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System III'
	@echo 'or System V R2 or earlier, no I&D space...'
	$(MAKE) wermit "CFLAGS = -DATTSV $(KFLAGS) -O" "LNKFLAGS ="

#AT&T 7300/Unix PC systems, sys3 but special handling for internal modem.
#Link with the shared library -- the conflict with openi in shared library
#is solved with -Dopeni=xopeni
sys3upc:
	@echo 'Making C-Kermit $(CKVER) for AT&T 7300 UNIX PC, shared lib...'
	@echo 'If shared lib causes trouble, use make sys3upcold.'
	$(MAKE) wermit "CFLAGS = -O -DATT7300 -DDYNAMIC -DNOSETBUF $(KFLAGS) \
	-Dopeni=xopeni" \
	"CC2 = ld /lib/crt0s.o /lib/shlib.ifile" "LNKFLAGS = -s"

#AT&T 7300/Unix PC systems, with curses support.
#Curses and the shared library don't get along, so we don't use the
#shared library.  We need to include CK_NEWTERM to avoid a conflict
#with curses and buffering on stdout.  Merged with submission by
#Robert Weiner/Programming Plus, rweiner@watsun.cc.columbia.edu.
#We don't need -Dopeni=xopeni since we're not using the shared library,
#but we keep it to be consistent with the other entries.
sys3upcc:
	@echo 'Making C-Kermit $(CKVER) for AT&T 7300 UNIX PC, curses...'
	$(MAKE) wermit "CFLAGS = -O -DATT7300 -DDYNAMIC -DNOSETBUF \
	-DCK_CURSES -DCK_NEWTERM $(KFLAGS) -Dopeni=xopeni" \
	"LIBS = -lcurses" "LNKFLAGS = -s"

#AT&T 7300/Unix PC systems, with curses and shared library support.
sys3upcshcc:
	@echo 'Making C-Kermit $(CKVER) for AT&T 7300 UNIX PC, shared lib...'
	@echo 'With curses.  Requires shcc.'
	$(MAKE) wermit "CFLAGS = -O -DATT7300 -DDYNAMIC $(KFLAGS) \
	-DNOSETBUF -DCK_NEWTERM -DCK_CURSES -Dopeni=xopeni" \
	"LNKFLAGS = -i -s" "CC = shcc" "CC2 = shcc" "LIBS = -lcurses"

#AT&T 7300/Unix PC systems, as above, no curses, but use gcc.
sys3upcg:
	@echo 'Making C-Kermit $(CKVER) for AT&T 7300 UNIX PC...'
	$(MAKE) wermit "CFLAGS = -O -DATT7300 -DDYNAMIC -DNOSETBUF $(KFLAGS) \
	-Dopeni=xopeni" \
	"CC = gcc" "CC2 = gcc" "LNKFLAGS = -s -shlib"

#AT&T 7300/Unix PC systems, curses and gcc.
sys3upcgc:
	@echo 'Making C-Kermit $(CKVER) for AT&T 7300 UNIX PC, curses...'
	$(MAKE) wermit "CFLAGS = -O -DATT7300 -DDYNAMIC -DNOSETBUF \
	-DCK_CURSES -DCK_NEWTERM" \
	"CC = gcc" "CC2 = gcc" "LIBS = -lcurses" "LNKFLAGS = -s"

#AT&T 7300/Unix PC systems, sys3 but special handling for internal modem.
sys3upcold:
	@echo 'Making C-Kermit $(CKVER) for AT&T 7300 UNIX PC...'
	$(MAKE) wermit "CFLAGS = -DATT7300 $(KFLAGS) -O" "LNKFLAGS = -i"

#AT&T 6300 PLUS (warning, -O makes it run out of space).
#NOTE: Remove -DHDBUUCP if not using Honey DanBer UUCP.
att6300:
	@echo 'Making C-Kermit $(CKVER) for AT&T 6300 PLUS...'
	$(MAKE) wermit "CFLAGS = -DATT6300 -DHDBUUCP -DNOFILEH \
	$(KFLAGS) -O -Ml -i" "LNKFLAGS = -i -Ml"

#As above, but with curses support.  Debugging disabled to prevent thrashing.
att6300c:
	@echo 'Making C-Kermit $(CKVER) for AT&T 6300 PLUS...'
	$(MAKE) wermit "CFLAGS = -DATT6300 -DHDBUUCP -DNOFILEH -DNOCSETS \
	-DCK_CURSES -DNODEBUG $(KFLAGS) -O -Ml -i" "LNKFLAGS = -i -Ml" \
	"LIBS = -lcurses"

#AT&T 6300 PLUS with no debugging (about 34K smaller)
# -Optimization saves about 20K too.
att6300nd:
	@echo 'Making C-Kermit $(CKVER) for AT&T 6300 PLUS, no debugging...'
	$(MAKE) wermit \
	"CFLAGS = -DATT6300 -DHDBUUCP -DNODEBUG -DNOFILEH $(KFLAGS) \
	-O -i -Ml" "LNKFLAGS = -i -Ml"

#AT&T 3B2, 3B20-series computers running AT&T UNIX System V.
#This is just generic System V with Honey DanBer UUCP, so refer to sys3hdb.
att3bx:
	$(MAKE) "MAKE=$(MAKE)" sys3hdb

#AT&T 3B2, 3B20-series computers running AT&T UNIX System V,
#with fullscreen file transfer display
att3bxc:
	@echo 'Making C-Kermit $(CKVER) for AT&T 3B2 or 3B20'
	@echo 'with Honey DanBer UUCP and curses...'
	$(MAKE) wermit "CFLAGS = -DATTSV -DHDBUUCP $(KFLAGS) \
		-DCK_CURSES -DCK_NEWTERM -i -O" \
	"LNKFLAGS = -i" "LIBS=-lcurses"

#Any System V R2 or earlier with Honey DanBer UUCP (same as above)
sys3hdb:
	@echo 'Making C-Kermit $(CKVER) for AT&T UNIX System III'
	@echo 'or System V R2 or earlier with Honey DanBer UUCP...'
	$(MAKE) wermit "CFLAGS = -DATTSV -DHDBUUCP $(KFLAGS) -i -O" \
	"LNKFLAGS = -i"

#Sperry/UNISYS 5000 UTS V 5.2 (System V R2), Honey DanBer UUCP
unisys5r2:
	@echo 'Making C-Kermit $(CKVER) for Sperry/UNISYS 5000 UTS V 5.2...'
	$(MAKE) wermit \
	"CFLAGS = -DATTSV -DUNISYS52 -DHDBUUCP $(KFLAGS) -i -O" \
	"LNKFLAGS = -i"

#In case they say "make sys5hdb" instead of "make sys3hdb"...
sys5hdb:
	$(MAKE) "MAKE=$(MAKE)" sys3hdb

#IBM PS/2 with AIX 1.0 (currently in field test as F10A)
#  Reports indicate that -O switch must be omitted.
#  It is also possible that "made bsd" will work (reports welcome).
#  One report recommended "make LIBS=-lbsd bsd" did the trick.
ps2aix:
	@echo 'Making C-Kermit $(CKVER) for IBM AIX 1.0 PS/2...'
	$(MAKE) wermit \
	"CFLAGS = -DATTSV -DPS2AIX10 $(KFLAGS) -i" "LNKFLAGS = -i"

#IBM RT PC with AIX 2.2.1
#This one has unique and strange lockfiles.
rtaix:
	@echo 'Making C-Kermit $(CKVER) for IBM RT PC, AIX 2.2.1...'
	$(MAKE) wermit \
	"CFLAGS = -DATTSV -DRTAIX -DHDBUUCP -DTCPSOCKET \
	-DCLSOPN $(KFLAGS) -O -w" "LNKFLAGS = -s"

#IBM RT PC with AIX 2.2.1 (BSD 4.3)
# Add -O, -DDYNAMIC, -s, etc, if they work.
rtacis:
	@echo Making C-Kermit $(CKVER) for RT PC with ACIS 2.2.1 = BSD 4.3...
	$(MAKE) wermit "CFLAGS= -DBSD4 -DTCPSOCKET $(KFLAGS) -U__STDC__" \
	"LNKFLAGS = -s"

#HP 9000 series 300, 500, 800, no long filenames and no job control.
#This is certainly only good for HPUX versions earlier than 6.5.
hpuxpre65:
	@echo 'Making C-Kermit $(CKVER) for HP-9000 HP-UX, no long filenames.'
	$(MAKE) wermit "CFLAGS = -DHPUX -DHPUXPRE65 $(KFLAGS) -O" "LNKFLAGS ="

#HP 9000 series 300, 500, 800, no long filenames.
#This is probably only good for HPUX versions earlier than 6.2.
hpux:
	@echo 'Making C-Kermit $(CKVER) for HP-9000 HP-UX, no long filenames.'
	$(MAKE) wermit "CFLAGS = -DHPUX $(KFLAGS) -O" "LNKFLAGS ="

#HP-9000 500 HP-UX 5.21 with Wollongong WIN/TCP 1.2 TCP/IP
#Requires /usr/wins/usr/include and /usr/lib/libnet.a from Wollongong
hpux500wintcp:
	@echo 'Making C-Kermit $(CKVER) for HP-9000 500 HP-UX 5.21 WIN/TCP' 
	$(MAKE) wermit "CFLAGS = -DHPUX -DHPUXPRE65 -DTCPSOCKET -DDYNAMIC
	-I/usr/wins/usr/include $(KFLAGS) -O" \
	"LIBS = /usr/lib/libnet.a" "LNKFLAGS = "

#HP-UX 7.0, no long filenames, no network support.
hpux7sf:
	@echo 'Making C-Kermit $(CKVER) for HP-9000 HP-UX, no long filenames.'
	$(MAKE) wermit "CFLAGS = -DHPUX $(KFLAGS) -DSIG_V -O" "LNKFLAGS ="

#HP 9000 series 300, 800, long filenames (using BSD file system)
# (This one is probably necessary for the Series 300)
hpuxlf:
	@echo 'Making C-Kermit $(CKVER) for HP-9000 HP-UX, long filenames...'
	$(MAKE) wermit \
	"CFLAGS = -DHPUX -DNDIR -DLONGFN -DYNAMIC $(KFLAGS) -O" \
	"LNKFLAGS ="

#HP 9000 series 300, 800, long filenames (using <dirent.h>)
hpuxde:
	@echo 'Making C-Kermit $(CKVER) for HP-9000 HP-UX, long filenames...'
	$(MAKE) wermit "CFLAGS = -DHPUX -DDIRENT -DDYNAMIC $(KFLAGS) -O" \
	"LNKFLAGS ="

#HP 9000 series 300, 800, long filenames (using <dirent.h>) and TCP/IP
hpuxdetcp:
	@echo 'Making C-Kermit $(CKVER) for HP-9000 HP-UX, long filenames...'
	$(MAKE) wermit "CFLAGS = -DHPUX -DDIRENT -DDYNAMIC -DTCPSOCKET \
	$(KFLAGS) -O" "LNKFLAGS ="

#HP 9000 series 300, 800, long filenames, System V R3 or later
# (Does anybody know what is the earliest release of HP-UX based on SVR3?)
hpuxs5r3:
	@echo 'Making C-Kermit $(CKVER) for HP-9000 HP-UX, long filenames...'
	$(MAKE) wermit \
	"CFLAGS = -DHPUX -DSVR3 -DDIRENT -DTCPSOCKET -DDYNAMIC \
	-DNOSETBUF $(KFLAGS) -O" "LNKFLAGS =" "LIBS=-lBSD"

#HP 9000 series 800 HP-UX 7.0, long filenames, network support, HDB uucp.
#there must be <arpa/telnet.h> & <arpa/inet.h> present to support this
#configuration.  To use this, you must have bought the ARPA Services
#Product from HP, and you must get the files "telnet.h" and "inet.h"
#from the Berkeley Standard Distribution because (reportedly) they are not
#part of the HP product.  And you need /usr/lib/libBSD.a.
#
hpux70lfn:
	@echo 'Making C-Kermit $(CKVER) for HP9000/8xx HP-UX V. 7.0'
	@echo 'supporting: long filenames, networking, HDB uucp...'
	$(MAKE) wermit "CFLAGS = -DHPUXDEBUG -DHPUX -DSVR3 -DDIRENT -DLONGFN \
	-DHDBUUCP -DDYNAMIC -DLOCK_DIR=\\\"/usr/spool/uucp\\\" -DNOSETBUF \
	-DTCPSOCKET  $(KFLAGS) -O" "LNKFLAGS = -s" "LIBS = -lBSD"

hpux70lfnc:
	@echo 'Making C-Kermit $(CKVER) for HP9000/8xx HP-UX V. 7.0'
	@echo 'supporting: curses, long filenames, networking, HDB uucp...'
	$(MAKE) wermit "CFLAGS = -DHPUXDEBUG -DHPUX -DSVR3 -DDIRENT -DLONGFN \
	-DHDBUUCP -DDYNAMIC -DLOCK_DIR=\\\"/usr/spool/uucp\\\" -DNOSETBUF \
	-DTCPSOCKET  -DCK_CURSES $(KFLAGS) -O" \
	"LIBS= -lcurses -ltermcap -lBSD" "LNKFLAGS = -s"

#HP 9000 Series 300 or 400, HP-UX 8.0, long filenames and TCP/IP support.
# +Obb optimization option not available.
hpux80:
	@echo 'Making C-Kermit $(CKVER) for HP9000/8xx HP-UX V. 8.0'
	@echo 'supporting: long filenames, networking, HDB uucp...'
	$(MAKE) -B wermit "CFLAGS = \
	-DHPUXDEBUG -DHPUX -DSVR3 -DDIRENT -DLONGFN \
	-DHDBUUCP -DDYNAMIC -DLOCK_DIR=\\\"/usr/spool/uucp\\\" -DNOSETBUF \
	-DTCPSOCKET  $(KFLAGS) -O" "LNKFLAGS = -s" "LIBS = -lBSD"

#HP 9000 Series 300 or 400, HP-UX 8.0, long filenames and TCP/IP support.
#Fullscreen file display support via curses library.
# +Obb optimization option not available.
hpux80c:
	@echo 'Making C-Kermit $(CKVER) for HP9000/8xx HP-UX V. 8.0'
	@echo 'supporting: long filenames, networking, HDB uucp...'
	$(MAKE) -B wermit "CFLAGS = -DCK_CURSES \
	-DHPUXDEBUG -DHPUX -DSVR3 -DDIRENT -DLONGFN \
	-DHDBUUCP -DDYNAMIC -DLOCK_DIR=\\\"/usr/spool/uucp\\\" -DNOSETBUF \
	-DTCPSOCKET  $(KFLAGS) -O" "LNKFLAGS = -s" "LIBS = -lBSD -lcurses"

#HP 9000 HP-UX 8.0, same as 7.0 with long filenames and TCP/IP support.
#HP 9000 Series 700 or 800, HP-UX 8.0, long filenames and TCP/IP support.
# +Obb optimization option available.
hpux80pa:
	@echo 'Making C-Kermit $(CKVER) for HP9000/8xx HP-UX V. 8.0'
	@echo 'supporting: long filenames, networking, HDB uucp...'
	$(MAKE) -B wermit "CFLAGS = +Obb700 \
	-DHPUXDEBUG -DHPUX -DSVR3 -DDIRENT -DLONGFN \
	-DHDBUUCP -DDYNAMIC -DLOCK_DIR=\\\"/usr/spool/uucp\\\" -DNOSETBUF \
	-DTCPSOCKET  $(KFLAGS) -O" "LNKFLAGS = -s" "LIBS = -lBSD"

#HP 9000 HP-UX 8.0, same as 7.0 with long filenames and TCP/IP support.
#HP 9000 Series 700 or 800, HP-UX 8.0, long filenames and TCP/IP support.
#Fullscreen file display support via curses library.
# +Obb optimization option available.
hpux80pac:
	@echo 'Making C-Kermit $(CKVER) for HP9000/8xx HP-UX V. 8.0'
	@echo 'supporting: long filenames, networking, HDB uucp...'
	$(MAKE) -B wermit "CFLAGS = +Obb700 -DCK_CURSES \
	-DHPUXDEBUG -DHPUX -DSVR3 -DDIRENT -DLONGFN \
	-DHDBUUCP -DDYNAMIC -DLOCK_DIR=\\\"/usr/spool/uucp\\\" -DNOSETBUF \
	-DTCPSOCKET  $(KFLAGS) -O" "LNKFLAGS = -s" "LIBS = -lBSD -lcurses"

#HP 9000 HP-UX 8.0, no TCP/IP because /usr/lib/libBSD.a can't be found,
#or TCP/IP header files missing.
hpux80notcp:
	$(MAKE) "MAKE=$(MAKE)" hpuxde

#Regulus on CIE Systems 680/20
cie:
	@echo 'Making C-Kermit $(CKVER) for CIE Systems 680/20 Regulus...'
	$(MAKE) wermit "CFLAGS = -DATTSV -DNOFILEH -DCIE $(KFLAGS) -O" \
	"LNKFLAGS ="

# Linux with the GCC 2.1 compiler.
linuxgcc2:
	@echo 'Making C-Kermit Linux with gcc2.1'
	$(MAKE) wermit "CC = gcc" "CFLAGS = -O -DPOSIX -DDIRENT -DDYNAMIC  \
	-DUSLEEP $(KFLAGS)" "CC2 = gcc" "LDFLAGS = -ltermcap "

# Linux with the GCC 2.1 compiler with TCP/IP
linuxgcc2net:
	@echo 'Making C-Kermit Linux with gcc2.1 and TCP/IP'
	$(MAKE) wermit "CC = gcc" "CFLAGS = -O -DPOSIX -DDIRENT -DDYNAMIC  \
	-DUSLEEP -DTCPSOCKET $(KFLAGS)" "CC2 = gcc" "LDFLAGS = -ltermcap "

#Microport SV/AT for IBM PC/AT 286 and clones, System V R2.
#The -O flag may fail on some modules (like ckuus2.c), in which case you 
#should compile them by hand, omitting the -O.  If you get "hash table
#overflow", try adding -DNODEBUG.
#Also, reportedly this compiles better with gcc than with cc.
mpsysv:
	@echo 'Making C-Kermit $(CKVER) for Microport SV/AT 286...'
	$(MAKE) wermit "CFLAGS= -DATTSV $(KFLAGS) -O -Ml" "LNKFLAGS = -Ml"

#Microsoft "Xenix/286" e.g. for IBM PC/AT
xenix:
	@echo 'Making C-Kermit $(CKVER) for Xenix/286'
	$(MAKE) wermit \
	"CFLAGS= -DXENIX -DNOFILEH $(KFLAGS) -Dunix -F 3000 -i" \
	"LNKFLAGS = -F 3000 -i"

#SCO Xenix 2.2.1 for IBM PC, XT, PS2/30, or other 8088 or 8086 machine
#If this doesn't work, try some of the tricks from sco286.
sco86:
	@echo 'Making C-Kermit $(CKVER) for SCO Xenix/86...'
	$(MAKE) wermit \
	"CFLAGS= -DXENIX -DNOFILEH $(KFLAGS) -Dunix -F 3000 -i -M0me" \
	"LNKFLAGS = -F 3000 -i -M0me" "LIBS = -lx"

#SCO Xenix/286 2.2.1, e.g. for IBM PC/AT, PS/2 Model 50, etc.
#Reportedly, this "make" can fail simply because of the size of this
#makefile.  If that happens, use "makeL", or edit out some of the
#other entries.
sco286:
	@echo 'Making C-Kermit $(CKVER) for SCO Xenix/286...'
	@echo 'If make fails, try using makeL.'
	$(MAKE) wermit \
	"CFLAGS= -s -O -LARGE -DXENIX -DNOFILEH -Dunix -DRDCHK -DNAP \
	$(KFLAGS) -F 3000 -i -M2let32" \
	"LIBS = -lx" "LNKFLAGS = -s -O -LARGE -F 3000 -i -M2let32"

#SCO Xenix/286 2.2.1, e.g. for IBM PC/AT, PS/2 Model 50, etc.
#As above, but with HDBUUCP
sco286hdb:
	@echo 'Making C-Kermit $(CKVER) for SCO Xenix/286 with HDB UUCP...'
	@echo 'If make fails, try using makeL.'
	$(MAKE) wermit \
	"CFLAGS= -s -O -LARGE -DXENIX -DNOFILEH -Dunix -DRDCHK -DNAP \
	-DHDBUUCP $(KFLAGS) -F 3000 -i -M2let32" \
	"LIBS = -lx" "LNKFLAGS = -s -O -LARGE -F 3000 -i -M2let32"

#SCO Xenix/386 2.2.2
sco386:
	@echo 'Making C-Kermit $(CKVER) for SCO Xenix/386...'
	$(MAKE) wermit \
	"CFLAGS= -DXENIX -DNOFILEH -Dunix -DRDCHK -DNAP $(KFLAGS) -Otcl -M3e" \
	"LIBS = -lx"

#SCO XENIX/386 2.3.3 with gcc 1.37 or later...
sco386gcc:
	@echo 'Making C-Kermit $(CKVER) for SCO Xenix/386 2.3.3, gcc...'
	$(MAKE) wermit "CC = gcc" "CC2 = gcc" \
	"CFLAGS= -O -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP \
	-DNOJC -DNODEBUG -DDYNAMIC $(KFLAGS) \
	-traditional -fpcc-struct-return -fstrength-reduce \
	-DM_BITFIELDS -DM_COFF -DM_I386 -DM_I86 -DM_I86SM \
	-DM_INTERNAT -DM_SDATA -DM_STEXT -DM_SYS3 -DM_SYS5 \
	-DM_SYSIII -DM_SYSV -DM_WORDSWAP -DM_XENIX \
	-DPWID_T=int " "LNKFLAGS = " "LIBS = -lx"

#NOTE: SCO UNIX entries need -lc in LIBS.  You can change this to -lc_s
#to use shared libraries to save memory.

#SCO UNIX/386 3.2.0, 3.2.1, and SCO Xenix 2.3.x
sco3r2:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	$(MAKE) wermit \
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
	$(MAKE) wermit \
	"CFLAGS= -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP -DNOJC \
	-DSCO32 $(KFLAGS) -Otcl" "LIBS = -lc -lx"

#SCO UNIX/386 3.2.0 and SCO Xenix 2.3.x with Excelan TCP/IP support.
#If this entry has compilation or runtime problems, try adding 
#"-DUID_T=int -DGID_T=int" to the CFLAGS.  If that doesn't work, try
#"-DUID_T=uid_t -DGID_T=gid_t".
sco3r2net:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 / Excelan...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	$(MAKE) wermit \
	"CFLAGS= -I/usr/include/exos -DXENIX -DSVR3 -DDYNAMIC -DNOFILEH \
	-DHDBUUCP -DRDCHK -DNAP -DTCPSOCKET -DEXCELAN -DNOJC $(KFLAGS) \
	-Otcl"
	"LIBS = -lc -lx -lsocket"

#SCO UNIX/386 3.2.0 and SCO Xenix 2.3.x with Excelan TCP/IP support.
#As above, with curses library added for FULLSCREEN file transfer display.
#See comments for sco3r2net.
sco3r2netc:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 / Excelan / curses...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	$(MAKE) wermit \
	"CFLAGS= -I/usr/include/exos -DXENIX -DSVR3 -DDYNAMIC -DNOFILEH \
	-DHDBUUCP -DRDCHK -DNAP -DTCPSOCKET -DEXCELAN -DNOJC $(KFLAGS) \
	-DCK_CURSES -Otcl"
	"LIBS = -lc -lx -lsocket -lcurses -ltermcap"

#SCO UNIX 3.2.x or SCO Xenix 2.3.x with Racal InterLan TCP/IP support
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
	$(MAKE) wermit \
	"CFLAGS= -I/usr/include/interlan -DXENIX -DNOFILEH -DHDBUUCP \
	-DSVR3 -DRDCHK -DNAP -DTCPSOCKET -DPARAMH -DINTERLAN -Di386 -DSYSV \
	-DNOJC $(KFLAGS) -Otcl -M3e" "LIBS = -lc -lx -ltcp"

#NOTE: SCO UNIX entries need -lc in LIBS.  You can change this to -lc_s
#to use shared libraries to save memory.

#ANOTHER NOTE: SCO UNIX provides different directory routines in -lc and -lx.
#Either one works as long as the corresponding correct header file is used.
#When the wrong header file is used, the compilation and linking proceed
#without complaint, but the directory-reading routines don't work correctly.
#For example, filenames might have their first few characters missing.

#SCO UNIX/386 3.2v2 (POSIX job control)
sco3r22:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 3.2v2 ...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	make wermit \
	"CFLAGS= -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP \
	-DPID_T=pid_t -DPWID_T=int $(KFLAGS) -Otcl" "LIBS = -lc -lx"

#SCO UNIX/386 3.2v2 with gcc 1.40 or later (POSIX job control)
sco3r22gcc:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 3.2v2, gcc'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	make wermit "CC = gcc" \
	"CFLAGS= -O -DPOSIX -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP \
	-traditional -fpcc-struct-return -fstrength-reduce \
	-DM_BITFIELDS -DM_COFF -DM_I386 -DM_I86 -DM_I86SM \
	-DM_INTERNAT -DM_SDATA -DM_STEXT -DM_SYS3 -DM_SYS5 \
	-DM_SYSIII -DM_SYSV -DM_UNIX -DM_WORDSWAP -DM_XENIX -Dunix \
	-DPID_T=pid_t -DPWID_T=int $(KFLAGS) " "LNKFLAGS = " "LIBS = -lx"

#SCO UNIX/386 3.2v2 (ODT 1.1) (POSIX job control) with SCO TCP/IP
sco3r22net:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 3.2.2 + TCP/IP...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	make wermit \
	"CFLAGS= -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP -DTCPSOCKET \
	-DPID_T=pid_t -DPWID_T=int $(KFLAGS) -Otcl" \
	"LNKFLAGS = " "LIBS = -lc -lx -lsocket -lc_s"

#As above, but with curses for fullscreen file transfer display.
#If you have problems at the link stage, try:
#"LIBS= -lx -lsocket -lcurses -ltermcap -lc_s".
#To save space in the executable, use "LINKFLAGS = -s".
sco3r22netc:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 3.2v2 + TCP/IP...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	make wermit \
	"CFLAGS= -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP -DTCPSOCKET \
	-DCK_CURSES -DPID_T=pid_t -DPWID_T=int $(KFLAGS) -Otcl" \
	"LNKFLAGS = " "LIBS = -lcurses -lc -lx -lsocket -lc_s"

#SCO UNIX/386 3.2v4 (POSIX job control)
sco32v4:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 3.2.4...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	make wermit \
	"CFLAGS= -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP -DNOANSI \
	-DPID_T=pid_t -DPWID_T=int $(KFLAGS) -Otcl" "LIBS = -lc -lx"

#SCO UNIX/386 3.2v4 with gcc 1.40 or later, POSIX job control
sco32v4gcc:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 3.2v4, gcc'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	make wermit "CC = gcc" \
	"CFLAGS= -O -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP -DNOANSI \
	-traditional -fpcc-struct-return -fstrength-reduce \
	-D_KR -D_NO_PROTOTYPE -D_SVID \
	-DM_BITFIELDS -DM_COFF -DM_I386 -DM_I86 -DM_I86SM \
	-DM_INTERNAT -DM_SDATA -DM_STEXT -DM_SYS3 -DM_SYS5 \
	-DM_SYSIII -DM_SYSV -DM_UNIX -DM_WORDSWAP -DM_XENIX -Dunix \
	-DPID_T=pid_t -DPWID_T=int $(KFLAGS) " "LNKFLAGS = " "LIBS = -lx"

#SCO UNIX/386 3.2v4 (POSIX job control) with SCO TCP/IP
sco32v4net:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 3.2v4 + TCP/IP...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	make wermit \
	"CFLAGS= -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK -DNAP -DTCPSOCKET \
	-DNOANSI $(KFLAGS) -DPID_T=pid_t -DPWID_T=int -Otcl" \
	"LNKFLAGS = " "LIBS = -lc -lx -lsocket -lc_s"

#SCO UNIX/386 3.2v4 (POSIX job control), TCP/IP, curses, ANSI C compilation, 
#<dirent.h> (EAFS) file system.  With DIRENT, -lc must come before -lx.
sco32v4netc:
	@echo 'Making C-Kermit $(CKVER) for SCO UNIX/386 3.2v4...'
	$(MAKE) wermit \
	"CFLAGS= -DXENIX -DSVR3 -DDIRENT -DNOFILEH -DHDBUUCP -DRDCHK -DNAP \
	-DTCPSOCKET -DCK_ANSIC -DCK_CURSES $(KFLAGS) \
	-DPID_T=pid_t -DPWID_T=int -Otcl" \
	"LNKFLAGS = -s" "LIBS = -lc -lx -lsocket -lcurses -ltermcap"

# SCO XENIX/386 2.3.3 SysV with SCO TCP/IP
# System V STREAMS TCP developed by Lachman Associates and
# Convergent Technologies.
sco3r2lai:
	@echo 'Making C-Kermit $(CKVER) for SCO XENIX/386 2.3.3 + TCP/IP...'
	@echo 'Warning: If make blows up, edit the makefile to join'
	@echo 'the following three continued lines into one line.'
	$(MAKE) wermit \
	"CFLAGS= -DLAI_TCP -Di386 -DXENIX -DSVR3 -DNOFILEH -DHDBUUCP -DRDCHK \
	-DNAP -DTCPSOCKET -DPWID_T=int $(KFLAGS) -Otcl -i -M3e" \
	"LNKFLAGS = -i" "LIBS = -lx -lsocket"

#PC/IX, Interactive Corp System III for IBM PC/XT
pcix:
	@echo 'Making C-Kermit $(CKVER) for PC/IX...'
	$(MAKE) wermit \
	"CFLAGS= -DPCIX -DISIII $(KFLAGS) -Dsdata=sdatax -O -i" "LNKFLAGS = -i"

#Interactive Corp System III port in general --
is3:
	@echo 'Making C-Kermit $(CKVER) for Interactive System III...'
	$(MAKE) wermit \
	"CFLAGS= -DISIII $(KFLAGS) -Ddata=datax -O -i" "LNKFLAGS= -i"

#Interactive UNIX System V R3, no network support.  Uses <dirent.h> and Honey
#DanBer UUCP.  If this entry does not compile correctly, try any or all of the
#following.  These suggestions also apply more or less to the other is5r3xxx 
#entries that follow this one.
# . Remove the UID_T and GID_T definitions, or change them as required.
# . Change -DDIRENT to -DSDIRENT.
# . Add -DSIGTYP=void.
# . Remove -g from LNKFLAGS.
# . Add -DNOANSI to remove compiler complaints about ANSI C constructions
# . Add other -DNOxxx's to save space (e.g. -DNOCSETS)
# See the next few makefile entries for related examples.
is5r3:
	@echo 'Making C-Kermit $(CKVER) for Interactive 386/ix or later...'
	@echo 'If this does not work please read the makefile entry.'
	$(MAKE) wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -g -DNOCSETS \
	-DUID_T=ushort -DGID_T=ushort -DDYNAMIC -DI386IX $(KFLAGS)" \
	"LNKFLAGS = -g"

#Interactive UNIX System V R3, POSIX variant.  Untested.
#Uses dirent.h and Honey DanBer uucp.  Read comments in is5r3 entry.
is5r3p:
	@echo 'Making C-Kermit $(CKVER) for Interactive 386/ix or later...'
	$(MAKE) wermit "CFLAGS= -DSVR3 -DDIRENT -DHDBUUCP -g -DNOCSETS \
	-DDYNAMIC -DI386IX -DPOSIX $(KFLAGS)" "LNKFLAGS=" "LIBS=-lcposix"

#Interactive UNIX System V R3 with TCP/IP network support.
#Needs -linet for net functions.  signal() is void rather than int.
#Uses dirent.h and Honey DanBer uucp. Read comments in is5r3 entry.
#Also see is5r3net2 if you have trouble with this entry.
is5r3net:
	@echo 'Making C-Kermit $(CKVER) for Interactive 386/ix...'
	@echo 'If this does not work please read the makefile entry.'
	$(MAKE) wermit \
	"CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DTCPSOCKET \
	-DI386IX $(KFLAGS) -O" "LIBS = -linet"

#Interactive UNIX System V R3, no job control, signal() void rather than int.
#Uses dirent.h and Honey DanBer uucp.  Needs -linet for net functions.
#Read comments in is5r3 entry.  Use this entry if is5r3net fails.
#Saves some space by stripping (-s) and using shared library (-lc_s).
is5r3net2:
	@echo 'Making C-Kermit $(CKVER) for Interactive 386/ix...'
	$(MAKE) wermit \
	"CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DTCPSOCKET -DNOJC \
	-DSIGTYP=void -DNOANSI -DI386IX $(KFLAGS) -O" \
	"LNKFLAGS= -s" "LIBS = -linet -lc_s"

#Interactive UNIX System V R3 (version 2.2 or later) with job control & curses.
#Uses dirent.h and Honey DanBer UUCP.
is5r3jc:
	@echo 'Making C-Kermit $(CKVER) for Interactive Unix 2.2 or later...'
	$(MAKE) wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -O -DNOCSETS \
	-DUID_T=ushort -DGID_T=ushort -DDYNAMIC -DI386IX -DSVR3JC -DCK_CURSES \
	$(KFLAGS)" "LNKFLAGS = -s" "LIBS=-lcurses -lc_s"

#Masscomp System III
rtu:
	@echo 'Making C-Kermit $(CKVER) for Masscomp RTU System III...'
	$(MAKE) wermit \
	"CFLAGS= -UFIONREAD -DATTSV $(KFLAGS) -O" "LNKFLAGS =" "LIBS= -ljobs"

#Masscomp/Concurrent RTU 4.0 or later, Berkeley environment.
#Includes <ndir.h> = /usr/include/ndir.h
#Note "LIBS = -lndir" might not be necessary because of "ucb make".
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
#Note "LIBS = -lndir" might not be necessary because of "ucb make".
rtubsd3:
	@echo 'Making C-Kermit $(CKVER) for Masscomp RTU 4.x BSD...'
	ucb make wermit "CFLAGS= -DBSD4 -DRTU -DHDBUUCP $(KFLAGS)" \
	"LIBS = -lndir"

#Masscomp/Concurrent RTU 4.0 or later, System V R2, using <dirent.h>.
#If this gives problems, add back the -DRTU switch.
#If the -DTCPSOCKET gives trouble, remove it.
rtus5:
	@echo 'Making C-Kermit $(CKVER) for Masscomp RTU 4.x...'
	$(MAKE) wermit \
	"CFLAGS= -DATTSV -DHDBUUCP -DDIRENT -DTCPSOCKET -DDYNAMIC \
	-DNOSETBUF $(KFLAGS)"

#Masscomp/Concurrent RTU 4.x, System V R3, using <dirent.h>.
#Use this one if rtus5 gives warnings about pointer type mismatches.
#If this gives problems, add back the -DRTU switch.
rtus5r3:
	@echo 'Making C-Kermit $(CKVER) for Masscomp RTU Sys V R3...'
	$(MAKE) wermit "CFLAGS= -DSVR3 -DHDBUUCP -DDIRENT $(KFLAGS)"

#DEC Pro-3xx with Pro/Venix V1.0 or V1.1
# Requires code-mapping on non-I&D-space 11/23 processor, plus some
# fiddling to get interrupt targets into resident code section.
# This almost certainly doesn't work any more.
provx1:
	@echo 'Making C-Kermit $(CKVER) for DEC Pro-3xx, Pro/Venix 1.x...'
	$(MAKE) wart "CFLAGS= -DPROVX1 $(KFLAGS)" "LNKFLAGS= "
	$(MAKE) wermit "CFLAGS = -DPROVX1 -DNOFILEH -md780" \
		"LNKFLAGS= -u _sleep -lc -md780"

#NCR Tower 1632, OS 1.02
tower1:
	@echo 'Making C-Kermit $(CKVER) for NCR Tower 1632, OS 1.02...'
	$(MAKE) wermit "CFLAGS= -DTOWER1 $(KFLAGS)"

#NCR Tower 32, OS Release 1.xx.xx
tower32-1:
	@echo 'Making C-Kermit $(CKVER) for NCR Tower 32 Rel 1 System V R2...'
	$(MAKE) wermit \
	"CFLAGS = -DATTSV -DDYNAMIC $(KFLAGS) -O" "LNKFLAGS = -n"

#NCR Tower 32, OS Release 2.xx.xx
tower32-2:
	@echo 'Making C-Kermit $(CKVER) for NCR Tower 32 Rel 2 System V R2...'
	$(MAKE) wermit "CFLAGS = -DATTSV -DHDBUUCP -DDYNAMIC $(KFLAGS) -O2" \
	"LNKFLAGS = -n"

#NCR Tower 32, OS Releases based on System V R3
#Don't add -DNAP (doesn't work right) or -DRDCHK (not available in libc).
tower32:
	@echo 'Making C-Kermit $(CKVER) for NCR Tower 32 System V R3...'
	$(MAKE) wermit \
	"CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DNOSYSIOCTLH $(KFLAGS) \
	-DUID_T=ushort -DGID_T=ushort -O1"

#NCR Tower 32, OS Releases based on System V R3
tower32g:
	@echo 'Making C-Kermit $(CKVER) for NCR Tower 32 System V R3, gcc...'
	$(MAKE) wermit "CC = gcc" \
	"CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DNOSYSIOCTLH $(KFLAGS) \
	DUID_T=ushort -DGID_T=ushort -O -fstrength-reduce -fomit-frame-pointer"

#Fortune 32:16, For:Pro 1.8 (mostly like 4.1bsd)
ft18:
	@echo 'Making C-Kermit $(CKVER) for Fortune 32:16 For:Pro 1.8...'
	$(MAKE) wermit "CFLAGS= -DNODEBUG -DBSD4 -DFT18 -DNOFILEH $(KFLAGS) \
	-DPID_T=short"

#Fortune 32:16, For:Pro 2.1 (mostly like 4.1bsd)
ft21:
	@echo 'Making C-Kermit $(CKVER) for Fortune 32:16 For:Pro 2.1...'
	$(MAKE) wermit "CFLAGS= -O -DNODEBUG -DBSD4 -DFT21 -DNOFILEH -SYM 800 \
	-DDYNAMIC -DNOSETBUF -DCK_CURSES $(KFLAGS) -DPID_T=short" \
	"LNKFLAGS= -n -s" "LIBS= -lcurses -ltermcap -lv -lnet"

#Valid Scaldstar
#Berkeleyish, but need to change some variable names.
valid:
	@echo 'Making C-Kermit $(CKVER) for Valid Scaldstar...'
	$(MAKE) wermit \
	"CFLAGS= -DBSD4 -DNODEBUG -DNOTLOG -Dcc=ccx -DFREAD=1 $(KFLAGS)"

#IBM IX/370 on IBM 370 Series mainframes
#Mostly like sys3, but should buffer packets.
ix370:
	@echo 'Making C-Kermit $(CKVER) for IBM IX/370...'
	$(MAKE) wermit "CFLAGS = -DIX370 -DATTSV $(KFLAGS) -i -O" \
	"LNKFLAGS = -i"

#Amdahl UTS 2.4 on IBM 370 series compatible mainframes.
#Mostly like V7, but can't do initrawq() buffer peeking.
uts24:
	@echo 'Making C-Kermit $(CKVER) for Amdahl UTS 2.4...'
	$(MAKE) wermit "CFLAGS=-DV7 -DPROCNAME=\\\"$(PROC)\\\" \
	-DUTS24 -DBOOTNAME=\\\"$(BOOTFILE)\\\" -DNPROCNAME=\\\"$(NPROC)\\\" \
	-DNPTYPE=$(NPTYPE) $(DIRECT) $(KFLAGS)"

#Amdahl UTSV UNIX System V = System V R2 or earlier.
utsv:
	@echo 'Making C-Kermit $(CKVER) for Amdahl UTSV...'
	$(MAKE) wermit "CFLAGS = -DUTSV $(KFLAGS) -i -O" "LNKFLAGS = -i"

#BBN C/70 with IOS 2.0
#Mostly Berkeley-like, but with some ATTisms
c70:
	@echo 'Making C-Kermit $(CKVER) for BBN C/70 IOS 2.0...'
	$(MAKE) wermit "CFLAGS= -DBSD4 -DC70 $(KFLAGS)"

#Zilog ZEUS 3.21
zilog:
	@echo 'Making C-Kermit $(CKVER) for Zilog Zeus 3.21...'
	$(MAKE) wermit "CFLAGS = -DATTSV -DZILOG -DNODEBUG $(KFLAGS) -i -O" \
	"LNKFLAGS = -i -lpw"

#Whitechapel MG-1 Genix 1.3
white:
	@echo 'Making C-Kermit $(CKVER) for Whitechapel MG-1 Genix 1.3...'
	@touch ckcpro.c
	$(MAKE) wermit "CFLAGS= -DBSD4 -Dzkself()=0  $(KFLAGS)"

#Pixel 1000
pixel:
	@echo 'Making C-Kermit $(CKVER) for Pixel 1000...'
	$(MAKE) wermit "CFLAGS= -DBSD4 -Dzkself()=0 $(KFLAGS)"

ptx:
	$(MAKE) "MAKE=$(MAKE)" dynix12

#CDC VX/VE 5.2.1
vxve:
	@echo 'Making C-Kermit $(CKVER) for CDC VX/VE 5.2.1...'
	$(MAKE) wermit \
	"CFLAGS = -DATTSV -DVXVE -DNODEBUG -DNOTLOG $(KFLAGS) -i -O" \
	"LNKFLAGS = -i"

#Tandy 16/6000 with Xenix 3.0
#Add -DNOxxx options to remove features if program won't load.
trs16:
	@echo 'Making C-Kermit $(CKVER) for Tandy 16/16000, Xenix 3.0...'
	$(MAKE) wermit "CFLAGS = -DATTSV -DTRS16 -DDCLPOPEN $(KFLAGS) -O" \
	"LNKFLAGS = -n -s"

#DIAB DS90 or LUXOR ABC-9000 with pre-5.2 DNIX.  Sys V with nap() and rdchk().
# nd = no opendir(), readdir(), closedir(), etc.
# Some of the modules fail to compile with -O.
dnixnd:
	@echo 'Making C-Kermit $(CKVER) for DIAB DS90 with very old DNIX 5.2.'
	$(MAKE) wermit "CFLAGS = -DATTSV -DNAP -DRDCHK -DDYNAMIC -DDCLPOPEN \
	-U__STDC__ $(KFLAGS)"

#DIAB DS90 with DNIX 5.2.  Sys V with nap() and rdchk().
# This one has opendir(), readdir(), closedir(), etc.
# Some of the modules fail to compile with -O.
dnix:
	@echo 'Making C-Kermit $(CKVER) for DIAB DS90 with old DNIX 5.2...'
	$(MAKE) wermit "CFLAGS = -DATTSV -DNAP -DRDCHK -DDIRENT -DDYNAMIC  \
	-U__STDC__ $(KFLAGS)"

#DIAB DS90 with DNIX 5.3 or later, with HDB UUCP, nap() and rdchk().
dnix5r3:
	@echo 'Making C-Kermit $(CKVER) for DIAB DS90 with DNIX 5.3...'
	@echo 'with Honey DanBer UUCP'
	$(MAKE) wermit "CFLAGS = -DSVR3 -DHDBUUCP -DNAP -DRDCHK -DDIRENT \
	-DDYNAMIC -DCK_CURSES $(KFLAGS) -O" "LIBS= -lcurses"

#DIAB DS90 with DNIX 5.3 or later, with HDB UUCP, nap() and rdchk() + TCP/IP
dnix5r3net:
	@echo 'Making C-Kermit $(CKVER) for DIAB DS90 with DNIX 5.3...'
	@echo 'with Honey DanBer UUCP and TCP/IP'
	$(MAKE) wermit "CFLAGS = -DSVR3 -DHDBUUCP -DNAP -DRDCHK -DDIRENT \
	-DTCPSOCKET -DDYNAMIC -DCK_CURSES $(KFLAGS) -O -I/usr/include/bsd" \
	"LIBS = -ln -lcurses"

#DIAB DS90 with DNIX 5.3 2.2 or later, with HDB UUCP, nap() and rdchk(),
#ANSI C compilation and libraries.
#Note that for DNIX 5.3 2.2 you have to correct a bug in /usr/include/stdlib.h:
#change "extern	void free(char *str);"
#to     "extern void free(void *str);"
#
dnix5r3ansi:
	@echo 'Making C-Kermit $(CKVER) for DIAB DS90 with DNIX 5.3...'
	@echo 'with ANSI C Honey DanBer UUCP'
	$(MAKE) wermit \
	"CFLAGS = -DSVR3 -DDIAB -DHDBUUCP -DNAP -DRDCHK -DDIRENT -DDYNAMIC \
	-DCK_ANSILIBS -DCK_CURSES -O -X7 -X9 $(KFLAGS)" "LIBS= -lcurses"

#Ridge 32 with ROS 3.2
ridge32:
	@echo 'Making C-Kermit $(CKVER) Ridge 32 ROS 3.2'
	$(MAKE) wermit \
	"CFLAGS = -DATTSV -DNOFILEH -DNODEBUG -DNOTLOG $(KFLAGS) -i -O" \
	"LNKFLAGS = -i"

#Altos 486, 586, or 986 with Xenix 3.0
altos:
	@echo 'Making C-Kermit $(CKVER) for Altos x86 with Xenix 3.0...'
	$(MAKE) wermit \
	"CFLAGS= -DATTSV -DA986 -DNODEBUG -DNOTLOG $(KFLAGS) -i -O" \
	"LNKFLAGS= -i"

#Altos 986 with Xenix 3.0, as above, but command-line only, minimal size.
#For systems with small memories.  It might also be necessary to chop certain
#modules up into smaller pieces, e.g. ckuus3-6, because of symbol table
#overflow.   If this makefile is too big or complex for the Altos, compile
#and link by hand or write shell scripts.
altosc:
	@echo 'Making C-Kermit $(CKVER) for Altos x86 Xenix 3.0, remote...'
	$(MAKE) wermit \
	"CFLAGS= -DATTSV -DA986 -DNODEBUG -DNOTLOG -DNOSCRIPT -DNODIAL \
	-DNOCSETS -DNOANSI -DNOMSEND -DNOSPL -DNOICP $(KFLAGS) -Mm -O" \
	"LNKFLAGS= -Mm -s"

#Altos 986 with Xenix 3.0, as above, but interactive only, minimal size.
altosi:
	@echo 'Making C-Kermit $(CKVER) for Altos x86 Xenix 3.0, local...'
	$(MAKE) wermit \
	"CFLAGS= -DATTSV -DA986 -DNODEBUG -DNOTLOG -DNOSCRIPT -DNODIAL \
	-DNOCSETS -DNOANSI -DNOMSEND -DNOSPL -DNOCMDL -DNOFRILLS -DNOHELP \
	-DNOSETKEY $(KFLAGS) -Mm -O" "LNKFLAGS= -Mm -s"

# Altos ACS68000 68000 System, UNIX System 3 Release 2, 512k memory.
# also needs getcwd() external function; see ckuins.doc file.
# also, sys/types.h needed modifying:
#   #ifdef __SYS_TYPES_H__, #define ..., #endif
# also, ckuus2.c MUST be compiled NOOPT else symbol table is destroyed!
# Submission by Robert Weiner/Programming Plus, rweiner@watsun.cc.columbia.edu
#
altos3:
	@echo 'Making C-Kermit $(CKVER) for Altos ACS68k UNIX System III'
	$(MAKE) ckuus2.$(EXT) "CFLAGS = -DATTSV -DNOCSETS -DNOSETKEY -DNOJC \
		-DNODIAL -DDCLPOPEN $(KFLAGS) -i"
	$(MAKE) wermit   "CFLAGS = -DATTSV -DNOCSETS -DNOSETKEY -DNOJC \
		-DNODIAL -DDCLPOPEN $(KFLAGS) -i -O" \
		"LNKFLAGS = -i" "LIBS = getcwd.$(EXT)"

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
	$(MAKE) wermit EXT=s \
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
	$(MAKE) wermit \
	"CFLAGS= -DV7 -DMINIX -i -D_MINIX -D_POSIX_SOURCE -DDYNAMIC \
	-DPID_T=pid_t -DUID_T=uid_t -DGID_T=gid_t -DSIG_V -DNODIAL \
	-DNOHELP -DNODEBUG -DNOTLOG -DNOSCRIPT -DNOCSETS -DNOICP $(KFLAGS)" \
	"LNKFLAGS= -i -T"

#MINIX/386 (PC Minix modifed by Bruce Evans in Australia to use 386 addressing)
minix386:
	@echo 'Making C-Kermit $(CKVER) for MINIX/386...'
	@echo 'TOTALLY UNTESTED!'
	$(MAKE) wermit EXT=s \
	"CFLAGS= -DV7 -DMINIX -D_POSIX_SOURCE -DDYNAMIC $(KFLAGS)"

#MINIX/386 (PC Minix modifed by Bruce Evans in Australia to use 386 addressing)
minix386gcc:
	@echo 'Making C-Kermit $(CKVER) for MINIX/386 with gcc...'
	@echo 'TOTALLY UNTESTED!'
	$(MAKE) wermit "CC=gcc -g -O" "CC2=gcc -g" \
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
	$(MAKE) wermit \
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
	$(MAKE) wermit "CC= cc -c68" \
	"CFLAGS= -DV7 -DMINIX -D_MINIX -D_POSIX_SOURCE -DDYNAMIC \
	-DNODIAL -DNOHELP -DNODEBUG -DNOTLOG \
	-DNOSCRIPT -DNOCSETS -DNOSPL $(KFLAGS) \
	-DPID_T=pid_t -DUID_T=uid_t -DGID_T=gid_t -DSIG_V"

#MIPS Computer Systems with UMIPS RISC/OS AT&T UNIX System V R3.0.
#Add -DNOJC if UNIX job control causes trouble.
mips:
	@echo 'Making C-Kermit $(CKVER) for MIPS AT&T System V R3.0...'
	$(MAKE) wermit "CFLAGS = -DMIPS -DDIRENT -DDYNAMIC -DPID_T=int \
	$(KFLAGS) -DGID_T=gid_t -DUID_T=uid_t -i -O1500"

#Motorola Delta System V/68 R3, signal() is void rather than int.
#Uses dirent.h and Honey DanBer uucp.
#Supports TCPSOCKET
sv68r3:
	@echo 'Making C-Kermit $(CKVER) for Motorola UNIX System V/68 R3...'
	$(MAKE) wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DTCPSOCKET $(KFLAGS) \
	-O" "LNKFLAGS ="

#Motorola Delta System V/68 R3V5, signal() is void rather than int.
#Uses dirent.h and Honey DanBer UUCP.
#Supports TCPSOCKET
sv68r3v5:
	@echo 'Making C-Kermit $(CKVER) for Motorola UNIX System V/68 R3V5'
	$(MAKE) wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DTCPSOCKET -DINADDRX $(KFLAGS) -O" "LNKFLAGS =" "LIBS = -linet"

#Motorola MVME147 System V/68 R3 V5.1 
#Requires gcc 2.1 to compile.
sv68r3v51:
	@echo 'Making C-Kermit $(CKVER) for Motorola UNIX System V/68 R3V5.1'
	$(MAKE) wermit "CC=gcc-delta" "CC2=gcc-delta" \
	"CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC -DNODEBUG -DNOSETBUF \
	$(KFLAGS) -O2 -v -ftraditional" "LNKFLAGS = -s -v" "LIBS = -lm881"

#Motorola Delta System V/88 R32, signal() is void rather than int.
#Uses dirent.h and Honey DanBer uucp.  Needs <sys/utime.h> for setting
#file dates.  Supports TCP/IP.
sv88r32:
	@echo 'Making C-Kermit $(CKVER) for Motorola UNIX System V/88 R32...'
	$(MAKE) wermit "CFLAGS = -DSVR3 -DDIRENT -DHDBUUCP -DDYNAMIC \
	-DTCPSOCKET -DSYSUTIMEH -DCK_CURSES $(KFLAGS) \
	-O" "LIBS= -lcurses" "LNKFLAGS = -s"

#Motorola Delta System V/88 R40.  Has <sys/termiox.h>, regular Berkeley
#sockets library, i.e. in.h and inet.h are not misplaced in sys (rather than
#netinet and arpa, respectively).  Uses ANSI C constructs, advisory file
#locking on devices, etc.  curses support added.  Reportedly, the
#/usr/include/sys/vnode.h file has a bug which must be fixed before this
#makefile entry can work correctly.  The "if DEBUG" directive at about line
#320 must be changed to "ifdef DEBUG".
sv88r40:
	@echo 'Making C-Kermit $(CKVER) for Motorola UNIX System V/88 R40...'
	$(MAKE) wermit "CFLAGS = -O -DSVR4 -DDIRENT -DHDBUUCP \
	-DDYNAMIC -DSTERMIOX -DTCPSOCKET -DCK_CURSES $(KFLAGS)" \
	"LIBS= -lsocket -lnsl -lcurses" "LNKFLAGS = -s"

#PFU Compact A Series UNIX System V R3, SX/A TISP V10/L50 (Japan)
#Maybe the -i link option should be removed?
sxae50:
	@echo 'Making C-Kermit $(CKVER) for PFU SX/A V10/L50...'
	$(MAKE) wermit \
	"CFLAGS= -DSVR3 -DDIRENT -DsxaE50 -DTCPSOCKET $(KFLAGS) -i -O \
	-DKANJI" "LNKFLAGS= "

#Olivetti X/OS R2.3, 3.x.
#NOTES:
# . If you build the executable on 2.x X/OS, it will also run on 3.x.
# . If you build it on 3.x X/OS, it will NOT run on 2.x.
# . Kermit can run with no privileges unless the uucp lines are protected,
#   in which case kermit must be owned by uucp with suid bit set:
#   chown uucp kermit ; chmod 4111 kermit.
xos23:
	@echo 'Making C-Kermit $(CKVER) for Olivetti X/OS...'
	$(MAKE) wermit \
	'CFLAGS=-ucb -DBSD4 -DTCPSOCKET -DHDBUUCP -DDYNAMIC -DOXOS $(KFLAGS)' \
	"LNKFLAGS=-ucb -s"		

xos23c:
	@echo 'Making C-Kermit $(CKVER) for Olivetti X/OS...'
	$(MAKE) wermit \
	'CFLAGS=-ucb -DBSD4 -DTCPSOCKET -DHDBUUCP -DDYNAMIC -DOXOS \
	-DCK_CURSES $(KFLAGS)' "LIBS=-lcurses" "LNKFLAGS=-ucb -s" 

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
	@echo 'Running Lint on C-Kermit $(CKVER) sources for SunOS version...'
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
