/* ckuver.h -- C-Kermit UNIX Version heralds */
/*
  Author: Frank da Cruz (fdc@columbia.edu, FDCCU@CUVMA.BITNET),
  Columbia University Center for Computing Activities.
  First released January 1985.
  Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
  York.  Permission is granted to any individual or institution to use this
  software as long as it is not sold for profit.  This copyright notice must be
  retained.  This software may not be included in commercial products without
  written permission of Columbia University.
*/

#ifndef CKUVER_H
#define CKUVER_H

/* Arranged more or less alphabetically by compiler symbol */
/* Must be included AFTER ckcdeb.h. */

#ifdef MAC
#define HERALD " Apple Macintosh"
#endif /* MAC */

#ifdef A986
#define HERALD " Altos 986 / Xenix 3.0"
#endif /* A986 */

#ifdef aegis
#ifdef BSD4
#define HERALD " Apollo DOMAIN/IX 4.2 BSD"
#else
#ifdef ATTSV
#define HERALD " Apollo DOMAIN/IX System V"
#else
#define HERALD " Apollo Aegis"
#endif /* BSD4  */
#endif /* ATTSV */
#endif /* aegis */

#ifdef AIXRS
#define HERALD " IBM RS/6000 (AIX 3.x)"
#endif /* AIXRS */

#ifdef PS2AIX10
#define HERALD " IBM PS/2 (AIX 1.x)"
#endif /* PS2AIX10 */

#ifdef AIXPS2
#define HERALD " IBM PS/2 (AIX 3.x)"
#endif /* AIXPS2 */

#ifdef AIX370
#ifndef HERALD
#define HERALD " IBM System/370 (AIX 3.x)"
#endif
#endif /* AIX370 */

#ifdef ATT6300
#define HERALD " AT&T 6300"
#endif /* ATT6300 */

#ifdef ATT7300
#define HERALD " AT&T 7300 UNIX PC"
#endif /* ATT7300 */

#ifdef AUX
#define HERALD " Apple Macintosh AUX"
#endif /* AUX */

#ifdef BSD44
#define HERALD " 4.4 BSD"
#endif /* BSD44 */

#ifdef ENCORE
#ifdef BSD43
#define HERALD " Encore Multimax UMAX 4.3"
#else
#define HERALD " Encore Multimax UMAX 4.2"
#endif
#endif /* ENCORE */

#ifdef BSD29
#define HERALD " 2.9 BSD"
#endif /* BSD29 */

#ifdef BSD41
#define HERALD " 4.1 BSD"
#endif /* BSD41 */

#ifdef C70
#define HERALD " BBN C/70"
#endif /* c70 */

#ifdef CIE
#define HERALD " CIE Systems 680/20 Regulus"
#endif /* CIE */

#ifdef COHERENT
#ifdef _I386
#define HERALD " MWC Coherent 386 4.x"
#else
#define HERALD " PC/AT MWC Coherent 286 3.x"
#endif /* _I386 */
#endif /* COHERENT */

#ifdef CONVEX9
#define HERALD " Convex/OS"
#endif /* CONVEX9 */

#ifdef DGUX430
#define HERALD " Data General DG/UX 4.30"
#endif /* DGUX430 */

#ifdef DGUX540
#define HERALD " Data General DG/UX 5.4"
#endif /* DGUX540 */

#ifdef datageneral
#ifndef HERALD
#define HERALD " Data General AOS/VS"
#endif /* HERALD */
#endif /* datageneral */

#ifdef DELL_SVR4
#define HERALD " Dell System V R4"
#endif /* DELL_SVR4 */

#ifdef ICL_SVR4
#define HERALD " ICL System V R4 DRS N/X"
#endif /* ICL_SVR4 */

#ifdef FT18
#ifdef FT21
#define HERALD " Fortune For:Pro 2.1"
#else
#define HERALD " Fortune For:Pro 1.8"
#endif /* FT21 */
#endif /* FT18 */

#ifdef GEMDOS
#define HERALD " Atari ST GEM 1.0"
#endif /* GEMDOS */

#ifdef I386IX
#ifdef SVR3JC
#define HERALD " Interactive UNIX System V/386 R3.2"
#else
#define HERALD " Interactive Systems Corp 386/ix"
#endif /* SVR3JC */
#endif /* I386IX */

#ifdef IRIX40
#define HERALD " Silicon Graphics IRIX 4.0"
#endif /* IRIX40 */

#ifdef ISIII
#define HERALD " Interactive Systems Corp System III"
#endif /* ISIII */

#ifdef IX370
#define HERALD " IBM IX/370"
#endif /* IX370 */

#ifdef HPUX
#define HERALD " HP 9000 Series HP-UX"
#endif /* HPUX */

#ifdef MINIX
#define HERALD " Minix"
#endif /* MINIX */

#ifdef MIPS
#define HERALD " MIPS RISC/OS (System V R3)"
#endif /* MIPS */

#ifdef NEXT
#define HERALD " NeXT"
#endif /* NEXT */

#ifdef OSF
#define HERALD " DEC OSF/1 1.0"
#endif /* OSF */

#ifdef PTX
#define HERALD " DYNIX/PTX 1.3"
#endif /* PTX */

#ifdef PCIX
#define HERALD " PC/IX"
#endif /* PCIX */

#ifdef sxaE50
#define HERALD " PFU SX/A V10/L50"
#endif /* sxaE50 */

#ifdef PROVX1
#define HERALD " DEC Professional 300 (Venix 1.0)"
#endif /* PROVX1 */

#ifdef RTAIX
#define HERALD " IBM RT PC (AIX 2.2)"
#endif /* RTAIX */

#ifdef RTU
#define HERALD " Masscomp/Concurrent RTU"
#endif /* RTU */

#ifdef sony_news
#define HERALD " SONY NEWS"
#endif /* sony_news */

#ifdef SVR4
#ifdef sun
#define HERALD "SUN Solaris 2.1"
#endif /* sun */
#endif /* SVR4 */

#ifdef SOLARIS
#ifdef sun
#define HERALD " SUN Solaris 2.0"
#else
#define HERALD " Solaris 2.0"
#endif /* SUN */
#endif /* SOLARIS */

#ifdef SUNOS4
#ifdef BSD4
#ifdef SUNOS41
#define HERALD " SunOS 4.1 (BSD)"
#else
#define HERALD " SunOS 4.0 (BSD)"
#endif /* SUNOS41 */
#endif /* BSD4 */
#endif /* SUNOS4 */

#ifdef SUN4S5
#ifdef HDBUUCP
#define HERALD " SunOS 4.1 (SVR3)"
#else
#define HERALD " SunOS 4.0 (SVR3)"
#endif /* HDBUUCP */
#endif /* SUN4S5 */

#ifdef TOWER1
#define HERALD " NCR Tower 1632 (OS 1.02)"
#endif /* TOWER1 */

#ifdef TRS16
#define HERALD " Tandy 16/6000 (Xenix 3.0)"
#endif /* TRS16 */

#ifdef u3b2
#ifndef HERALD
#ifdef SVR3
#define HERALD " AT&T 3B2 (System V R3)"
#else
#define HERALD " AT&T 3B2 (System V)"
#endif /* SVR3 */
#endif /* HERALD */
#endif /* u3b2 */

#ifdef ultrix
#ifdef vax
#define HERALD " VAX/ULTRIX"
#else
#ifdef mips
#define HERALD " DECstation/ULTRIX"
#else
#define HERALD " ULTRIX"
#endif /* mips */
#endif /* vax */
#endif /* ultrix */

#ifdef OXOS
#define HERALD " Olivetti X/OS"
#endif /* OXOS */

#ifdef _386BSD
#define HERALD " 386BSD"
#endif /* _386BSD */

#ifdef POSIX
#ifdef HERALD
#undef HERALD
#endif /* HERALD */
#define HERALD " POSIX"
#endif /* POSIX */

#ifdef UTS24
#define HERALD " Amdahl UTS 2.4"
#endif /* UTS24 */

#ifdef UTSV
#define HERALD " Amdahl UTS V"
#endif /* UTSV */

#ifdef VXVE
#define HERALD " CDC VX/VE 5.2.1 System V"
#endif /* VXVE */

#ifdef XENIX
#ifdef HERALD
#undef HERALD
#endif /* HERALD */
#ifdef M_UNIX 
#define HERALD " SCO UNIX/386"
#else
#ifdef M_I386
#define HERALD " Xenix/386"
#else
#ifdef M_I286
#define HERALD " Xenix/286"
#else
#define HERALD " Xenix"
#endif /* M_I286 */
#endif /* M_I386 */
#endif /* M_UNIX */
#endif /* XENIX  */

#ifdef ZILOG
#define HERALD " Zilog S8000 Zeus 3.21+"
#endif /* ZILOG */

#ifdef UTEK
#define HERALD " UTek"
#endif /* UTEK */

/* Catch-alls for anything not defined explicitly above */

#ifndef HERALD
#ifdef SVR4
#ifdef i386
#define HERALD " AT&T System V/386 R4"
#else
#ifdef AMIX
#define HERALD " Commodore Amiga System V/m68k R4"
#else
#define HERALD " AT&T System V R4"
#endif /* AMIX */
#endif /* i386 */
#else
#ifdef SVR3
#define HERALD " AT&T System V R3"
#else
#ifdef ATTSV
#define HERALD " AT&T System III / System V"
#else
#ifdef BSD43
#ifdef pdp11
#define HERALD " PDP-11 2.10 BSD"
#else
#ifdef vax
#define HERALD " VAX 4.3 BSD"
#else
#define HERALD " 4.3 BSD"
#endif /* vax */
#endif /* pdp11 */
#else
#ifdef BSD4
#ifdef vax
#define HERALD " VAX 4.2 BSD"
#else
#define HERALD " 4.2 BSD"
#endif /* vax */
#else
#ifdef V7
#define HERALD " UNIX Version 7"
#endif /* V7 */
#endif /* BSD4 */
#endif /* BSD43 */
#endif /* ATTSV */
#endif /* SVR3 */
#endif /* SVR4 */
#endif /* HERALD */

#ifdef OS2
#ifdef HERALD
#undef HERALD
#endif /* HERALD */
#define HERALD " OS/2"
#endif /* OS/2 */

#ifndef HERALD
#define HERALD " Unknown Version"
#endif /* HERALD */

/* Hardware type */

#ifdef vax				/* DEC VAX */
#ifndef CKCPU
#define CKCPU "vax"
#endif /* CKCPU */
#endif /*  vax */
#ifdef pdp11				/* DEC PDP-11 */
#ifndef CKCPU
#define CKCPU "pdp11"
#endif /* CKCPU */
#endif /* pdp11 */

#ifdef __ALPHA
#ifndef CKCPU
#define CKCPU "Alpha"
#endif /* CKCPU */
#endif /* __ALPHA */

#ifdef __hp9000s800			/* HP 9000 */
#define CKCPU "hp9000s800"
#endif /* __hp9000s800 */
#ifdef __hp9000s500
#ifndef CKCPU
#define CKCPU "hp9000s500"
#endif /* CKCPU */
#endif /* __hp9000s500 */
#ifdef __hp9000s300
#ifndef CKCPU
#define CKCPU "hp9000s300"
#endif /* CKCPU */
#endif /* __hp9000s300 */
#ifdef __hp9000s200
#ifndef CKCPU
#define CKCPU "hp9000s200"
#endif /* CKCPU */
#endif /* __hp9000s200 */
#ifdef m88000				/* Motorola 88000 */
#ifndef CKCPU
#define CKCPU "mc88000"
#endif /* CKCPU */
#endif /* m88000 */
#ifdef __using_M88KBCS			/* DG symbol for Motorola 88000 */
#ifndef CKCPU
#define CKCPU "mc88000"
#endif /* CKCPU */
#endif /* __using_M88KBCS */
#ifdef m88k				/* Motorola symbol for 88000 */
#ifndef CKCPU
#define CKCPU "mc88000"
#endif /* CKCPU */
#endif /* m88k */
#ifdef mc68040				/* Motorola 68040 */
#ifndef CKCPU
#define CKCPU "mc68040"
#endif /* CKCPU */
#endif /* mc68040 */
#ifdef mc68030				/* Motorola 68030 */
#ifndef CKCPU
#define CKCPU "mc68030"
#endif /* CKCPU */
#endif /* mc68030 */
#ifdef mc68020				/* Motorola 68020 */
#ifndef CKCPU
#define CKCPU "mc68020"
#endif /* CKCPU */
#endif /* mc68020 */
#ifdef mc68010				/* Motorola 68010 */
#ifndef CKCPU
#define CKCPU "mc68010"
#endif /* CKCPU */
#endif /* mc68010 */
#ifdef mc68000				/* Motorola 68000 */
#ifndef CKCPU
#define CKCPU "mc68000"
#endif /* CKCPU */
#endif /* mc68000 */
#ifdef mc68k				/* Ditto (used by DIAB DS90) */
#ifndef CKCPU
#define CKCPU "mc68000"
#endif /* CKCPU */
#endif /* mc68k */
#ifdef m68				/* Ditto */
#ifndef CKCPU
#define CKCPU "mc68000"
#endif /* CKCPU */
#endif /* m68 */
#ifdef m68k				/* Ditto */
#ifndef CKCPU
#define CKCPU "mc68000"
#endif /* CKCPU */
#endif /* m68k */
#ifdef i486				/* Intel 80486 */
#ifndef CKCPU
#define CKCPU "i486"
#endif /* CKCPU */
#endif /* i80486 */
#ifdef i386				/* Intel 80386 */
#ifndef CKCPU
#define CKCPU "i386"
#endif /* CKCPU */
#endif /* i80386 */
#ifdef i286				/* Intel 80286 */
#ifndef CKCPU
#define CKCPU "i286"
#endif /* CKCPU */
#endif /* i286 */
#ifdef i186				/* Intel 80186 */
#ifndef CKCPU
#define CKCPU "i186"
#endif /* CKCPU */
#endif /* i186 */
#ifdef M_I386				/* Intel 80386 */
#ifndef CKCPU
#define CKCPU "i386"
#endif /* CKCPU */
#endif /* M_I386 */
#ifdef _M_I386				/* Intel 80386 */
#ifndef CKCPU
#define CKCPU "i386"
#endif /* CKCPU */
#endif /* _M_I386 */
#ifdef M_I286				/* Intel 80286 */
#ifndef CKCPU
#define CKCPU "i286"
#endif /* CKCPU */
#endif /* M_I286 */
#ifdef M_I86				/* Intel 80x86 */
#ifndef CKCPU
#define CKCPU "ix86"
#endif /* CKCPU */
#endif /* M_I86 */
#ifdef sparc				/* SUN SPARC */
#ifndef CKCPU
#define CKCPU "sparc"
#endif /* CKCPU */
#endif /* sparc */
#ifdef mips				/* MIPS RISC processor */
#ifndef CKCPU
#define CKCPU "mips"
#endif /* CKCPU */
#endif /* mips */
#ifdef _IBMR2				/* IBM RS/6000 */
#ifndef CKCPU				/* (what do they call the chip?) */
#define CKCPU "rs6000"
#endif /* CKCPU */
#endif /* rs6000 */
#ifdef u3b5				/* WE32000 MAC-32, AT&T 3Bx */
#ifndef CKCPU
#define CKCPU "u3b5"
#endif /* CKCPU */
#endif /* u3b5 */
#ifdef n3b
#ifndef CKCPU
#define CKCPU "n3b"
#endif /* CKCPU */
#endif /* n3b */
#ifdef u3b
#ifndef CKCPU
#define CKCPU "u3b"
#endif /* CKCPU */
#endif /* u3b */
#ifdef n16				/* Encore Multimax */
#ifndef CKCPU
#define CKCPU "n16"
#endif /* CKCPU */
#endif /* n16 */
#ifdef u370				/* IBM 370 */
#ifndef CKCPU
#define CKCPU "u370"
#endif /* CKCPU */
#endif /* u370 */
#ifdef MAC				/* Macintosh catch-all */
#ifndef CKCPU
#define CKCPU "mc68000"
#endif /* CKCPU */
#endif /* MAC */

#ifndef CKCPU				/* All others */
#define CKCPU "unknown"
#endif /* CKCPU */

#endif /* CKUVER_H */
