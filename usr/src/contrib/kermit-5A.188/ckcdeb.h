/*  C K C D E B . H  */

/*
  This file is included by all C-Kermit modules, including the modules
  that aren't specific to Kermit (like the command parser and the ck?tio and
  ck?fio modules.  It specifies format codes for debug(), tlog(), and similar
  functions, and includes any necessary definitions to be used by all C-Kermit
  modules, and also includes some feature selection compile-time switches, and
  also system- or compiler-dependent definitions, plus #includes and prototypes
  required by all C-Kermit modules.
*/

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

/*
  Etymology: The name of this file means "C-Kermit Common-C-Language Debugging
  Header", because originally it contained only the formats (F000-F111) for
  the debug() and tlog() functions.  See how it has grown...
*/

#ifndef CKCDEB_H			/* Don't include me more than once. */
#define CKCDEB_H

#ifdef OS2				/* For OS/2 debugging */
#include "ckoker.h"
#endif /* OS2 */

#include <stdio.h>			/* Begin by including this. */
#include <ctype.h>			/* and this. */

#ifdef MAC
/*
 * The MAC doesn't use standard stdio routines.
 */
#undef getchar
#undef putchar
#endif /* MAC */

/* System-type compilation switches */

#ifdef FT21				/* Fortune For:Pro 2.1 implies 1.8 */
#ifndef FT18
#define FT18
#endif /* FT18 */
#endif /* FT21 */

#ifdef AIXPS2				/* AIXPS2 implies AIX370 */
#ifndef AIX370
#define AIX370
#endif /* AIX370 */
#endif /* AIXPS2 */

#ifdef AIX370				/* AIX PS/2 or 370 implies BSD4 */
#ifndef BSD4
#define BSD4
#endif /* BSD4 */
#endif /* AIX370 */

#ifdef DGUX540				/* DG UX 5.40 implies Sys V R 4 */
#ifndef SVR4
#define SVR4
#endif /* SVR4 */
#endif /* DGUX540 */

#ifdef SUNOS41				/* SUNOS41 implies SUNOS4 */
#ifndef SUNOS4
#define SUNOS4
#endif /* SUNOS4 */
#endif /* SUNOS41 */

#ifdef SUN4S5				/* Sun-4 System V environment */
#ifndef SVR3				/* implies System V R3 or later */
#define SVR3
#endif /* SVR3 */
#endif /* SUN4S5 */

#ifdef MIPS				/* MIPS System V environment */
#ifndef SVR3				/* implies System V R3 or later */
#define SVR3
#endif /* SVR3 */
#endif /* MIPS */

/*
  4.4BSD is a mixture of System V R4, POSIX, and 4.3BSD.
*/
#ifdef BSD44				/* 4.4 BSD */
#ifndef SVR4				/* BSD44 implies SVR4 */
#define SVR4
#endif /* SVR4 */
#ifndef NOSETBUF			/* NOSETBUF is safe */
#define NOSETBUF
#endif /* NOSETBUF */
#ifndef DIRENT				/* Uses <dirent.h> */
#define DIRENT
#endif /* DIRENT */
#endif /* BSD44 */

#ifdef SVR3				/* SVR3 implies ATTSV */
#ifndef ATTSV
#define ATTSV
#endif /* ATTSV */
#endif /* SVR3 */

#ifdef SVR4				/* SVR4 implies ATTSV */
#ifndef ATTSV
#define ATTSV
#endif /* ATTSV */
#ifndef SVR3				/* ...as well as SVR3 */
#define SVR3
#endif /* SVR3 */
#endif /* SVR4 */

#ifdef UTSV				/* UTSV implies ATTSV */
#ifndef ATTSV
#define ATTSV
#endif /* ATTSV */
#endif /* UTSV */

#ifdef XENIX				/* XENIX implies ATTSV */
#ifndef ATTSV
#define ATTSV
#endif /* ATTSV */
#endif /* XENIX */

#ifdef AUX				/* AUX implies ATTSV */
#ifndef ATTSV
#define ATTSV
#endif /* ATTSV */
#endif /* AUX */

#ifdef ATT7300				/* ATT7300 implies ATTSV */
#ifndef ATTSV
#define ATTSV
#endif /* ATTSV */
#endif /* ATT7300 */

#ifdef ATT6300				/* ATT6300 implies ATTSV */
#ifndef ATTSV
#define ATTSV
#endif /* ATTSV */
#endif /* ATT6300 */

#ifdef HPUX				/* HPUX implies ATTSV */
#ifndef ATTSV
#define ATTSV
#endif /* ATTSV */
#endif /* HPUX */

#ifdef ISIII				/* ISIII implies ATTSV */
#ifndef ATTSV
#define ATTSV
#endif /* ATTSV */
#endif /* ISIII */

#ifdef NEXT				/* NEXT implies BSD4 */
#ifndef BSD4
#define BSD4
#endif /* BSD4 */
#endif /* NEXT */

#ifdef SUNOS4				/* SUNOS4 implies BSD4 */
#ifndef BSD4
#define BSD4
#endif /* BSD4 */
#endif /* SUNOS4 */

#ifdef BSD41				/* BSD41 implies BSD4 */
#ifndef BSD4
#define BSD4
#endif /* BSD4 */
#endif /* BSD41 */

#ifdef BSD43				/* BSD43 implies BSD4 */
#ifndef BSD4
#define BSD4
#endif /* BSD4 */
#endif /* BSD43 */

#ifdef BSD4				/* BSD4 implies ANYBSD */
#ifndef ANYBSD
#define ANYBSD
#endif /* ANYBSD */
#endif /* BSD4 */

#ifdef BSD29				/* BSD29 implies ANYBSD */
#ifndef ANYBSD
#define ANYBSD
#endif /* ANYBSD */
#endif /* BSD29 */

#ifdef ATTSV				/* ATTSV implies UNIX */
#ifndef UNIX
#define UNIX
#endif /* UNIX */
#endif /* ATTSV */

#ifdef ANYBSD				/* ANYBSD implies UNIX */
#ifndef UNIX
#define UNIX
#endif /* UNIX */
#endif /* ANYBSD */

#ifdef POSIX				/* POSIX implies UNIX */
#ifndef UNIX
#define UNIX
#endif /* UNIX */
#ifndef DIRENT				/* and DIRENT, i.e. <dirent.h> */
#ifndef SDIRENT
#define DIRENT
#endif /* SDIRENT */
#endif /* DIRENT */
#ifndef NOFILEH				/* POSIX doesn't use <sys/file.h> */
#define NOFILEH
#endif /* NOFILEH */
#endif /* POSIX */

#ifdef V7
#ifndef UNIX
#define UNIX
#endif /* UNIX */
#endif /* V7 */

#ifdef COHERENT
#ifndef UNIX
#define UNIX
#endif /* UNIX */
#endif /* COHERENT */

#ifdef MINIX
#ifndef UNIX
#define UNIX
#endif /* UNIX */
#endif /* MINIX */
/*
  The symbol SVORPOSIX is defined for both AT&T and POSIX compilations
  to make it easier to select items that System V and POSIX have in common,
  but which BSD, V7, etc, do not have.
*/
#ifdef ATTSV
#ifndef SVORPOSIX
#define SVORPOSIX
#endif /* SVORPOSIX */
#endif /* ATTSV */

#ifdef POSIX
#ifndef SVORPOSIX
#define SVORPOSIX
#endif /* SVORPOSIX */
#endif /* POSIX */

/*
  The symbol SVR4ORPOSIX is defined for both AT&T System V R4 and POSIX 
  compilations to make it easier to select items that System V R4 and POSIX 
  have in common, but which BSD, V7, and System V R3 and earlier, etc, do
  not have.
*/
#ifdef POSIX
#ifndef SVR4ORPOSIX
#define SVR4ORPOSIX
#endif /* SVR4ORPOSIX */
#endif /* POSIX */
#ifdef SVR4
#ifndef SVR4ORPOSIX
#define SVR4ORPOSIX
#endif /* SVR4ORPOSIX */
#endif /* SVR4 */

/*
  The symbol BSD44ORPOSIX is defined for both 4.4BSD and POSIX compilations
  to make it easier to select items that 4.4BSD and POSIX have in common,
  but which System V, BSD, V7, etc, do not have.
*/
#ifdef BSD44
#ifndef BSD44ORPOSIX
#define BSD44ORPOSIX
#endif /* BSD44ORPOSIX */
#endif /* BSD44 */

#ifdef POSIX
#ifndef BSD44ORPOSIX
#define BSD44ORPOSIX
#endif /* BSD44ORPOSIX */
#endif /* POSIX */

#ifdef apollo				/* May be ANSI-C, check further */
#ifdef __STDCPP__
#define CK_ANSIC			/* Yes, this is real ANSI-C */
#define SIG_V
#else
#define NOANSI				/* Nope, not ANSI */
#undef __STDC__				/* Even though it say it is! */
#define SIG_I
#endif /* __STDCPP__ */
#endif /* apollo */

#ifdef POSIX				/* -DPOSIX on cc command line */
#ifndef _POSIX_SOURCE			/* Implies _POSIX_SOURCE */
#define _POSIX_SOURCE
#endif /* _POSIX_SOURCE */
#endif /* POSIX */

/*
  ANSI C?  That is, do we have function prototypes, new-style
  function declarations, and parameter type checking and coercion?
*/
#ifdef MAC				/* MPW C is ANSI */
#ifndef NOANSI
#ifndef CK_ANSIC
#define CK_ANSIC
#endif /* CK_ANSIC */
#endif /* NOANSI */
#endif /* MAC */

#ifndef NOANSI
#ifdef __STDC__				/* __STDC__ means ANSI C */
#ifndef CK_ANSIC
#define CK_ANSIC
#endif /* CK_ANSIC */
#endif /* __STDC__ */
#endif /* NOANSI */
/*
  _PROTOTYP() is used for forward declarations of functions so we can have
  parameter and return value type checking if the compiler offers it.
  __STDC__ should be defined by the compiler only if function prototypes are
  allowed.  Otherwise, we get old-style forward declarations.  Our own private
  CK_ANSIC symbol tells whether we use ANSI C prototypes.  To force use of
  ANSI prototypes, include -DCK_ANSIC on the cc command line.  To disable the
  use of ANSI prototypes, include -DNOANSI.
*/
#ifdef CK_ANSIC
#define _PROTOTYP( func, parms ) func parms
#else /* Not ANSI C */
#define _PROTOTYP( func, parms ) func()
#endif /* CK_ANSIC */

/*
  Altos-specific items: 486, 586, 986 models...
*/
#ifdef A986
#define M_VOID
#define void int
#define CHAR char
#define SIG_I
#endif /* A986 */

/* Void type */

#ifndef VOID				/* Used throughout all C-Kermit */
#ifdef CK_ANSIC				/* modules... */
#define VOID void
#else
#define VOID int
#endif /* CK_ANSIC */
#endif /* VOID */

/* Signal type */

#ifndef SIG_V				/* signal() type, if not def'd yet */
#ifndef SIG_I
#ifdef OS2
#define SIG_V
#else
#ifdef POSIX
#define SIG_V
#else
#ifdef SVR3				/* System V R3 and later */
#define SIG_V
#else
#ifdef SUNOS4				/* SUNOS V 4.0 and later */
#ifndef sun386
#define SIG_V 
#else
#define SIG_I
#endif /* sun386 */
#else
#ifdef NEXT				/* NeXT */
#define SIG_V
#else
#ifdef AIX370
#define SIG_V
#define SIGTYP __SIGVOID		/* AIX370 */
#else
#define SIG_I
#endif /* AIX370 */
#endif /* NEXT */
#endif /* SUNOS4 */
#endif /* SVR3 */
#endif /* POSIX */
#endif /* OS2 */
#endif /* SIG_I */
#endif /* SIG_V */

#ifdef SIG_I
#define SIGRETURN return(0)
#ifndef SIGTYP
#define SIGTYP int
#endif /* SIGTYP */
#endif /* SIG_I */

#ifdef SIG_V
#define SIGRETURN return
#ifndef SIGTYP
#define SIGTYP void
#endif /* SIGTYP */
#endif /* SIG_V */

#ifndef SIGTYP
#define SIGTYP int
#endif /* SIGTYP */

#ifndef SIGRETURN
#define SIGRETURN return(0)
#endif /* SIGRETURN */

/* We want all characters to be unsigned if the compiler supports it */

#ifdef PROVX1
typedef char CHAR;
/* typedef long LONG; */
typedef int void;
#else
#ifdef MINIX
typedef unsigned char CHAR;
#else
#ifdef V7
typedef char CHAR;
#else
#ifdef C70
typedef char CHAR;
/* typedef long LONG; */
#else
#ifdef BSD29
typedef char CHAR;
/* typedef long LONG; */
#else
#ifdef datageneral
#define CHAR unsigned char			/* 3.22 compiler */	
#else
#ifdef CHAR
#undef CHAR
#endif /* CHAR */
typedef unsigned char CHAR;
#endif /* datageneral */
#endif /* BSD29 */
#endif /* C70 */
#endif /* V7 */
#endif /* MINIX */
#endif /* PROVX1 */

/*
 Debug and transaction logging is included automatically unless you define
 NODEBUG or NOTLOG.  Do this if you want to save the space and overhead.
 (Note, in version 4F these definitions changed from "{}" to the null string
 to avoid problems with semicolons after braces, as in: "if (x) tlog(this);
 else tlog(that);"
*/
#ifndef NODEBUG
#ifndef DEBUG
#define DEBUG
#endif /* DEBUG */
#else
#ifdef DEBUG
#undef DEBUG
#endif /* DEBUG */
#endif /* NODEBUG */

#ifndef NOTLOG
#ifndef TLOG
#define TLOG
#endif /* TLOG */
#endif /* NOTLOG */

/* debug() macro style selection. */

#ifdef MAC
#ifndef IFDEBUG
#define IFDEBUG
#endif /* IFDEBUG */
#endif /* MAC */

#ifdef OS2
#ifndef IFDEBUG
#define IFDEBUG
#endif /* IFDEBUG */
#endif /* OS2 */

#ifndef DEBUG
/* Compile all the debug() statements away.  Saves a lot of space and time. */
#define debug(a,b,c,d)
#else
#ifndef CKCMAI
/* Debugging included.  Declare debug log flag in main program only. */
extern int deblog;
#endif /* CKCMAI */
/* Now define the debug() macro. */
#ifdef IFDEBUG
/* Use this form to avoid function calls: */
#define debug(a,b,c,d) if (deblog) dodebug(a,b,(char *)c,(long)d)
#else
/* Use this form to save space: */
#define debug(a,b,c,d) dodebug(a,b,(char *)c,(long)d)
#endif /* MAC */
_PROTOTYP(int dodebug,(int, char *, char *, long));
#endif /* DEBUG */

#ifndef TLOG
#define tlog(a,b,c,d)
#else
_PROTOTYP(VOID tlog,(int, char *, char *, long));
#endif /* TLOG */

/* Formats for debug() and tlog() */

#define F000 0
#define F001 1
#define F010 2
#define F011 3
#define F100 4
#define F101 5
#define F110 6
#define F111 7

/* Kermit feature selection */

#ifdef MAC				/* For Macintosh, no escape */
#define NOPUSH				/* to operating system */
#endif /* MAC */

#ifdef UNIX
#ifndef NOPARSEN
#define PARSENSE			/* Automatic parity detection */
#endif /* NOPARSEN */
#endif /* UNIX */			/* for Unix */

#ifdef VMS				/* ... and VMS */
#ifndef NOPARSEN
#define PARSENSE
#endif /* NOPARSEN */
#endif /* VMS */

#ifdef MAC				/* and Macintosh */
#ifndef NOPARSEN
#define PARSENSE
#endif /* NOPARSEN */
#endif /* MAC */

#ifdef VMS				/* Use dynamic memory allocation */
#ifndef DYNAMIC
#define DYNAMIC				/* in VMS version. */
#endif /* DYNAMIC */
#endif /* VMS */

#ifndef CK_LBRK				/* Can send Long BREAK */

#ifdef UNIX				/* (everybody but OS-9) */
#define CK_LBRK
#endif /* UNIX */
#ifdef VMS
#define CK_LBRK
#endif /* VMS */
#ifdef datageneral
#define CK_LBRK
#endif /* datageneral */
#ifdef GEMDOS
#define CK_LBRK
#endif /* GEMDOS */
#ifdef OS2
#define CK_LBRK
#endif /* OS2 */
#ifdef AMIGA
#define CK_LBRK
#endif /* AMIGA */

#endif /* CK_LBRK */

/* Carrier treatment */
/* These are defined here because they are shared by the system dependent */
/* and the system independent modules. */

#define  CAR_OFF 0	/* On: heed carrier always, except during DIAL. */
#define  CAR_ON  1      /* Off: ignore carrier always. */
#define  CAR_AUT 2      /* Auto: heed carrier, but only if line is declared */
			/* to be a modem line, and only during CONNECT. */

/* Hangup by modem command supported by default */

#ifndef NODIAL
#ifndef NOMDMHUP
#ifndef MDMHUP
#define MDMHUP 
#endif /* MDMHUP */
#endif /* NOMDMHUP */
#endif /* NODIAL */

/* Types of flow control available */

#define CK_XONXOFF			/* Everybody can do this, right? */

#ifdef AMIGA				/* Commodore Amiga */
#define CK_RTSCTS			/* has RTS/CTS */
#endif /* AMIGA */

#ifdef SUN4S5				/* SunOS in System V environment */
#define CK_RTSCTS
#else					/* SunOS 4.0/4.1 in BSD environment */
#ifdef SUNOS4				/* SunOS 4.0+later supports RTS/CTS */
#ifdef SUNOS41				/* Easy in 4.1 and later */
#define CK_RTSCTS
#else					/* Harder in 4.0 */
#ifndef __GNUC__			/* (see tthflow() in ckutio.c) */
#ifndef GNUC
#define CK_RTSCTS			/* Only if not using GNU gcc */
#endif /* __GNUC__ */
#endif /* GNUC */
#endif /* SUNOS41 */
#endif /* SUNOS4 */
#endif /* SUN4S5 */

#ifdef BSD44				/* And in 4.4 BSD */
#define CK_RTSCTS
#endif /* BSD44 */

#ifdef TERMIOX				/* Sys V R4 <termiox.h> */
#define CK_RTSCTS			/* has RTS/CTS */
#define CK_DTRCD			/* and DTR/CD */
#endif /* TERMIOX */
#ifdef STERMIOX				/* Sys V R4 <sys/termiox.h> */
#define CK_RTSCTS			/* Ditto. */
#define CK_DTRCD
#endif /* STERMIOX */

/*
 Systems where we can expand tilde at the beginning of file or directory names
*/
#ifdef POSIX
#ifndef DTILDE
#define DTILDE
#endif /* DTILDE */
#endif /* POSIX */
#ifdef BSD4
#ifndef DTILDE
#define DTILDE
#endif /* DTILDE */
#endif /* BSD4 */
#ifdef ATTSV
#ifndef DTILDE
#define DTILDE
#endif /* DTILDE */
#endif /* ATTSV */
#ifdef OSK
#ifndef DTILDE
#define DTILDE
#endif /* DTILDE */
#endif /* OSK */
#ifdef HPUX				/* I don't know why this is */
#ifndef DTILDE				/* necessary, since -DHPUX */
#define DTILDE				/* automatically defines ATTSV */
#endif /* DTILDE */			/* (see above) ... */
#endif /* HPUX */

/* Line delimiter for text files */

/*
 If the system uses a single character for text file line delimitation,
 define NLCHAR to the value of that character.  For text files, that
 character will be converted to CRLF upon output, and CRLF will be converted
 to that character on input during text-mode (default) packet operations.
*/
#ifdef MAC                              /* Macintosh */
#define NLCHAR 015
#else
#ifdef OSK				/* OS-9/68K */
#define NLCHAR 015
#else                                   /* All Unix-like systems */
#define NLCHAR 012
#endif /* OSK */
#endif /* MAC */

/*
 At this point, if there's a system that uses ordinary CRLF line
 delimitation AND the C compiler actually returns both the CR and
 the LF when doing input from a file, then #undef NLCHAR.
*/
#ifdef OS2				/* OS/2 */
#undef NLCHAR
#endif /* OS2 */

#ifdef GEMDOS				/* Atari ST */
#undef NLCHAR
#endif /* GEMDOS */

/*
  VMS file formats are so complicated we need to do all the conversion 
  work in the CKVFIO module, so we tell the rest of C-Kermit not to fiddle
  with the bytes.
*/

#ifdef vms
#undef NLCHAR
#endif /* vms */

/* The device name of a job's controlling terminal */
/* Special for VMS, same for all Unixes (?), not used by Macintosh */

#ifdef vms
#define CTTNAM "TT:"
#else
#ifdef datageneral
#define CTTNAM "@output"
#else
#ifdef OSK
extern char myttystr[];
#define CTTNAM myttystr
#else
#ifdef OS2
#define CTTNAM "con"
#else
#ifdef UNIX
#define CTTNAM "/dev/tty"
#else
#ifdef GEMDOS
#define CTTNAM "aux:"
#else /* Anyone else... */
#define CTTNAM "stdout"			/* This is a kludge used by Mac */
#endif /* GEMDOS */
#endif /* UNIX */
#endif /* OS2 */
#endif /* OSK */
#endif /* datageneral */
#endif /* vms */

#ifdef SUNS4S5
#define tolower _tolower
#define toupper _toupper
#endif /* SUNS4S5 */

/* Error number */

#ifndef VMS
#ifndef OS2
/*
  The following declaration causes problems for VMS and OS/2, in which
  errno is an "extern volatile int noshare"...
*/
extern int errno;			/* Needed by most modules. */
#endif /* OS2 */
#endif /* VMS */

/* File System Defaults */

#ifdef VMS
#define DBLKSIZ 512
#define DLRECL 512
#else
#define DBLKSIZ 0
#define DLRECL 0
#endif

/* Program return codes for DECUS C and UNIX (VMS uses UNIX codes) */

#ifdef decus
#define GOOD_EXIT   IO_NORMAL
#define BAD_EXIT    IO_ERROR
#else
#define GOOD_EXIT   0
#define BAD_EXIT    1
#endif /* decus */

/* Special hack for Fortune, which doesn't have <sys/file.h>... */

#ifdef FT18
#define FREAD 0x01
#define FWRITE 0x10
#endif /* FT18 */

/* Special hack for OS-9/68k */
#ifdef OSK
#define SIGALRM 30			/* May always cancel I/O */
#define SIGARB	1234			/* Arbitrary for I/O */
SIGTYP (*signal())();
#endif /* OSK */

#ifdef OS2
#ifdef putchar                  /* MSC 5.1 simply uses a macro which causes */
#undef putchar                  /* no problems. */
#endif /* putchar */
#endif /* OS2 */

#ifdef MINIX
#ifdef putchar
#undef putchar
#endif /* putchar */
#define putchar(c) {putc(c,stdout);fflush(stdout);}
#endif /* MINIX */

/* Escape/quote character used by the command parser */

#define CMDQ '\\'

/* Symbols for RS-232 modem signals */

#define KM_FG    1			/* Frame ground */
#define KM_TXD   2			/* Transmit */
#define KM_RXD   3			/* Receive */
#define KM_RTS   4			/* Request to Send */
#define KM_CTS   5			/* Clear to Send */
#define KM_DSR   6			/* Data Set Ready */
#define KM_SG    7			/* Signal ground */
#define KM_DCD   8			/* Carrier Detect */
#define KM_DTR  20			/* Data Terminal Ready */
#define KM_RI   22			/* Ring Indication */

/* Bit mask values for modem signals */

#define BM_CTS   0001			/* Clear to send       (From DCE) */
#define BM_DSR   0002			/* Dataset ready       (From DCE) */
#define BM_DCD   0004			/* Carrier             (From DCE) */
#define BM_RNG   0010			/* Ring Indicator      (From DCE) */
#define BM_DTR   0020			/* Data Terminal Ready (From DTE) */
#define BM_RTS   0040			/* Request to Send     (From DTE) */

/* Codes for full duplex flow control */

#define FLO_NONE 0			/* None */
#define FLO_XONX 1			/* Xon/Xoff (soft) */
#define FLO_RTSC 2			/* RTS/CTS (hard) */
#define FLO_DTRC 3			/* DTR/CD (hard) */
#define FLO_ETXA 4			/* ETX/ACK (soft) */
#define FLO_STRG 5			/* String-based (soft) */
#define FLO_DIAL 6			/* DIALing kludge */
#define FLO_DIAX 7			/* Cancel dialing kludge */
#define FLO_DTRT 8			/* DTR/CTS (hard) */
#define FLO_KEEP 9			/* Keep, i.e. don't touch or change */

/* And finally... */

#ifdef COMMENT				/* Make sure this is NOT defined! */
#undef COMMENT
#endif /* COMMENT */

/* Structure definitions for Kermit file attributes */
/* All strings come as pointer and length combinations */
/* Empty string (or for numeric variables, -1) = unused attribute. */

struct zstr {             /* string format */
    int len;	          /* length */
    char *val;            /* value */
};
struct zattr {            /* Kermit File Attribute structure */
    long lengthk;         /* (!) file length in K */
    struct zstr type;     /* (") file type (text or binary) */
    struct zstr date;     /* (#) file creation date yyyymmdd[ hh:mm[:ss]] */
    struct zstr creator;  /* ($) file creator id */
    struct zstr account;  /* (%) file account */
    struct zstr area;     /* (&) area (e.g. directory) for file */
    struct zstr passwd;   /* (') password for area */
    long blksize;         /* (() file blocksize */
    struct zstr access;   /* ()) file access: new, supersede, append, warn */
    struct zstr encoding; /* (*) encoding (transfer syntax) */
    struct zstr disp;     /* (+) disposition (mail, message, print, etc) */
    struct zstr lprotect; /* (,) protection (local syntax) */
    struct zstr gprotect; /* (-) protection (generic syntax) */
    struct zstr systemid; /* (.) ID for system of origin */
    struct zstr recfm;    /* (/) record format */
    struct zstr sysparam; /* (0) system-dependent parameter string */
    long length;          /* (1) exact length on system of origin */
    struct zstr charset;  /* (2) transfer syntax character set */
    struct zstr reply;    /* This goes last, used for attribute reply */
};

/* Kermit file information structure */

struct filinfo {
  int bs;				/* Blocksize */
  int cs;				/* Character set */
  long rl;				/* Record length */
  int org;				/* Organization */
  int fmt;				/* Record format */
  int cc;				/* Carriage control */
  int typ;				/* Type (text/binary) */
  int dsp;				/* Disposition */
  char *os_specific;			/* OS-specific attributes */
  unsigned int lblopts;			/* LABELED FILE options bitmask */
};

/* VMS values for LABELED FILE options bitmask */

#ifdef VMS
#define LBL_NAM  1			/* Ignore incoming name if set */
#define LBL_PTH  2			/* Use complete path if set */
#define LBL_ACL  4			/* Preserve ACLs if set */
#define LBL_BCK  8			/* Preserve backup date if set */
#define LBL_OWN 16			/* Preserve ownership if set */
#endif /* VMS */

/*
  Data types.  First the header file for data types so we can pick up the
  types used for pids, uids, and gids.  Override this section by putting
  -DCKTYP_H=xxx on the command line to specify the header file where your
  system defines these types.
*/
#ifdef OSK				/* OS-9 */
#include <types.h>
#else					/* General case, not OS-9 */
#ifndef CKTYP_H
#ifndef VMS
#ifndef MAC
#ifndef AMIGA
#define CKTYP_H <sys/types.h>
#endif /* AMIGA */
#endif /* MAC */
#endif /* VMS */
#endif /* CKTYP_H */

#ifdef GEMDOS
#undef CKTYP_H
#include <types.h>
#endif /* GEMDOS */

#ifdef CKTYP_H				/* Include it. */
#ifdef COHERENT				/* Except for COHERENT */
#include <sys/types.h>
#else
#ifdef datageneral
#include <sys/types.h>			/* Compiler didn't like other */
#else
#include CKTYP_H
#endif /* datageneral */
#endif /* COHERENT */
#endif /* CKTYP_H */

#endif /* OSK */			/* End of types.h section */

/*
  Data type for pids.  If your system uses a different type, put something
  like -DPID_T=pid_t on command line, or override here.
*/
#ifndef PID_T
#define PID_T int
#endif /* PID_T */
/*
  Data types for uids and gids.  Same deal as for pids.
  Wouldn't be nice if there was a preprocessor test to find out if a
  typedef existed?
*/
#ifdef VMS
/* Not used in VMS so who cares */
#define UID_T int
#define GID_T int
#endif /* VMS */

#ifdef POSIX
/* Or would it be better (or worse?) to use _POSIX_SOURCE here? */
#ifndef UID_T
#define UID_T uid_t
#endif /* UID_T */
#ifndef GID_T
#define GID_T gid_t
#endif /* GID_T */
#else /* Not POSIX */
#ifdef SVR4
/* SVR4 and later have uid_t and gid_t. */
/* SVR3 and earlier use int, or unsigned short, or.... */
#ifndef UID_T
#define UID_T uid_t
#endif /* UID_T */
#ifndef GID_T
#define GID_T gid_t
#endif /* GID_T */
#else /* Not SVR4 */
#ifdef BSD43
#ifndef UID_T
#define UID_T uid_t
#endif /* UID_T */
#ifndef GID_T
#define GID_T gid_t
#endif /* GID_T */
#else /* Not BSD43 */
/* Default these to int for older UNIX versions */
#ifndef UID_T
#define UID_T int
#endif /* UID_T */
#ifndef GID_T
#define GID_T int
#endif /* GID_T */
#endif /* BSD43 */
#endif /* SVR4  */
#endif /* POSIX */

/* 
  getpwuid() arg type, which is not necessarily the same as UID_T,
  e.g. in SCO UNIX SVR3, it's int.
*/
#ifndef PWID_T
#define PWID_T UID_T
#endif /* PWID_T */

#ifdef NEXT				/* Argument for wait() */
#include <sys/wait.h>
typedef union wait WAIT_T;
#else
#ifdef POSIX
#include <sys/wait.h>
#define WAIT_T pid_t
#else
typedef int WAIT_T;
#endif /* POSIX */
#endif /* NEXT */

/* Forward declarations of system-dependent functions callable from all */
/* C-Kermit modules. */

/* File-related functions from system-dependent file i/o module */

_PROTOTYP( int zkself, (void) );
_PROTOTYP( int zopeni, (int, char *) );
_PROTOTYP( int zopeno, (int, char *, struct zattr *, struct filinfo *) );
_PROTOTYP( int zclose, (int) );
#ifndef MAC
_PROTOTYP( int zchin, (int, int *) );
#endif /* MAC */
_PROTOTYP( int zsinl, (int, char *, int) );
_PROTOTYP( int zinfill, (void) );
_PROTOTYP( int zsout, (int, char*) );
_PROTOTYP( int zsoutl, (int, char*) );
_PROTOTYP( int zsoutx, (int, char*, int) );
_PROTOTYP( int zchout, (int, char) );
_PROTOTYP( int zoutdump, (void) );
_PROTOTYP( int zsyscmd, (char *) );
_PROTOTYP( int zshcmd, (char *) );
_PROTOTYP( int chkfn, (int) );
_PROTOTYP( long zchki, (char *) );
_PROTOTYP( int iswild, (char *) );
_PROTOTYP( int zchko, (char *) );
_PROTOTYP( int zdelet, (char *) );
_PROTOTYP( VOID zrtol, (char *,char *) );
_PROTOTYP( VOID zltor, (char *,char *) );
_PROTOTYP( VOID zstrip, (char *,char **) );
_PROTOTYP( int zchdir, (char *) );
_PROTOTYP( char * zhome, (void) );
_PROTOTYP( char * zgtdir, (void) );
_PROTOTYP( int zxcmd, (int, char *) );
#ifndef MAC
_PROTOTYP( int zclosf, (int) );
#endif /* MAC */
_PROTOTYP( int zxpand, (char *) );
_PROTOTYP( int znext, (char *) );
_PROTOTYP( int zchkspa, (char *, long) );
_PROTOTYP( VOID znewn, (char *, char **) );
_PROTOTYP( int zrename, (char *, char *) );
_PROTOTYP( int zsattr, (struct zattr *) );
_PROTOTYP( int zfree, (char *) );
_PROTOTYP( char * zfcdat, (char *) );
_PROTOTYP( int zstime, (char *, struct zattr *, int) );
_PROTOTYP( int zmail, (char *, char *) ); 
_PROTOTYP( int zprint, (char *, char *) ); 
_PROTOTYP( char * tilde_expand, (char *) ); 

/* Functions from system-dependent terminal i/o module */

_PROTOTYP( int ttopen, (char *, int *, int, int) );  /* tty functions */
#ifndef MAC
_PROTOTYP( int ttclos, (int) );
#endif /* MAC */
_PROTOTYP( int tthang, (void) );
_PROTOTYP( int ttres, (void) );
_PROTOTYP( int ttpkt, (long, int, int) );
#ifndef MAC
_PROTOTYP( int ttvt, (long, int) );
#endif /* MAC */
_PROTOTYP( int ttsspd, (int) );
_PROTOTYP( long ttgspd, (void) );
_PROTOTYP( int ttflui, (void) );
_PROTOTYP( int ttfluo, (void) );
_PROTOTYP( int ttchk, (void) );
_PROTOTYP( int ttxin, (int, CHAR *) );
_PROTOTYP( int ttol, (CHAR *, int) );
_PROTOTYP( int ttoc, (char) );
_PROTOTYP( int ttinc, (int) );
_PROTOTYP( int ttscarr, (int) );
_PROTOTYP( int ttgmdm, (void) );
_PROTOTYP( int ttsndb, (void) );
_PROTOTYP( int ttsndlb, (void) );
#ifdef PARSENSE
#ifdef UNIX
_PROTOTYP( int ttinl, (CHAR *, int, int, CHAR, CHAR, int) );
#else
#ifdef VMS
_PROTOTYP( int ttinl, (CHAR *, int, int, CHAR, CHAR, int) );
#else
_PROTOTYP( int ttinl, (CHAR *, int, int, CHAR, CHAR) );
#endif /* VMS */
#endif /* UNIX */
#else
_PROTOTYP( int ttinl, (CHAR *, int, int, CHAR) );
#endif /* PARSENSE */

/* Console functions */

_PROTOTYP( int congm, (void) );
#ifdef COMMENT
_PROTOTYP( VOID conint, (SIGTYP (*)(int, int), SIGTYP (*)(int, int)) );
#else
_PROTOTYP( VOID conint, (SIGTYP (*)(int), SIGTYP (*)(int)) );
#endif
_PROTOTYP( VOID connoi, (void) );
_PROTOTYP( int concb, (char) );
_PROTOTYP( int conbin, (char) );
_PROTOTYP( int conres, (void) );
_PROTOTYP( int conoc, (char) );
_PROTOTYP( int conxo, (int, char *) );
_PROTOTYP( int conol, (char *) );
_PROTOTYP( int conola, (char *[]) );
_PROTOTYP( int conoll, (char *) );
_PROTOTYP( int conchk, (void) );
_PROTOTYP( int coninc, (int) );
_PROTOTYP( int psuspend, (int) );
_PROTOTYP( int priv_ini, (void) );
_PROTOTYP( int priv_on, (void) );
_PROTOTYP( int priv_off, (void) );
_PROTOTYP( int priv_can, (void) );
_PROTOTYP( int priv_chk, (void) );
_PROTOTYP( int priv_opn, (char *, int) );

_PROTOTYP( int sysinit, (void) );	/* Misc Kermit functions */
_PROTOTYP( int syscleanup, (void) );
_PROTOTYP( int msleep, (int) );
_PROTOTYP( VOID rtimer, (void) );
_PROTOTYP( int gtimer, (void) );
_PROTOTYP( VOID ttimoff, (void) );
_PROTOTYP( VOID ztime, (char **) );
_PROTOTYP( int parchk, (CHAR *, CHAR, int) );
_PROTOTYP( VOID doexit, (int, int) );
_PROTOTYP( int askmore, (void) );
_PROTOTYP( VOID fatal, (char *) );

/* Key mapping support */

#ifdef NOICP
#ifndef NOSETKEY
#define NOSETKEY
#endif /* NOSETKEY */
#endif /* NOICP */

_PROTOTYP( int congks, (int) );
#ifndef NOSETKEY
#ifdef OS2
#define KMSIZE 768
typedef int KEY;
typedef CHAR *MACRO;
extern int wideresult;
_PROTOTYP( VOID keymapinit, (void) );
#else /* Not OS2 */
/*
  Catch-all for systems where we don't know how to read keyboard scan
  codes > 255.  Note: CHAR (i.e. unsigned char) is very important here.
*/
#define KMSIZE 256
typedef CHAR KEY;
typedef CHAR * MACRO;
#define congks coninc
#endif /* OS2 */
#endif /* NOSETKEY */

/*
  Function prototypes for system and library functions.
*/
#ifdef _POSIX_SOURCE
#ifndef VMS
#ifndef MAC
#define CK_ANSILIBS
#endif /* MAC */
#endif /* VMS */
#endif /* _POSIX_SOURCE */

#ifdef NEXT
#define CK_ANSILIBS
#endif /* NEXT */

#ifdef SVR4
#define CK_ANSILIBS
#endif /* SVR4 */

#ifdef OS2
#define KANJI
#define CK_ANSILIBS
#define MYCURSES
#define CK_RTSCTS
#ifdef __IBMC__
#define S_IFMT 0xF000
#define timezone _timezone
#endif /* __IBMC__ */
#include <io.h>
#ifdef __EMX__
#ifndef __32BIT__
#define __32BIT__
#endif /* __32BIT__ */
#include <sys/timeb.h>
#else
#include <direct.h>
#define SIGALRM SIGUSR1
_PROTOTYP( unsigned alarm, (unsigned) );
_PROTOTYP( unsigned sleep, (unsigned) );
#endif /* __EMX__ */
_PROTOTYP( long zdskspace, (int) );
_PROTOTYP( int zchdsk, (int) );
_PROTOTYP( int conincraw, (int) );
_PROTOTYP( int ttiscom, (int f) );
_PROTOTYP( int IsFileNameValid, (char *) );
_PROTOTYP( void ChangeNameForFAT, (char *) );
_PROTOTYP( char *GetLoadPath, (void) );
#endif /* OS2 */

#ifdef MYCURSES				/* MYCURSES implies CK_CURSES */
#ifndef CK_CURSES
#define CK_CURSES
#endif /* CK_CURSES */
#endif /* MYCURSES */

#ifdef CK_ANSILIBS
/*
  String library functions.
  For ANSI C, get prototypes from <string.h>.
  Otherwise, skip the prototypes.
*/
#include <string.h>

/*
  Prototypes for other commonly used library functions, such as
  malloc, free, getenv, atol, atoi, and exit.  Otherwise, no prototypes.
*/
#include <stdlib.h>
#ifdef DIAB /* DIAB DS90 */
/* #include <commonC.h>  */
#include <sys/wait.h>
#ifdef COMMENT
extern void exit(int status);
extern void _exit(int status);
extern int uname(struct utsname *name);
#endif /* COMMENT */
extern int chmod(char *path, int mode);
extern int ioctl(int fildes, int request, ...);
extern int rdchk(int ttyfd);
extern int nap(int m);
#ifdef COMMENT
extern int getppid(void);
#endif /* COMMENT */
extern int _filbuf(FILE *stream);
extern int _flsbuf(char c,FILE *stream);
#endif /* DIAB */

/*
  Prototypes for UNIX functions like access, alarm, chdir, sleep, fork,
  and pause.  Otherwise, no prototypes.
*/
#ifdef NEXT
#include <libc.h>
#else
#ifndef AMIGA
#ifndef OS2
#include <unistd.h>
#endif /* OS2 */
#endif /* AMIGA */
#endif /* NEXT */

#else /* Not ANSI libs... */

#ifdef MAC
#include <String.h>
#include <StdLib.h>
#endif /* MAC */

#ifdef SUNOS41
#include <unistd.h>
#include <stdlib.h>
#else
/*
  It is essential that these are declared correctly!
*/
_PROTOTYP( char * malloc, (unsigned int) );
_PROTOTYP( char * getenv, (char *) );
_PROTOTYP( long atol, (char *) );
#endif /* SUNOS4 */
#endif /* CK_ANSILIBS */

#ifndef NULL				/* In case NULL is still not defined */
#define NULL 0L
/* or #define NULL 0 */
/* or #define NULL ((char *) 0) */
/* or #define NULL ((void *) 0) */
#endif /* NULL */

/* Funny names for library functions department... */

#ifdef ZILOG
#define setjmp setret
#define longjmp longret
#define jmp_buf ret_buf
#define getcwd curdir
#endif /* ZILOG */

#endif /* CKCDEB_H */

/* End of ckcdeb.h */
