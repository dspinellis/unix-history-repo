
/*
 *                     RCS common definitions and data structures
 */
#define RCSBASE "$Id: rcsbase.h,v 4.9 89/05/01 15:17:14 narten Exp $"

/* Copyright (C) 1982, 1988, 1989 Walter Tichy
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Walter Tichy.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Report all problems and direct all questions to:
 *   rcs-bugs@cs.purdue.edu
 * 







*/



/*****************************************************************************
 * INSTRUCTIONS:
 * =============
 * The following should be handled in the Makefile:
 *     For USG Unix, define USG; for BSD Unix, don't (see ifdef USG).
 *     For 4.2 bsd, define V4_2BSD; this will replace the routines
 *     getwd() and rename() with the corresponding ones in the C-library.
 *     V4_2BSD also selects different definitions for the macros NCPFN and NCPPN
 *     (max. number of characters per filename, number of characters per path name).
 *     Define STRICT_LOCKING appropriately (see STRICT_LOCKING).
 * The following need be changed for porting to a different machine:
 *     Define SMALLOG for a machine with small memory (like the PDP11).
 *     SMALLOG conserves space for log messages.
 *     Change BYTESIZ if necessary.
 * If you need to change the comment leaders, update the table comtable[]
 * in rcsfnms.c. (This can wait until you know what a comment leader is.)
 *****************************************************************************
 */


/* $Log:	rcsbase.h,v $
 * Revision 4.9  89/05/01  15:17:14  narten
 * botched previous USG fix 
 * 
 * Revision 4.8  89/05/01  14:53:05  narten
 * changed #include <strings.h> -> string.h for USG systems.
 * 
 * Revision 4.7  88/11/08  15:58:45  narten
 * removed defs for functions loaded from libraries
 * 
 * Revision 4.6  88/11/08  12:04:06  narten
 * changes from eggert@sm.unisys.com (Paul Eggert)
 * 
 * Revision 4.6  88/08/09  19:12:36  eggert
 * Shrink stdio code size; remove lint; permit -Dhshsize=nn.
 * 
 * Revision 4.5  87/12/18  17:06:41  narten
 * made removed BSD ifdef, now uses V4_2BSD
 * 
 * Revision 4.4  87/10/18  10:29:49  narten
 * Updating version numbers
 * Changes relative to 1.1 are actually relative to 4.2
 * 
 * Revision 1.3  87/09/24  14:02:25  narten
 * changes for lint
 * 
 * Revision 1.2  87/03/27  14:22:02  jenkins
 * Port to suns
 * 
 * Revision 1.1  84/01/23  14:50:14  kcs
 * Initial revision
 * 
 * Revision 4.2  83/12/20  16:04:20  wft
 * merged 3.6.1.1 and 4.1 (SMALLOG, logsize).
 * moved setting of STRICT_LOCKING to Makefile.
 * changed DOLLAR to UNKN (conflict with KDELIM).
 * 
 * Revision 4.1  83/05/04  09:12:41  wft
 * Added markers Id and RCSfile.
 * Added Dbranch for default branches.
 * 
 * Revision 3.6.1.1  83/12/02  21:56:22  wft
 * Increased logsize, added macro SMALLOG.
 * 
 * Revision 3.6  83/01/15  16:43:28  wft
 * 4.2 prerelease
 * 
 * Revision 3.6  83/01/15  16:43:28  wft
 * Replaced dbm.h with BYTESIZ, fixed definition of rindex().
 * Added variants of NCPFN and NCPPN for bsd 4.2, selected by defining V4_2BSD.
 * Added macro DELNUMFORM to have uniform format for printing delta text nodes.
 * Added macro DELETE to mark deleted deltas.
 *
 * Revision 3.5  82/12/10  12:16:56  wft
 * Added two forms of DATEFORM, one using %02d, the other %.2d.
 *
 * Revision 3.4  82/12/04  20:01:25  wft
 * added LOCKER, Locker, and USG (redefinition of rindex).
 *
 * Revision 3.3  82/12/03  12:22:04  wft
 * Added dbm.h, stdio.h, RCSBASE, RCSSEP, RCSSUF, WORKMODE, TMPFILE3,
 * PRINTDATE, PRINTTIME, map, and ctab; removed Suffix. Redefined keyvallength
 * using NCPPN. Changed putc() to abort on write error.
 *
 * Revision 3.2  82/10/18  15:03:52  wft
 * added macro STRICT_LOCKING, removed RCSUMASK.
 * renamed JOINFILE[1,2] to JOINFIL[1,2].
 *
 * Revision 3.1  82/10/11  19:41:17  wft
 * removed NBPW, NBPC, NCPW.
 * added typdef int void to aid compiling
 */



#include <stdio.h>
#ifdef USG
#include <string.h>
#else
#include <strings.h>
#endif
#undef putc         /* will be redefined */


#ifdef USG
#       define rindex    strrchr
#       define DATEFORM  "%.2d.%.2d.%.2d.%.2d.%.2d.%.2d"
#else
#       define DATEFORM  "%02d.%02d.%02d.%02d.%02d.%02d"
#endif
/* Make sure one of %02d or %.2d prints a number with a field width 2, with
 * leading zeroes. For example, 0, 1, and 22 must be printed as 00, 01, and
 * 22. Otherwise, there will be problems with the dates.
 */

#define PRINTDATE(file,date) fprintf(file,"%.2s/%.2s/%.2s",date,date+3,date+6)
#define PRINTTIME(file,date) fprintf(file,"%.2s:%.2s:%.2s",date+9,date+12,date+15)
/* print RCS format date and time in nice format from a string              */

/*
 * Parameters
 */
#define BYTESIZ             8 /* number of bits in a byte                   */

/*#define STRICT_LOCKING    0 /* 0 sets the default locking to non-strict;  */
                              /* used in experimental environments.         */
                              /* 1 sets the default locking to strict;      */
                              /* used in production environments.           */
			      /* STRICT_LOCKING is set in the Makefile!     */
#ifndef hshsize
#define hshsize           239 /* hashtable size; MUST be prime and -1 mod 4 */
                              /* other choices: 547 or 719                  */
#endif

#define strtsize (hshsize * 50) /* string table size                        */
#ifdef SMALLOG
#  define logsize         1024 /* max. size of log message for pdp11        */
#else
#  define logsize         4096 /* max. size of log message for others       */
#endif
#define revlength          30 /* max. length of revision numbers            */
#define datelength         20 /* length of a date in RCS format             */
#define joinlength         20 /* number of joined revisions permitted       */
#define RCSDIR         "RCS/" /* subdirectory for RCS files                 */
#define RCSSUF            'v' /* suffix for RCS files                       */
#define RCSSEP            ',' /* separator for RCSSUF                       */
#define KDELIM            '$' /* delimiter for keywords                     */
#define VDELIM            ':' /* separates keywords from values             */
#define DEFAULTSTATE    "Exp" /* default state of revisions                 */
#ifdef V4_2BSD
#  define NCPFN           256 /* number of characters per filename          */
#  define NCPPN          1024 /* number of characters per pathname          */
#else
#  define NCPFN            14 /* number of characters per filename          */
#  define NCPPN       6*NCPFN /* number of characters per pathname          */
#endif
#define keylength          20 /* buffer length for expansion keywords       */
#define keyvallength NCPPN+revlength+datelength+60
                              /* buffer length for keyword expansion        */



#define true     1
#define false    0
#define nil      0
#define elsif    else if
#define elif     else if


/* temporary file names */

#define NEWRCSFILE  ",RCSnewXXXXXX"
#define DIFFILE     ",RCSciXXXXXX"
#define TMPFILE1    ",RCSt1XXXXXX"
#define TMPFILE2    ",RCSt2XXXXXX"
#define TMPFILE3    ",RCSt3XXXXXX"
#define JOINFIL2    ",RCSj2XXXXXX"
#define JOINFIL3    ",RCSj3XXXXXX"


#define putc(x,p) (--(p)->_cnt>=0? ((int)(*(p)->_ptr++=(unsigned)(x))):fflsbuf((unsigned)(x),p))
/* This version of putc prints a char, but aborts on write error            */

#define GETC(in,out,echo) (c=getc(in), echo?putc(c,out):c)
/* GETC modifies a local variable c; a kludge, but smaller and faster.      */
/* GETC writes a del-character (octal 177) on end of file                   */

#define WORKMODE(RCSmode) (RCSmode&~0222)|((lockflag||!StrictLocks)?0600:0000)
/* computes mode of working file: same as RCSmode, but write permission     */
/* determined by lockflag and StrictLocks.                                  */


/* character classes and token codes */
enum tokens {
/* char classes*/  DIGIT, IDCHAR, NEWLN, LETTER, PERIOD, SBEGIN, SPACE, UNKN,
/* tokens */       COLON, DATE, EOFILE, ID, KEYW, NUM, SEMI, STRING,
};

#define AT      SBEGIN  /* class SBEGIN (string begin) is returned by lex. anal. */
#define SDELIM  '@'     /* the actual character is needed for string handling*/
/* these must be changed consistently, for instance to:
 * #define DQUOTE       SBEGIN
 * #define SDELIM       '"'
 * #define AT           IDCHAR
 * there should be no overlap among SDELIM, KDELIM, and VDELIM
 */

/* other characters */

#define ACCENT   IDCHAR
#define AMPER    IDCHAR
#define BACKSL   IDCHAR
#define BAR      IDCHAR
#define COMMA    UNKN
#define DIVIDE   IDCHAR
#define DOLLAR   UNKN                /* overlap with KDELIM */
#define DQUOTE   IDCHAR
#define EQUAL    IDCHAR
#define EXCLA    IDCHAR
#define GREAT    IDCHAR
#define HASH     IDCHAR
#define INSERT   UNKN
#define LBRACE   IDCHAR
#define LBRACK   IDCHAR
#define LESS     IDCHAR
#define LPARN    IDCHAR
#define MINUS    IDCHAR
#define PERCNT   IDCHAR
#define PLUS     IDCHAR
#define QUEST    IDCHAR
#define RBRACE   IDCHAR
#define RBRACK   IDCHAR
#define RPARN    IDCHAR
#define SQUOTE   IDCHAR
#define TILDE    IDCHAR
#define TIMES    IDCHAR
#define UNDER    IDCHAR
#define UPARR    IDCHAR




/***************************************
 * Data structures for the symbol table
 ***************************************/


/* Hash table entry */
struct hshentry {
        char              * num;      /* pointer to revision number (ASCIZ) */
        char              * date;     /* pointer to date of checking        */
        char              * author;   /* login of person checking in        */
        char              * lockedby; /* who locks the revision             */
        char              * log;      /* log message requested at checkin   */
        char              * state;    /* state of revision (Exp by default) */
        struct branchhead * branches; /* list of first revisions on branches*/
        struct hshentry   * next;     /* next revision on same branch       */
        int                 insertlns;/* lines inserted (computed by rlog)  */
        int                 deletelns;/* lines deleted  (computed by rlog)  */
        char                selector; /* marks entry for selection/deletion */
};

/* list element for branch lists */
struct branchhead {
        struct hshentry   * hsh;
        struct branchhead * nextbranch;
};

/* accesslist element */
struct access {
        char              * login;
        struct access     * nextaccess;
};

/* list element for locks  */
struct lock {
        char              * login;
        struct hshentry   * delta;
        struct lock       * nextlock;
};

/* list element for symbolic names */
struct assoc {
        char              * symbol;
        struct hshentry   * delta;
        struct assoc      * nextassoc;
};


/* common variables (getadmin and getdelta())*/
extern char            * Comment;
extern struct access   * AccessList;
extern struct assoc    * Symbols;
extern struct lock     * Locks;
extern struct hshentry * Head;
extern struct hshentry * Dbranch;
extern int               StrictLocks;
extern int               TotalDeltas;
#ifndef lint
static char copyright[]="Copyright (C) 1982 by Walter F. Tichy";
#endif

/* common variables (lexical analyzer)*/
extern enum tokens map[];
#define ctab (&map[1])
extern enum tokens       nexttok;
extern int               hshenter;
extern char            * NextString;
extern char            * cmdid;

#if defined(USG) || defined(V4_2BSD)
#define VOID	(void)
#else
typedef int void;
#define VOID
#endif

/* common routines */
extern char *talloc();
extern int serror();
extern int faterror();
extern int fatserror();
extern void ignoreints();
extern void catchints();
extern void restoreints();

#ifdef USG
extern int sprintf();
#endif

/*
 * Markers for keyword expansion (used in co and ident)
 */
#define AUTHOR          "Author"
#define DATE            "Date"
#define HEADER          "Header"
#define IDH             "Id"
#define LOCKER          "Locker"
#define LOG             "Log"
#define RCSFILE         "RCSfile"
#define REVISION        "Revision"
#define SOURCE          "Source"
#define STATE           "State"

enum markers { Nomatch, Author, Date, Header, Id,
	       Locker, Log, RCSfile, Revision, Source, State };

#define DELNUMFORM      "\n\n%s\n%s\n"
/* used by putdtext and scanlogtext */
#define DELETE          'D'
/* set by rcs -o and used by puttree() in rcssyn */

