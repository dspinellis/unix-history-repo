/*  C K C D E B . H  */

/*
 This file is included by all C-Kermit modules, including the modules
 that aren't specific to Kermit (like the command parser and the ck?tio and
 ck?fio modules.  It specifies format codes for debug(), tlog(), and similar
 functions, and includes any necessary typedefs to be used by all C-Kermit
 modules, and also includes some feature selection compile-time switches.
*/
/*
 Copyright (C) 1985, Trustees of Columbia University in the City of New York.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as it is not sold for profit, provided this
 copyright notice is retained.
*/

/*
 DEBUG and TLOG should be defined in the Makefile if you want debugging
 and transaction logs.  Don't define them if you want to save the space
 and overhead.
*/
#ifndef DEBUG
#define debug(a,b,c,d) {}
#endif

#ifndef TLOG
#define tlog(a,b,c,d) {}
#endif

/* Formats for debug(), tlog(), etc */

#define F000 0

#define F001 1
#define F010 2
#define F011 3
#define F100 4
#define F101 5
#define F110 6
#define F111 7

/* Compiler dependencies */

#ifdef PROVX1
typedef char CHAR;
typedef long LONG;
typedef int void;
#else
#ifdef	V7
typedef char CHAR;
typedef long LONG;
#else
#ifdef	C70
typedef char CHAR;
typedef long LONG;
#else
typedef unsigned char CHAR;
typedef long LONG;
#endif
#endif
#endif

#ifdef TOWER1
typedef int void;
#endif

/* Line delimiter for text files */

/*
 If the system uses a single character for text file line delimitation,
 define NLCHAR to the value of that character.  For text files, that
 character will be converted to CRLF upon output, and CRLF will be converted
 to that character on input.
*/
#ifdef MAC				/* Macintosh */
#define NLCHAR 015
#else					/* All Unix-like systems */
#define NLCHAR 012
#endif
/*
 At this point, if there's a system that uses ordinary CRLF line
 delimitation AND the C compiler actually returns both the CR and
 the LF when doing input from a file, then #undef NLCHAR.
*/

/* The device name of a job's controlling terminal */
/* Special for VMS, same for all Unixes (?), not used by Macintosh */

#ifdef vax11c
#define CTTNAM "TT:"
#else
#define CTTNAM "/dev/tty"
#endif



/* Some special includes for VAX/VMS */

#ifdef vax11c
#include ssdef
#include stsdef
#endif

/* Program return codes for VMS, DECUS C, and Unix */

#ifdef vax11c
#define	GOOD_EXIT   (SS$_NORMAL | STS$M_INHIB_MSG)
#define	BAD_EXIT    SS$_ABORT
#else
#ifdef decus
#define	GOOD_EXIT   IO_NORMAL
#define	BAD_EXIT    IO_ERROR
#else
#define GOOD_EXIT   0
#define BAD_EXIT    1
#endif
#endif
