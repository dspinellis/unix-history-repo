/*  C K U C M D . H  --  Header file for Unix cmd package  */
 
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
 
#ifndef CKUCMD_H
#define CKUCMD_H

/* Special getchars... */
 
#ifdef DYNAMIC				/* Dynamic command buffers */
#define DCMDBUF
/*
  Use malloc() to allocate the many command-related buffers in ckucmd.c.
*/
#endif /* DYNAMIC */

#ifdef VMS
#define getchar()   vms_getchar()
#endif /* VMS */
 
#ifdef aegis
#undef getchar
#define getchar()   coninc(0)
#endif /* aegis */
 
#ifdef AMIGA
#undef getchar
#define getchar() coninc(0)
#endif /* AMIGA */

/* Sizes of things */
 
#ifndef CMDDEP
#define CMDDEP  20			/* Maximum command recursion depth */
#endif /* CMDDEP */
#define HLPLW   78			/* Width of ?-help line */
#define HLPCW   19			/* Width of ?-help column */
#define HLPBL  100			/* Help string buffer length */
#define ATMBL  256			/* Command atom buffer length*/
#ifdef NOSPL
/* No script programming language, save some space */
#define CMDBL 512			/* Command buffer length */
#else
#define CMDBL 1024			/* Command buffer length */
#endif /* NOSPL */
 
/* Special characters */
 
#define RDIS 0022			/* Redisplay   (^R) */
#define LDEL 0025			/* Delete line (^U) */
#define WDEL 0027			/* Delete word (^W) */
 
/* Keyword table flags */
 
#define CM_INV 1			/* Invisible keyword */
#define CM_ABR 2			/* Abbreviation */
 
/* Token flags */

#define CMT_COM 0			/* Comment (; or #) */
#define CMT_SHE 1			/* Shell escape (!) */
#define CMT_LBL 2			/* Label (:) */
#define CMT_FIL 3			/* Indirect filespec (@) */

/* Keyword Table Template */
 
struct keytab {				/* Keyword table */
    char *kwd;				/* Pointer to keyword string */
    int kwval;				/* Associated value */
    int flgs;				/* Flags (as defined above) */
};

/* Function prototypes */

#ifdef CK_ANSIC				/* ANSI C */
#ifdef M_SYSV				/* SCO Microsoft C wants no args */
typedef int (*xx_strp)();
#else
typedef int (*xx_strp)(char *, char **, int *);
#endif /* M_SYSV */
#else					/* Not ANSI C */
typedef int (*xx_strp)();
#endif /* CK_ANSIC */

_PROTOTYP( int xxesc, (char **) );
_PROTOTYP( VOID cmsetp, (char *) );
_PROTOTYP( VOID cmsavp, (char [], int) );
_PROTOTYP( VOID prompt, (xx_strp) );
_PROTOTYP( VOID pushcmd, (void) );
_PROTOTYP( VOID cmres, (void) );
_PROTOTYP( VOID cmini, (int) );
_PROTOTYP( int cmpush, (void) );
_PROTOTYP( int cmpop, (void) );
_PROTOTYP( VOID untab, (char *) );
_PROTOTYP( int cmnum, (char *, char *, int, int *, xx_strp ) );
_PROTOTYP( int cmofi, (char *, char *, char **, xx_strp ) );
_PROTOTYP( int cmifi, (char *, char *, char **, int *, xx_strp ) );
_PROTOTYP( int cmdir, (char *, char *, char **, xx_strp ) );
_PROTOTYP( int cmfld, (char *, char *, char **, xx_strp ) );
_PROTOTYP( int cmtxt, (char *, char *, char **, xx_strp ) );
_PROTOTYP( int cmkey, (struct keytab [], int, char *, char *, xx_strp) );
_PROTOTYP( int cmkey2,(struct keytab [], int, char *, char *, char *,xx_strp));
_PROTOTYP( int chktok, (char *) );
_PROTOTYP( int cmcfm, (void) );
_PROTOTYP( int rdigits, (char *) );
_PROTOTYP( int chknum, (char *) );
_PROTOTYP( int lower, (char *) );
_PROTOTYP( int lookup, (struct keytab [], char *, int, int *) );
_PROTOTYP( int ungword, (void) );

#ifdef DCMDBUF
_PROTOTYP( int cmsetup, (void) );
#endif /* DCMDBUF */

#endif /* CKUCMD_H */

/* End of ckucmd.h */
