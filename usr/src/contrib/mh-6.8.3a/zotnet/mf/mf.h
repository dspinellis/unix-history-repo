/* mf.h - include file for mailbox filters */
/* @(#)$Id: mf.h,v 1.3 1992/12/15 00:20:22 jromine Exp $ */

#include "../h/strings.h"

#ifndef	TRUE
#define	TRUE	1
#endif	/* TRUE */
#ifndef	FALSE
#define	FALSE	0
#endif	/* FALSE */

#ifndef	NOTOK
#define	NOTOK	(-1)
#endif	/* NOTOK */
#ifndef	OK
#define	OK	0
#endif	/* OK */
#ifndef	DONE
#define	DONE	1
#endif	/* DONE */

#define	LINESIZ	512

#define	MBXMODE	0600
#define	TMPMODE	0600

#define	OWIDTH	75		/* length of a header line */

#define	HFROM	1		/* header has From: component	 */
#define	HSNDR	2		/* header has Sender: component  */
#define	HADDR	3		/* header has address component	 */
#define	HDATE	4		/* header has Date: component	 */
#define	HOTHR	5		/* header is unimportant	 */

/*  */

struct adrx {
    char   *text;
    char   *pers;
    char   *mbox;
    char   *host;
    char   *path;
    char   *grp;
    int     ingrp;
    char   *note;
    char   *err;
};

/*  */

/* 
 *    Codes returned by uucp2mmdf(), mmdf2uucp()
 */

#define	MFOK	0		/* all went well		 */
 /* remaining codes must > DONE	 */
#define	MFPRM	2		/* bad parameter		 */
#define	MFSIO	3		/* stdio package went screwy	 */
#define	MFROM	4		/* from line was bad		 */
#define	MFHDR	5		/* headers were bad		 */
#define	MFTXT	6		/* text was bad			 */
#define	MFERR	7		/* I/O or system error		 */
#define	MFDLM	8		/* Bad delimiter in MMDF file	 */


int     isfrom (), lequal (), mfgets ();
char   *legal_person ();
struct adrx *seekadrx (), *getadrx (), *uucpadrx ();
