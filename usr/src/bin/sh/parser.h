/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)parser.h	8.1 (Berkeley) %G%
 */

/* control characters in argument strings */
#define CTLESC '\201'
#define CTLVAR '\202'
#define CTLENDVAR '\203'
#define CTLBACKQ '\204'
#define CTLQUOTE 01		/* ored with CTLBACKQ code if in quotes */
/*	CTLBACKQ | CTLQUOTE == '\205' */
#define	CTLARI	'\206'
#define	CTLENDARI '\207'

/* variable substitution byte (follows CTLVAR) */
#define VSTYPE 07		/* type of variable substitution */
#define VSNUL 040		/* colon--treat the empty string as unset */
#define VSQUOTE 0100		/* inside double quotes--suppress splitting */

/* values of VSTYPE field */
#define VSNORMAL 1		/* normal variable:  $var or ${var} */
#define VSMINUS 2		/* ${var-text} */
#define VSPLUS 3		/* ${var+text} */
#define VSQUESTION 4		/* ${var?message} */
#define VSASSIGN 5		/* ${var=text} */


/*
 * NEOF is returned by parsecmd when it encounters an end of file.  It
 * must be distinct from NULL, so we use the address of a variable that
 * happens to be handy.
 */
extern int tokpushback;
#define NEOF ((union node *)&tokpushback)
extern int whichprompt;		/* 1 == PS1, 2 == PS2 */


#ifdef __STDC__
union node *parsecmd(int);
int goodname(char *);
char *getprompt(void *);
#else
union node *parsecmd();
int goodname();
char *getprompt();
#endif
