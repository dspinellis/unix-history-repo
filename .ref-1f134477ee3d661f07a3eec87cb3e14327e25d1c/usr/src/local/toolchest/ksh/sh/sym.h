/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)sym.h	1.1 */
/*
 *	UNIX shell
 *	S. R. Bourne
 *	Rewritten by David Korn
 */


/* symbols for parsing */
#define DOSYM	0405
#define FISYM	0420
#define EFSYM	0422
#define ELSYM	0421
#define INSYM	0412
#define BRSYM	0406
#define KTSYM	0450
#define THSYM	0444
#define ODSYM	0441
#define ESSYM	0442
#define IFSYM	0436
#define FORSYM	0435
#define WHSYM	0433
#define UNSYM	0427
#define CASYM	0417
#define PROCSYM	0460
#define SELSYM	0470
#define TIMSYM	0474

#define SYMREP	04000
#define ECSYM	(SYMREP|';')
#define ANDFSYM	(SYMREP|'&')
#define ORFSYM	(SYMREP|'|')
#define APPSYM	(SYMREP|'>')
#define DOCSYM	(SYMREP|'<')
#define SYMALT1	01000
#define SYMALT2	010000
#define COOPSYM	(SYMALT1|'|')
#define IPROC	(SYMALT1|'(')
#define OPROC	(SYMALT2|'(')
#define EOFSYM	02000
#define SYMFLG	0400

/* arg to `cmd' */
#define NLFLG	1
#define MTFLG	2

/* for peekc */
#define MARK	0100000

/* odd chars */
#define DQUOTE	'"'
#define SQUOTE	'`'
#define DOLLAR	'$'
#define BRACE	'{'
#define LPAREN	'('
#define RPAREN	')'

struct sysnod
{
#ifdef apollo
	/* pointers can not be in readonly sections */
	char   sysnam[28];
#else
	char	*sysnam;
#endif	/* apollo */
	unsigned sysval;
};

#define A_RAW	1		/* string needs no processing */
#define A_MAKE	2		/* bit set during argument expansion */
#define A_MAC	4		/* string needs macro expansion */
#define	A_EXP	8		/* string needs file expansion */

/* dummy for access only */
struct argnod 
{
	struct argnod	*argnxt;
	struct argnod	*argchn;
	char	 argflag;
	char	argval[1];
};

typedef struct sysnod	SYSTAB[];
typedef struct argnod	*ARGPTR;

extern int	wdval;
extern int	wdnum;
extern ARGPTR	wdarg;
extern ARGPTR	gchain;
extern int	subflag;
extern SYSTAB	reserved;
extern SYSTAB	commands;
extern SYSTAB	option_flags;
extern SYSTAB	signal_names;
extern SYSTAB	sig_messages;
extern SYSTAB	testops;
extern SYSTAB	attributes;
extern MSG	let_syntax;
extern MSG	unexpected;
extern MSG	unmatched;
#ifdef DEVFD
extern MSG	devfd;
#endif	/* DEVFD */
