/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)options.h	8.1 (Berkeley) %G%
 */

struct shparam {
	int nparam;	/* number of positional parameters (without $0) */
	char malloc;	/* true if parameter list dynamicly allocated */
	char **p;		/* parameter list */
	char **optnext;	/* next parameter to be processed by getopts */
	char *optptr;	/* used by getopts */
};



#define eflag optlist[0].val
#define fflag optlist[1].val
#define Iflag optlist[2].val
#define iflag optlist[3].val
#define mflag optlist[4].val
#define nflag optlist[5].val
#define sflag optlist[6].val
#define xflag optlist[7].val
#define vflag optlist[8].val
#define Vflag optlist[9].val
#define	Eflag optlist[10].val
#define	Cflag optlist[11].val
#define	aflag optlist[12].val
#define	bflag optlist[13].val
#define	uflag optlist[14].val

#define NOPTS	15

struct optent {
	const char *name;
	const char letter;
	char val;
};

#ifdef DEFINE_OPTIONS
struct optent optlist[NOPTS] = {
	"errexit",	'e',	0,
	"noglob",	'f',	0,
	"ignoreeof",	'I',	0,
	"interactive",	'i',	0,
	"monitor",	'm',	0,
	"noexec",	'n',	0,
	"stdin",	's',	0,
	"xtrace",	'x',	0,
	"verbose",	'v',	0,
	"vi",		'V',	0,
	"emacs",	'E',	0,
	"noclobber",	'C',	0,
	"allexport",	'a',	0,
	"notify",	'b',	0,
	"nounset",	'u',	0,
};
#else
extern struct optent optlist[NOPTS];
#endif


extern char *minusc;		/* argument to -c option */
extern char *arg0;		/* $0 */
extern struct shparam shellparam;  /* $@ */
extern char **argptr;		/* argument list for builtin commands */
extern char *optarg;		/* set by nextopt */
extern char *optptr;		/* used by nextopt */


#ifdef __STDC__
void procargs(int, char **);
void setparam(char **);
void freeparam(struct shparam *);
int nextopt(char *);
#else
void procargs();
void setparam();
void freeparam();
int nextopt();
#endif
