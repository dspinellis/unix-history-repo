/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)options.h	5.1 (Berkeley) %G%
 */

struct shparam {
	int nparam;	/* number of positional parameters (without $0) */
	char malloc;	/* true if parameter list dynamicly allocated */
	char **p;		/* parameter list */
	char **optnext;	/* next parameter to be processed by getopts */
	char *optptr;	/* used by getopts */
};



#define eflag optval[0]
#define fflag optval[1]
#define Iflag optval[2]
#define iflag optval[3]
#define jflag optval[4]
#define nflag optval[5]
#define sflag optval[6]
#define xflag optval[7]
#define zflag optval[8]
#define vflag optval[9]

#define NOPTS	10

#ifdef DEFINE_OPTIONS
const char optchar[NOPTS+1] = "efIijnsxzv";       /* shell flags */
char optval[NOPTS+1];           /* values of option flags */
#else
extern const char optchar[NOPTS+1];
extern char optval[NOPTS+1];
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
