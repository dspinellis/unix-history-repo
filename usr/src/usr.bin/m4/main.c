/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ozan Yigit.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * main.c
 * Facility: m4 macro processor
 * by: oz
 */

#include "mdef.h"

/*
 * m4 - macro processor
 *
 * PD m4 is based on the macro tool distributed with the software 
 * tools (VOS) package, and described in the "SOFTWARE TOOLS" and 
 * "SOFTWARE TOOLS IN PASCAL" books. It has been expanded to include 
 * most of the command set of SysV m4, the standard UN*X macro processor.
 *
 * Since both PD m4 and UN*X m4 are based on SOFTWARE TOOLS macro,
 * there may be certain implementation similarities between
 * the two. The PD m4 was produced without ANY references to m4
 * sources.
 *
 * References:
 *
 *	Software Tools distribution: macro
 *
 *	Kernighan, Brian W. and P. J. Plauger, SOFTWARE
 *	TOOLS IN PASCAL, Addison-Wesley, Mass. 1981
 *
 *	Kernighan, Brian W. and P. J. Plauger, SOFTWARE
 *	TOOLS, Addison-Wesley, Mass. 1976
 *
 *	Kernighan, Brian W. and Dennis M. Ritchie,
 *	THE M4 MACRO PROCESSOR, Unix Programmer's Manual,
 *	Seventh Edition, Vol. 2, Bell Telephone Labs, 1979
 *
 *	System V man page for M4
 *
 * Modification History:
 *
 * Jan 28 1986 Oz	Break the whole thing into little
 *			pieces, for easier (?) maintenance.
 *
 * Dec 12 1985 Oz	Optimize the code, try to squeeze
 *			few microseconds out..
 *
 * Dec 05 1985 Oz	Add getopt interface, define (-D),
 *			undefine (-U) options.
 *
 * Oct 21 1985 Oz	Clean up various bugs, add comment handling.
 *
 * June 7 1985 Oz	Add some of SysV m4 stuff (m4wrap, pushdef,
 *			popdef, decr, shift etc.).
 *
 * June 5 1985 Oz	Initial cut.
 *
 * Implementation Notes:
 *
 * [1]	PD m4 uses a different (and simpler) stack mechanism than the one 
 *	described in Software Tools and Software Tools in Pascal books. 
 *	The triple stack nonsense is replaced with a single stack containing 
 *	the call frames and the arguments. Each frame is back-linked to a 
 * 	previous stack frame, which enables us to rewind the stack after 
 * 	each nested call is completed. Each argument is a character pointer 
 *	to the beginning of the argument string within the string space.
 *	The only exceptions to this are (*) arg 0 and arg 1, which are
 * 	the macro definition and macro name strings, stored dynamically
 *	for the hash table.
 *
 *	    .					   .
 *	|   .	|  <-- sp			|  .  |
 *	+-------+				+-----+
 *	| arg 3 ------------------------------->| str |
 *	+-------+				|  .  |
 *	| arg 2 --------------+ 		   .
 *	+-------+	      |
 *	    *		      |			|     |
 *	+-------+	      | 		+-----+
 *	| plev	|  <-- fp     +---------------->| str |
 *	+-------+				|  .  |
 *	| type	|				   .
 *	+-------+
 *	| prcf	-----------+		plev: paren level
 *	+-------+  	   |		type: call type
 *	|   .	| 	   |		prcf: prev. call frame
 *	    .	   	   |
 *	+-------+	   |
 *	|	<----------+
 *	+-------+
 *
 * [2]	We have three types of null values:
 *
 *		nil  - nodeblock pointer type 0
 *		null - null string ("")
 *		NULL - Stdio-defined NULL
 *
 */

ndptr hashtab[HASHSIZE];	/* hash table for macros etc.  */
char buf[BUFSIZE];		/* push-back buffer	       */
char *bp = buf; 		/* first available character   */
char *endpbb = buf+BUFSIZE;	/* end of push-back buffer     */
stae mstack[STACKMAX+1]; 	/* stack of m4 machine         */
char strspace[STRSPMAX+1];	/* string space for evaluation */
char *ep = strspace;		/* first free char in strspace */
char *endest= strspace+STRSPMAX;/* end of string space	       */
int sp; 			/* current m4  stack pointer   */
int fp; 			/* m4 call frame pointer       */
FILE *infile[MAXINP];		/* input file stack (0=stdin)  */
FILE *outfile[MAXOUT];		/* diversion array(0=bitbucket)*/
FILE *active;			/* active output file pointer  */
char *m4temp;			/* filename for diversions     */
int ilevel = 0; 		/* input file stack pointer    */
int oindex = 0; 		/* diversion index..	       */
char *null = "";                /* as it says.. just a null..  */
char *m4wraps = "";             /* m4wrap string default..     */
char lquote = LQUOTE;		/* left quote character  (`)   */
char rquote = RQUOTE;		/* right quote character (')   */
char scommt = SCOMMT;		/* start character for comment */
char ecommt = ECOMMT;		/* end character for comment   */
struct keyblk keywrds[] = {	/* m4 keywords to be installed */
	"include",      INCLTYPE,
	"sinclude",     SINCTYPE,
	"define",       DEFITYPE,
	"defn",         DEFNTYPE,
	"divert",       DIVRTYPE,
	"expr",         EXPRTYPE,
	"eval",         EXPRTYPE,
	"substr",       SUBSTYPE,
	"ifelse",       IFELTYPE,
	"ifdef",        IFDFTYPE,
	"len",          LENGTYPE,
	"incr",         INCRTYPE,
	"decr",         DECRTYPE,
	"dnl",          DNLNTYPE,
	"changequote",  CHNQTYPE,
	"changecom",    CHNCTYPE,
	"index",        INDXTYPE,
#ifdef EXTENDED
	"paste",        PASTTYPE,
	"spaste",       SPASTYPE,
#endif
	"popdef",       POPDTYPE,
	"pushdef",      PUSDTYPE,
	"dumpdef",      DUMPTYPE,
	"shift",        SHIFTYPE,
	"translit",     TRNLTYPE,
	"undefine",     UNDFTYPE,
	"undivert",     UNDVTYPE,
	"divnum",       DIVNTYPE,
	"maketemp",     MKTMTYPE,
	"errprint",     ERRPTYPE,
	"m4wrap",       M4WRTYPE,
	"m4exit",       EXITTYPE,
	"syscmd",       SYSCTYPE,
	"sysval",       SYSVTYPE,
	"unix",         MACRTYPE,
};

#define MAXKEYS	(sizeof(keywrds)/sizeof(struct keyblk))

extern ndptr lookup();
extern ndptr addent();
extern int onintr();

extern char *malloc();
extern char *mktemp();

extern int optind;
extern char *optarg;

main(argc,argv)
char *argv[];
{
	register int c;
	register int n;
	char *p;

	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, onintr);
#ifdef NONZEROPAGES
	initm4();
#endif
	initkwds();

	while ((c = getopt(argc, argv, "tD:U:o:")) != EOF)
		switch(c) {

		case 'D':               /* define something..*/
			for (p = optarg; *p; p++)
				if (*p == '=')
					break;
			if (*p)
				*p++ = EOS;
			dodefine(optarg, p);
			break;
		case 'U':               /* undefine...       */
			remhash(optarg, TOP);
			break;
		case 'o':		/* specific output   */
		case '?':
		default:
			usage();
		}

	infile[0] = stdin;		/* default input (naturally) */
	active = stdout;		/* default active output     */
	m4temp = mktemp(DIVNAM);	/* filename for diversions   */

	sp = -1;			/* stack pointer initialized */
	fp = 0; 			/* frame pointer initialized */

	macro();			/* get some work done here   */

	if (*m4wraps) { 		/* anything for rundown ??   */
		ilevel = 0;		/* in case m4wrap includes.. */
		putback(EOF);		/* eof is a must !!	     */
		pbstr(m4wraps); 	/* user-defined wrapup act   */
		macro();		/* last will and testament   */
	}

	if (active != stdout)
		active = stdout;	/* reset output just in case */
	for (n = 1; n < MAXOUT; n++)	/* default wrap-up: undivert */
		if (outfile[n] != NULL)
			getdiv(n);
					/* remove bitbucket if used  */
	if (outfile[0] != NULL) {
		(void) fclose(outfile[0]);
		m4temp[UNIQUE] = '0';
		(void) unlink(m4temp);
	}

	exit(0);
}

ndptr inspect();	/* forward ... */

/*
 * macro - the work horse..
 *
 */
macro() {
	char token[MAXTOK];
	register char *s;
	register int t, l;
	register ndptr p;
	register int  nlpar;

	cycle {
		if ((t = gpbc()) == '_' || isalpha(t)) {
			putback(t);
			if ((p = inspect(s = token)) == nil) {
				if (sp < 0)
					while (*s)
						putc(*s++, active);
				else
					while (*s)
						chrsave(*s++);
			}
			else {
		/*
		 * real thing.. First build a call frame:
		 *
		 */
				pushf(fp);	/* previous call frm */
				pushf(p->type); /* type of the call  */
				pushf(0);	/* parenthesis level */
				fp = sp;	/* new frame pointer */
		/*
		 * now push the string arguments:
		 *
		 */
				pushs(p->defn);	      /* defn string */
				pushs(p->name);	      /* macro name  */
				pushs(ep);	      /* start next..*/

				putback(l = gpbc());
				if (l != LPAREN)  {   /* add bracks  */
					putback(RPAREN);
					putback(LPAREN);
				}
			}
		}
		else if (t == EOF) {
			if (sp > -1)
				error("m4: unexpected end of input");
			if (--ilevel < 0)
				break;			/* all done thanks.. */
			(void) fclose(infile[ilevel+1]);
			continue;
		}
	/*
	 * non-alpha single-char token seen..
	 * [the order of else if .. stmts is
	 * important.]
	 *
	 */
		else if (t == lquote) { 		/* strip quotes */
			nlpar = 1;
			do {
				if ((l = gpbc()) == rquote)
					nlpar--;
				else if (l == lquote)
					nlpar++;
				else if (l == EOF)
					error("m4: missing right quote");
				if (nlpar > 0) {
					if (sp < 0)
						putc(l, active);
					else
						chrsave(l);
				}
			}
			while (nlpar != 0);
		}

		else if (sp < 0) {		/* not in a macro at all */
			if (t == scommt) {	/* comment handling here */
				putc(t, active);
				while ((t = gpbc()) != ecommt)
					putc(t, active);
			}
			putc(t, active);	/* output directly..	 */
		}

		else switch(t) {

		case LPAREN:
			if (PARLEV > 0)
				chrsave(t);
			while (isspace(l = gpbc()))
				;		/* skip blank, tab, nl.. */
			putback(l);
			PARLEV++;
			break;

		case RPAREN:
			if (--PARLEV > 0)
				chrsave(t);
			else {			/* end of argument list */
				chrsave(EOS);

				if (sp == STACKMAX)
					error("m4: internal stack overflow");

				if (CALTYP == MACRTYPE)
					expand(mstack+fp+1, sp-fp);
				else
					eval(mstack+fp+1, sp-fp, CALTYP);

				ep = PREVEP;	/* flush strspace */
				sp = PREVSP;	/* previous sp..  */
				fp = PREVFP;	/* rewind stack...*/
			}
			break;

		case COMMA:
			if (PARLEV == 1)	{
				chrsave(EOS);		/* new argument   */
				while (isspace(l = gpbc()))
					;
				putback(l);
				pushs(ep);
			}
			break;
		default:
			chrsave(t);			/* stack the char */
			break;
		}
	}
}


/*
 * build an input token..
 * consider only those starting with _ or A-Za-z. This is a
 * combo with lookup to speed things up.
 */
ndptr
inspect(tp) 
register char *tp;
{
	register int h = 0;
	register char c;
	register char *name = tp;
	register char *etp = tp+MAXTOK;
	register ndptr p;

	while (tp < etp && (isalnum(c = gpbc()) || c == '_'))
		h += (*tp++ = c);
	putback(c);
	if (tp == etp)
		error("m4: token too long");
	*tp = EOS;
	for (p = hashtab[h%HASHSIZE]; p != nil; p = p->nxtptr)
		if (strcmp(name, p->name) == 0)
			break;
	return(p);
}

#ifdef NONZEROPAGES
/*
 * initm4 - initialize various tables. Useful only if your system 
 * does not know anything about demand-zero pages.
 *
 */
initm4()
{
	register int i;

	for (i = 0; i < HASHSIZE; i++)
		hashtab[i] = nil;
	for (i = 0; i < MAXOUT; i++)
		outfile[i] = NULL;
}
#endif

/*
 * initkwds - initialise m4 keywords as fast as possible. 
 * This very similar to install, but without certain overheads,
 * such as calling lookup. Malloc is not used for storing the 
 * keyword strings, since we simply use the static  pointers
 * within keywrds block. We also assume that there is enough memory 
 * to at least install the keywords (i.e. malloc won't fail).
 *
 */
initkwds() {
	register int i;
	register int h;
	register ndptr p;

	for (i = 0; i < MAXKEYS; i++) {
		h = hash(keywrds[i].knam);
		p = (ndptr) malloc(sizeof(struct ndblock));
		p->nxtptr = hashtab[h];
		hashtab[h] = p;
		p->name = keywrds[i].knam;
		p->defn = null;
		p->type = keywrds[i].ktyp | STATIC;
	}
}
