/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)parseaddr.c	8.27 (Berkeley) %G%";
#endif /* not lint */

#include "sendmail.h"

#ifdef	CC_WONT_PROMOTE
static int toktype __P((char));
#else	/* !CC_WONT_PROMOTE */
static int toktype __P((int));				/* char -> int */
#endif	/* CC_WONT_PROMOTE */
static void _rewrite __P((char **, int));
static void callsubr __P((char **));
static ADDRESS * buildaddr __P((char **, ADDRESS *));
static void uurelativize __P((const char *, const char *, char **));

char	*DelimChar;		/* set to point to the delimiter */

/*
**  PARSEADDR -- Parse an address
**
**	Parses an address and breaks it up into three parts: a
**	net to transmit the message on, the host to transmit it
**	to, and a user on that host.  These are loaded into an
**	ADDRESS header with the values squirreled away if necessary.
**	The "user" part may not be a real user; the process may
**	just reoccur on that machine.  For example, on a machine
**	with an arpanet connection, the address
**		csvax.bill@berkeley
**	will break up to a "user" of 'csvax.bill' and a host
**	of 'berkeley' -- to be transmitted over the arpanet.
**
**	Parameters:
**		addr -- the address to parse.
**		a -- a pointer to the address descriptor buffer.
**			If NULL, a header will be created.
**		flags -- describe detail for parsing.  See RF_ definitions
**			in sendmail.h.
**		delim -- the character to terminate the address, passed
**			to prescan.
**		delimptr -- if non-NULL, set to the location of the
**			delim character that was found.
**		e -- the envelope that will contain this address.
**
**	Returns:
**		A pointer to the address descriptor header (`a' if
**			`a' is non-NULL).
**		NULL on error.
**
**	Side Effects:
**		none
*/

/* following delimiters are inherent to the internal algorithms */
# define DELIMCHARS	"()<>,;\r\n"	/* default word delimiters */

ADDRESS *
parseaddr(addr, a, flags, delim, delimptr, e)
	char *addr;
	register ADDRESS *a;
	int flags;
	int delim;
	char **delimptr;
	register ENVELOPE *e;
{
	register char **pvp;
	auto char *delimptrbuf;
	bool queueup;
	char pvpbuf[PSBUFSIZE];

	/*
	**  Initialize and prescan address.
	*/

	e->e_to = addr;
	if (tTd(20, 1))
		printf("\n--parseaddr(%s)\n", addr);

	{
		extern char *DelimChar;		/* parseaddr.c */
		char savec;
		bool invalid;
		extern char *finddelim();
		extern bool invalidaddr();

		DelimChar = finddelim(addr, delim);
		savec = *DelimChar;
		*DelimChar = '\0';
		invalid = invalidaddr(addr);
		*DelimChar = savec;
		if (invalid)
			return (NULL);
	}

	if (delimptr == NULL)
		delimptr = &delimptrbuf;

	pvp = prescan(addr, delim, pvpbuf, sizeof pvpbuf, delimptr);
	if (pvp == NULL)
	{
		if (tTd(20, 1))
			printf("parseaddr-->NULL\n");
		return (NULL);
	}

	if (invalidaddr(addr, delim == '\0' ? NULL : *delimptr))
	{
		if (tTd(20, 1))
			printf("parseaddr-->bad address\n");
		return NULL;
	}

	/*
	**  Save addr if we are going to have to.
	**
	**	We have to do this early because there is a chance that
	**	the map lookups in the rewriting rules could clobber
	**	static memory somewhere.
	*/

	if (bitset(RF_COPYPADDR, flags) && addr != NULL)
	{
		char savec = **delimptr;

		if (savec != '\0')
			**delimptr = '\0';
		addr = newstr(addr);
		if (savec != '\0')
			**delimptr = savec;
	}

	/*
	**  Apply rewriting rules.
	**	Ruleset 0 does basic parsing.  It must resolve.
	*/

	queueup = FALSE;
	if (rewrite(pvp, 3, 0, e) == EX_TEMPFAIL)
		queueup = TRUE;
	if (rewrite(pvp, 0, 0, e) == EX_TEMPFAIL)
		queueup = TRUE;


	/*
	**  Build canonical address from pvp.
	*/

	a = buildaddr(pvp, a, flags, e);

	/*
	**  Make local copies of the host & user and then
	**  transport them out.
	*/

	allocaddr(a, flags, addr);
	if (bitset(QBADADDR, a->q_flags))
		return a;

	/*
	**  If there was a parsing failure, mark it for queueing.
	*/

	if (queueup)
	{
		char *msg = "Transient parse error -- message queued for future delivery";

		if (tTd(20, 1))
			printf("parseaddr: queuing message\n");
		message(msg);
		if (e->e_message == NULL)
			e->e_message = newstr(msg);
		a->q_flags |= QQUEUEUP;
	}

	/*
	**  Compute return value.
	*/

	if (tTd(20, 1))
	{
		printf("parseaddr-->");
		printaddr(a, FALSE);
	}

	return (a);
}
/*
**  INVALIDADDR -- check for address containing meta-characters
**
**	Parameters:
**		addr -- the address to check.
**
**	Returns:
**		TRUE -- if the address has any "wierd" characters
**		FALSE -- otherwise.
*/

bool
invalidaddr(addr, delimptr)
	register char *addr;
	char *delimptr;
{
	char savedelim;

	if (delimptr != NULL)
	{
		savedelim = *delimptr;
		if (savedelim != '\0')
			*delimptr = '\0';
	}
#if 0
	/* for testing.... */
	if (strcmp(addr, "INvalidADDR") == 0)
	{
		usrerr("553 INvalid ADDRess");
		goto addrfailure;
	}
#endif
	for (; *addr != '\0'; addr++)
	{
		if ((*addr & 0340) == 0200)
			break;
	}
	if (*addr == '\0')
	{
		if (savedelim != '\0' && delimptr != NULL)
			*delimptr = savedelim;
		return FALSE;
	}
	setstat(EX_USAGE);
	usrerr("553 Address contained invalid control characters");
  addrfailure:
	if (savedelim != '\0' && delimptr != NULL)
		*delimptr = savedelim;
	return TRUE;
}
/*
**  ALLOCADDR -- do local allocations of address on demand.
**
**	Also lowercases the host name if requested.
**
**	Parameters:
**		a -- the address to reallocate.
**		flags -- the copy flag (see RF_ definitions in sendmail.h
**			for a description).
**		paddr -- the printname of the address.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Copies portions of a into local buffers as requested.
*/

allocaddr(a, flags, paddr)
	register ADDRESS *a;
	int flags;
	char *paddr;
{
	if (tTd(24, 4))
		printf("allocaddr(flags=%o, paddr=%s)\n", flags, paddr);

	a->q_paddr = paddr;

	if (a->q_user == NULL)
		a->q_user = "";
	if (a->q_host == NULL)
		a->q_host = "";

	if (bitset(RF_COPYPARSE, flags))
	{
		a->q_host = newstr(a->q_host);
		if (a->q_user != a->q_paddr)
			a->q_user = newstr(a->q_user);
	}

	if (a->q_paddr == NULL)
		a->q_paddr = a->q_user;
}
/*
**  INVALIDADDR -- check an address string for invalid control characters.
**
**	Parameters:
**		addr -- address string to be checked.
**
**	Returns:
**		TRUE if address string could cause problems, FALSE o/w.
**
**	Side Effects:
**		ExitStat may be changed and an error message generated.
*/

bool
invalidaddr(addr)
	const char *addr;
{
	register const char *cp;

	/* make sure error messages don't have garbage on them */
	errno = 0;

	/*
	** Sendmail reserves characters 020 - 036 for rewriting rules
	** which can cause havoc (e.g. infinite rewriting loops) if
	** one shows up at the wrong time.  If any of these characters
	** appear in an address, the address is deemed "invalid" and
	** an error message is generated.
	*/

	for (cp = addr; *cp; cp++)
		if ((*cp >= MATCHZANY && *cp <= HOSTEND) || *cp == '\001')
		{
			setstat(EX_USAGE);
			usrerr("address contained invalid control char(s)");
			return (TRUE);
		}
	return (FALSE);
}
/*
**  PRESCAN -- Prescan name and make it canonical
**
**	Scans a name and turns it into a set of tokens.  This process
**	deletes blanks and comments (in parentheses).
**
**	This routine knows about quoted strings and angle brackets.
**
**	There are certain subtleties to this routine.  The one that
**	comes to mind now is that backslashes on the ends of names
**	are silently stripped off; this is intentional.  The problem
**	is that some versions of sndmsg (like at LBL) set the kill
**	character to something other than @ when reading addresses;
**	so people type "csvax.eric\@berkeley" -- which screws up the
**	berknet mailer.
**
**	Parameters:
**		addr -- the name to chomp.
**		delim -- the delimiter for the address, normally
**			'\0' or ','; \0 is accepted in any case.
**			If '\t' then we are reading the .cf file.
**		pvpbuf -- place to put the saved text -- note that
**			the pointers are static.
**		pvpbsize -- size of pvpbuf.
**		delimptr -- if non-NULL, set to the location of the
**			terminating delimiter.
**
**	Returns:
**		A pointer to a vector of tokens.
**		NULL on error.
*/

/* states and character types */
# define OPR		0	/* operator */
# define ATM		1	/* atom */
# define QST		2	/* in quoted string */
# define SPC		3	/* chewing up spaces */
# define ONE		4	/* pick up one character */

# define NSTATES	5	/* number of states */
# define TYPE		017	/* mask to select state type */

/* meta bits for table */
# define M		020	/* meta character; don't pass through */
# define B		040	/* cause a break */
# define MB		M|B	/* meta-break */

static short StateTab[NSTATES][NSTATES] =
{
   /*	oldst	chtype>	OPR	ATM	QST	SPC	ONE	*/
	/*OPR*/		OPR|B,	ATM|B,	QST|B,	SPC|MB,	ONE|B,
	/*ATM*/		OPR|B,	ATM,	QST|B,	SPC|MB,	ONE|B,
	/*QST*/		QST,	QST,	OPR,	QST,	QST,
	/*SPC*/		OPR,	ATM,	QST,	SPC|M,	ONE,
	/*ONE*/		OPR,	OPR,	OPR,	OPR,	OPR,
};

/* token type table -- it gets modified with $o characters */
static TokTypeTab[256] =
{
	ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,SPC,SPC,SPC,SPC,SPC,ATM,ATM,
	ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,
	SPC,ATM,QST,ATM,ATM,ATM,ATM,ATM,ATM,SPC,ATM,ATM,ATM,ATM,ATM,ATM,
	ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,
	ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,
	ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,
	ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,
	ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,
	OPR,OPR,ONE,OPR,OPR,OPR,OPR,OPR,OPR,OPR,OPR,OPR,OPR,OPR,OPR,OPR,
	OPR,OPR,OPR,ONE,ONE,ONE,OPR,OPR,OPR,OPR,OPR,OPR,OPR,OPR,OPR,OPR,
	ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,
	ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,
	ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,
	ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,
	ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,
	ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,ATM,
};

#define toktype(c)	((int) TokTypeTab[(c) & 0xff])


# define NOCHAR		-1	/* signal nothing in lookahead token */

char **
prescan(addr, delim, pvpbuf, pvpbsize, delimptr)
	char *addr;
	char delim;
	char pvpbuf[];
	char **delimptr;
{
	register char *p;
	register char *q;
	register int c;
	char **avp;
	bool bslashmode;
	int cmntcnt;
	int anglecnt;
	char *tok;
	int state;
	int newstate;
	char *saveto = CurEnv->e_to;
	static char *av[MAXATOM+1];
	static char firsttime = TRUE;

	if (firsttime)
	{
		/* initialize the token type table */
		char obuf[50];

		firsttime = FALSE;
		expand("\201o", obuf, &obuf[sizeof obuf - sizeof DELIMCHARS], CurEnv);
		strcat(obuf, DELIMCHARS);
		for (p = obuf; *p != '\0'; p++)
		{
			if (TokTypeTab[*p & 0xff] == ATM)
				TokTypeTab[*p & 0xff] = OPR;
		}
	}

	/* make sure error messages don't have garbage on them */
	errno = 0;

	q = pvpbuf;
	bslashmode = FALSE;
	cmntcnt = 0;
	anglecnt = 0;
	avp = av;
	state = ATM;
	c = NOCHAR;
	p = addr;
	CurEnv->e_to = p;
	if (tTd(22, 11))
	{
		printf("prescan: ");
		xputs(p);
		(void) putchar('\n');
	}

	do
	{
		/* read a token */
		tok = q;
		for (;;)
		{
			/* store away any old lookahead character */
			if (c != NOCHAR && !bslashmode)
			{
				/* see if there is room */
				if (q >= &pvpbuf[pvpbsize - 5])
				{
					usrerr("553 Address too long");
	returnnull:
					if (delimptr != NULL)
						*delimptr = p;
					CurEnv->e_to = saveto;
					return (NULL);
				}

				/* squirrel it away */
				*q++ = c;
			}

			/* read a new input character */
			c = *p++;
			if (c == '\0')
			{
				/* diagnose and patch up bad syntax */
				if (state == QST)
				{
					usrerr("653 Unbalanced '\"'");
					c = '"';
				}
				else if (cmntcnt > 0)
				{
					usrerr("653 Unbalanced '('");
					c = ')';
				}
				else if (anglecnt > 0)
				{
					c = '>';
					usrerr("653 Unbalanced '<'");
				}
				else
					break;

				p--;
			}
			else if (c == delim && anglecnt <= 0 &&
					cmntcnt <= 0 && state != QST)
				break;

			if (tTd(22, 101))
				printf("c=%c, s=%d; ", c, state);

			/* chew up special characters */
			*q = '\0';
			if (bslashmode)
			{
				bslashmode = FALSE;

				/* kludge \! for naive users */
				if (cmntcnt > 0)
				{
					c = NOCHAR;
					continue;
				}
				else if (c != '!' || state == QST)
				{
					*q++ = '\\';
					continue;
				}
			}

			if (c == '\\')
			{
				bslashmode = TRUE;
			}
			if (state == QST)
			{
				/* do nothing, just avoid next clauses */
			}
			else if (c == '(')
			{
				cmntcnt++;
				c = NOCHAR;
			}
			else if (c == ')')
			{
				if (cmntcnt <= 0)
				{
					usrerr("653 Unbalanced ')'");
					c = NOCHAR;
				}
				else
					cmntcnt--;
			}
			else if (cmntcnt > 0)
				c = NOCHAR;
			else if (c == '<')
				anglecnt++;
			else if (c == '>')
			{
				if (anglecnt <= 0)
				{
					usrerr("653 Unbalanced '>'");
					c = NOCHAR;
				}
				else
					anglecnt--;
			}
			else if (delim == ' ' && isascii(c) && isspace(c))
				c = ' ';
			else if (c == ':' && !CurEnv->e_oldstyle)
			{
				/* consume characters until a semicolon */
				while (*p != '\0' && *p != ';')
					p++;
				if (*p == '\0')
					usrerr("Unbalanced ':...;' group spec");
				else
					p++;
				c = ' ';
			}

			else if (c == ';') /* semicolons are not tokens */
				c = NOCHAR;

			if (c == NOCHAR)
				continue;

			/* see if this is end of input */
			if (c == delim && anglecnt <= 0 && state != QST)
				break;

			newstate = StateTab[state][toktype(c)];
			if (tTd(22, 101))
				printf("ns=%02o\n", newstate);
			state = newstate & TYPE;
			if (bitset(M, newstate))
				c = NOCHAR;
			if (bitset(B, newstate))
				break;
		}

		/* new token */
		if (tok != q)
		{
			*q++ = '\0';
			if (tTd(22, 36))
			{
				printf("tok=");
				xputs(tok);
				(void) putchar('\n');
			}
			if (avp >= &av[MAXATOM])
			{
				syserr("553 prescan: too many tokens");
				goto returnnull;
			}
			if (q - tok > MAXNAME)
			{
				syserr("553 prescan: token too long");
				goto returnnull;
			}
			*avp++ = tok;
		}
	} while (c != '\0' && (c != delim || anglecnt > 0));
	*avp = NULL;
	p--;
	if (delimptr != NULL)
		*delimptr = p;
	if (tTd(22, 12))
	{
		printf("prescan==>");
		printav(av);
	}
	CurEnv->e_to = saveto;
	if (av[0] == NULL)
	{
		if (tTd(22, 1))
			printf("prescan: null leading token\n");
		return (NULL);
	}
	return (av);
}
/*
**  REWRITE -- apply rewrite rules to token vector.
**
**	This routine is an ordered production system.  Each rewrite
**	rule has a LHS (called the pattern) and a RHS (called the
**	rewrite); 'rwr' points the the current rewrite rule.
**
**	For each rewrite rule, 'avp' points the address vector we
**	are trying to match against, and 'pvp' points to the pattern.
**	If pvp points to a special match value (MATCHZANY, MATCHANY,
**	MATCHONE, MATCHCLASS, MATCHNCLASS) then the address in avp
**	matched is saved away in the match vector (pointed to by 'mvp').
**
**	When a match between avp & pvp does not match, we try to
**	back out.  If we back up over MATCHONE, MATCHCLASS, or MATCHNCLASS
**	we must also back out the match in mvp.  If we reach a
**	MATCHANY or MATCHZANY we just extend the match and start
**	over again.
**
**	When we finally match, we rewrite the address vector
**	and try over again.
**
**	Parameters:
**		pvp -- pointer to token vector.
**		ruleset -- the ruleset to use for rewriting.
**		reclevel -- recursion level (to catch loops).
**		e -- the current envelope.
**
**	Returns:
**		A status code.  If EX_TEMPFAIL, higher level code should
**			attempt recovery.
**
**	Side Effects:
**		pvp is modified.
*/

# define OP_NONZLEN	00001
# define OP_VARLEN	00002
# define OP_CLASS	00004
# define OP_EXACT	00010

struct match
{
	char	**first;	/* first token matched */
	char	**last;		/* last token matched */
	char	**pattern;	/* pointer to pattern */
	char	**source;	/* left hand source operand */
	char	flags;		/* attributes of this operator */
};

# define MAXMATCH	9	/* max params per rewrite */
# define MAX_CONTROL ' '

# ifndef MAXRULERECURSION
#  define MAXRULERECURSION	50	/* max recursion depth */
# endif
static char control_opts[MAX_CONTROL];


int
static char control_init_data[] = { 
	MATCHZANY,	OP_VARLEN,
	MATCHONE,	OP_NONZLEN,
	MATCHANY,	OP_VARLEN|OP_NONZLEN,
#ifdef MACVALUE
	MACVALUE,	OP_EXACT,
#endif /* MACVALUE */
	MATCHNCLASS,	OP_NONZLEN,
	MATCHCLASS,	OP_NONZLEN|OP_VARLEN|OP_CLASS,
};

static int nrw;

void
rewrite(pvp, ruleset, reclevel, e)
	char **pvp;
	int ruleset;
	int reclevel;
	register ENVELOPE *e;
{
	nrw = 0;
	_rewrite(pvp, ruleset);
}

static void
_rewrite(pvp, ruleset)
	char **pvp;
	int ruleset;
{
	register char *ap;		/* address pointer */
	register char *rp;		/* rewrite pointer */
	register char **avp;		/* address vector pointer */
	register char **rvp;		/* rewrite vector pointer */
	register struct match *mlp;	/* cur ptr into mlist */
	register struct rewrite *rwr;	/* pointer to current rewrite rule */
	int ruleno;			/* current rule number */
	int rstat = EX_OK;		/* return status */
	int loopcount;
	int subr;			/* subroutine number if >= 0 */
	bool dolookup;			/* do host aliasing */
	char *npvp[MAXATOM+1];		/* temporary space for rebuild */
	char tokbuf[MAXNAME+1];		/* for concatenated class tokens */
 	int nloops, nmatches = 0;	/* for looping rule checks */
	struct rewrite *prev_rwr;	/* pointer to previous rewrite rule */
	struct match mlist[MAXMATCH+1];	/* stores match on LHS */
	struct match *old_mlp;		/* to save our place */
	bool extend_match;	/* extend existing match during backup */

	if (OpMode == MD_TEST || tTd(21, 2))
	{
		printf("rewrite: ruleset %2d   input:", ruleset);
		printcav(pvp);
	}
	if (ruleset < 0 || ruleset >= MAXRWSETS)
	{
		syserr("554 rewrite: illegal ruleset number %d", ruleset);
		return EX_CONFIG;
	}
	if (reclevel++ > MAXRULERECURSION)
	{
		syserr("rewrite: infinite recursion, ruleset %d", ruleset);
		return EX_CONFIG;
	}
	if (pvp == NULL)
		return EX_USAGE;

	if (++nrw > 100)
	{
		char buf[MAXLINE];

		buf[0] = buf[MAXLINE-1] = 0;
		while (*pvp)
			(void) strncat(buf, *pvp++, sizeof buf);
		syserr("address causes rewrite loop: <%s>", buf);
		return;
	}

	/* Be sure to recognize first rule as new */
	prev_rwr = NULL;

	/*
	**  Run through the list of rewrite rules, applying any that match.
	*/

	ruleno = 1;
	loopcount = 0;
	for (rwr = RewriteRules[ruleset]; rwr != NULL; )
	{
		if (tTd(21, 12))
		{
			printf("-----trying rule:");
			printcav(rwr->r_lhs);
		}

		/*
		**  Set up the match list.  This is done once for each
		**  rule.  If a rule is used repeatedly, the list need not
		**  be set up the next time.
		*/

		if (rwr != prev_rwr)
		{
			prev_rwr = rwr;
			for (rvp = rwr->r_lhs, mlp = mlist;
				 *rvp && (mlp < &mlist[MAXMATCH]); rvp++)
			{
				mlp->flags = ((unsigned char) **rvp >= MAX_CONTROL) ?
								0 : control_opts[**rvp] ;
				if (mlp->flags)
				{
					mlp->source = rvp;
					mlp++;
				}
			}
			if (*rvp)
			{
				syserr("Too many variables on LHS in ruleset %d", ruleset);
				return;
			}
			mlp->source = rvp;

			/* Make sure end marker is initialized */
			mlp->flags = 0;
		}

		/* try to match on this rule */
		mlp = mlist;

		rvp = rwr->r_lhs;
		avp = pvp;
		nloops = 0;
		extend_match = FALSE;

		if (++loopcount > 100)
		{
			if (nloops++ > 400)
			{
				syserr("Looping on ruleset %d, rule %d",
					ruleset, rwr-RewriteRules[ruleset]);
				mlp = mlist - 1; /* force rule failure */
				break;
			}
			syserr("554 Infinite loop in ruleset %d, rule %d",
				ruleset, ruleno);
			if (tTd(21, 1))
			{
				printf("workspace: ");
				printav(pvp);
			}
			break;
		}

		while ((ap = *avp) != NULL || *rvp != NULL)
		{
			rp = *rvp;

			if (tTd(21, 35))
			{
				printf("ADVANCE rp=");
				xputs(rp);
				printf(", ap=");
				xputs(ap);
				printf("\n");
			}

			if (extend_match)
				extend_match = FALSE;
			else
			{
				mlp->first = avp;
				mlp->last = mlp->flags == 0 || (mlp->flags & OP_NONZLEN) ?
							avp + 1 : avp;
			}

			if (rp == NULL)
				/* end-of-pattern before end-of-address */
				goto backup;

			/* Premature end of address */
			if (ap == NULL && avp != mlp->last)
				goto backup;

			/*
			**  Simplest case - exact token comparison between
			**  pattern and address.  Such a match is not saved
			**  in mlp.
			*/

			if (rvp < mlp->source)
			{
				if (ap == NULL || strcasecmp(ap, rp))
					goto backup;
				rvp++;
				avp++;
				continue;
			}

#ifdef MACVALUE
			/*
			**  This is special case handled.  The match is exact,
			**  but might span zero or more address tokens.  The
			**  result is saved in mlp.
			*/

			if (*rp == MACVALUE)
			{
				int len;
				rp = macvalue(rp[1], CurEnv);

				if (rp)
					while (*rp)
					{
						if (*avp == NULL || strncasecmp(rp,*avp,len = strlen(*avp)))
							goto backup;
						rp += len;
						avp++;
					}
				mlp->last = avp;
				rvp++;
				mlp++;
				continue;
			}
#endif /* MACVALUE */

			/*
			**  All other matches are saved in mlp.  Initially
			**  assume they match at the shortest possible length
			**  for this pattern.  Variable patterns will be
			**  extended later as needed.
			*/

			/* Fixed length first */
			if (!(mlp->flags & OP_VARLEN))
			{
				switch (*rp)
				{
					break;
				}

				avp = mlp->last;
				rvp++;
				mlp++;
				break;

			  case MATCHZERO:
				/* match zero tokens */
				continue;
			}

			  case MACRODEXPAND:
				/*
				**  Match against run-time macro.
				**  This algorithm is broken for the
				**  general case (no recursive macros,
				**  improper tokenization) but should
				**  work for the usual cases.
				*/

				ap = macvalue(rp[1], e);
				mlp->first = avp;
				if (tTd(21, 2))
					printf("rewrite: LHS $&%c => \"%s\"\n",
						rp[1],
						ap == NULL ? "(NULL)" : ap);

				if (ap == NULL)
					break;
				while (*ap != '\0')
				{
					if (*avp == NULL ||
					    strncasecmp(ap, *avp, strlen(*avp)) != 0)
					{
						/* no match */
						avp = mlp->first;
						goto backup;
					}
					ap += strlen(*avp++);
				}

				/* match */
				break;

			/*
			**  We now have a variable length item.  It could
			**  be $+ or $* in which case no special checking
			**  is needed.  But a class match such as $=x must
			**  be verified.
			**
			**  As a speedup, if a variable length item is
			**  followed by a plain character token, we initially
			**  extend the match to the first such token we find.
			**  If the required character token cannot be found,
			**  we fail the match at this point.
			*/

			avp = mlp->last;

			/* If next token is char token */
			if (&rvp[1] < mlp[1].source)
			{
				while (*avp && strcasecmp(*avp, rvp[1]))
					avp++;

				/*
				**  If we can't find the proper ending token,
				**  leave avp point to NULL.  This indicates
				**  we have run out of address tokens.  It is
				**  pointless to advance the beginning of this
				**  match and retry.
				*/

				if (*avp == NULL)
					goto backup;
				mlp->last = avp;
			}
			else if (rvp[1] == NULL)
			/* next token is end of address */
			{
				while (*avp)
					avp++;
				mlp->last = avp;
			}

			if (mlp->flags & OP_CLASS)
			{
				register char *cp = tokbuf;

				avp = mlp->first;
				strcpy(cp, *avp);
				avp++;
				for (;;)
				{
					while (avp < mlp->last)
					{
						while (*cp)
							cp++;
						strcpy(cp, *avp);
						avp++;
					}
					switch (*rp)
					{
						register STAB *s;

					    case MATCHCLASS:
						s = stab(tokbuf, ST_CLASS, ST_FIND);
						if (s != NULL && bitnset(rp[1], s->s_class))
							goto have_match;
						break;
					}

					/*
					**  Class match initially failed.
					**  Extend the tentative match.
					**  Again, if followed by a character
					**  token, extend all the way to that
					**  token before checking.
					*/

					if (*avp)
					{
						(mlp->last)++;
						if (&rvp[1] < mlp[1].source)
						{
							while (*(mlp->last) && strcasecmp(*(mlp->last), rvp[1]))
								(mlp->last)++;
							if (*(mlp->last) == NULL)
								avp = mlp->last;
						}
					}
					if (*avp == NULL)
					{
						/*
						**  We could not find the
						**  ending token.  But we had
						**  found ending tokens before.
						**  A match is still plausible
						**  if the start of the
						**  tentative match is advanced.
						**  Hence we must not leave avp
						**  pointing to NULL.
						*/
						avp = mlp->first;
						goto backup;
					}
				}
			}

 have_match:
			rvp++;
			mlp++;
			continue;

backup:
			/* We failed to match.  mlp marks point of failure */

			/*
			**  There is a special case when we have exhausted
			**  the address, but have not exhausted the pattern.
			**  Under normal circumstances we could consider the
			**  failure permanent, since extending the number of
			**  address tokens matched by a '$+' or a '$*' will
			**  only worsen the situation.
			**
			**  There is an exception, however.  It is possible
			**  that we have matched a class token, say '$=x',
			**  with three or more tokens.  Extending a '$+' say,
			**  which precedes the '$=x' will move the beginning
			**  of the '$=x' match to the right, but it might match
			**  a smaller number of tokens then, possibly
			**  correcting the mismatch.
			**
			**  Thus in this case we initially back up to the
			**  $=x which matches three or more tokens.
			*/

			if (*avp == NULL)
			{
				rvp = mlp->pattern;
				while (--mlp > mlist)
				{
					if ((mlp->flags & OP_CLASS) &&
					    mlp->last > 2 + mlp->first)
						break;
				}
			}

			/*
			**  Now backup till we find a match with a pattern
			**  whose length is extendable, and extend that.
			*/

			mlp--;
			while (mlp >= mlist && !(mlp->flags & OP_VARLEN))
				mlp--;

			/* Total failure to match */
			if (mlp < mlist)
				break;

			avp = ++(mlp->last);
			rvp = mlp->source;

			/*
			**  We have found a backup point.  Normally we would
			**  increase the matched amount by one token, and
			**  continue from the next item in the pattern.  But
			**  there are two special cases.  If this is a
			**  class-type match (OP_CLASS), we must test the
			**  validity of the extended match.  If this pattern
			**  item is directly followed by a character token, it
			**  is worth going back and locating the next such
			**  character token before we continue on.
			*/
			if ((mlp->flags & OP_CLASS) || (&rvp[1] < mlp[1].source))
			{
				avp = mlp->first;
				extend_match = TRUE;
			}
			else
			{
				mlp++;
				rvp++;
			}
		}

		/*
		**  See if we successfully matched.
		*/

		if (mlp < mlist)
		{
			if (tTd(21, 10))
				printf("----- rule fails\n");
			rwr = rwr->r_next;
			ruleno++;
			loopcount = 0;
			nmatches = 0;
			continue;
		}

		if (nmatches++ > 200)
		{
			syserr("Loop in ruleset %d, rule %d (too many matches)",
				ruleset, rwr - RewriteRules[ruleset]);
			rwr = rwr->r_next;
			nmatches = 0;
			continue;
		}

		rvp = rwr->r_rhs;
		if (tTd(21, 12))
		{
			printf("-----rule matches:");
			printcav(rvp);
		}

		rp = *rvp;
		if ((*rp & 0377) == CANONUSER)
		{
			rvp++;
			rwr = rwr->r_next;
			ruleno++;
			loopcount = 0;
			nmatches = 0;
		}
		else if ((*rp & 0377) == CANONHOST)
		{
			rvp++;
			rwr = NULL;
		}
		else if ((*rp & 0377) == CANONNET)
			rwr = NULL;

		/* substitute */
		dolookup = FALSE;
		for (avp = npvp; *rvp != NULL; rvp++)
		{
			register struct match *m;
			register char **pp;

			rp = *rvp;

			/* check to see if we should do a lookup */
			if (*rp == MATCHLOOKUP)
				dolookup = TRUE;

			/* see if there is substitution here */
			if (*rp == MATCHREPL && rp[1] >= '1' && rp[1] <= '9')
			{
				/* substitute from LHS */
				m = &mlist[rp[1] - '1'];
				if (m < mlist || m >= mlp)
				{
				  toolong:
					syserr("rewrite: ruleset %d: replacement #%c out of bounds",
						ruleset, rp[1]);
					return EX_CONFIG;
				}
				if (tTd(21, 15))
				{
					printf("$%c:", rp[1]);
					pp = m->first;
					while (pp < m->last)
					{
						printf(" %x=\"", *pp);
						(void) fflush(stdout);
						printf("%s\"", *pp++);
					}
					printf("\n");
				}
				pp = m->first;
				while (pp < m->last)
				{
					if (avp >= &npvp[MAXATOM])
						goto toolong;
					*avp++ = *pp++;
				}
			}
			else
			{
				/* vanilla replacement */
				if (avp >= &npvp[MAXATOM])
					goto toolong;
#ifdef MACVALUE
				if (*rp == MACVALUE)
				{
					char *p = macvalue(rp[1], CurEnv);

					if (tTd(21, 2))
						printf("expanding runtime macro '%c' to \"%s\"\n",
						    rp[1], p ? p : "(null)");
					if (p)
						*avp++ = p;
				}
				else
#endif /* MACVALUE */
					*avp++ = rp;
			}
		}
		*avp++ = NULL;

		/*
		**  Check for any hostname/keyword lookups.
		*/

		for (rvp = npvp; *rvp != NULL; rvp++)
		{
			char **hbrvp, **ubrvp;
			char **xpvp;
			int trsize;
			char *replac;
			int endtoken;
			STAB *map;
			char *mapname;
			char **key_rvp;
			char **arg_rvp;
			char **default_rvp;
			char hbuf[MAXNAME + 1], ubuf[MAXNAME + 1];
			char *pvpb1[MAXATOM + 1];
			char *argvect[10];
			char pvpbuf[PSBUFSIZE];
			char *nullpvp[1];
			bool match, defaultpart;
			char begintype;
			char db = '\0';

			if ((**rvp & 0377) != HOSTBEGIN &&
			    (**rvp & 0377) != LOOKUPBEGIN)
				continue;

			/*
			**  Got a hostname/keyword lookup.
			**
			**	This could be optimized fairly easily.
			*/

			begintype = **rvp;
			hbrvp = rvp;
			ubrvp = NULL;
			if ((**rvp & 0377) == HOSTBEGIN)
			{
				endtoken = HOSTEND;
				mapname = "host";
			}
			else
			{
				endtoken = LOOKUPEND;
				mapname = *++rvp;
			}
			map = stab(mapname, ST_MAP, ST_FIND);
			if (map == NULL)
				syserr("554 rewrite: map %s not found", mapname);

			/* extract the match part */
			key_rvp = ++rvp;
			default_rvp = NULL;
			arg_rvp = argvect;
			xpvp = NULL;
			replac = pvpbuf;
			while (*rvp != NULL && (**rvp & 0377) != endtoken)
			{
				int nodetype = **rvp & 0377;

				if (nodetype != CANONHOST && nodetype != CANONUSER)
				{
					rvp++;
					continue;
				}

				*rvp++ = NULL;

				if (xpvp != NULL)
				{
					cataddr(xpvp, NULL, replac,
						&pvpbuf[sizeof pvpbuf] - replac,
						'\0');
					*++arg_rvp = replac;
					replac += strlen(replac) + 1;
					xpvp = NULL;
				}
				switch (nodetype)
				{
				  case CANONHOST:
					xpvp = rvp;
					break;

				  case CANONUSER:
					default_rvp = rvp;
					break;
				}
			}
			if (*rvp != NULL)
				*rvp++ = NULL;
			if (xpvp != NULL)
			{
				cataddr(xpvp, NULL, replac,
					&pvpbuf[sizeof pvpbuf] - replac, 
					'\0');
				*++arg_rvp = replac;
			}
			*++arg_rvp = NULL;

			/* save the remainder of the input string */
			trsize = (int) (avp - rvp + 1) * sizeof *rvp;
			bcopy((char *) rvp, (char *) pvpb1, trsize);

			/* append it to the token list */
				for (avp = hbrvp; *xpvp != NULL; xpvp++)
				{
				*avp++ = newstr(*xpvp);
				if (avp >= &npvp[MAXATOM])
					goto toolong;
				}
			}
			else
				avp = hbrvp;

			/* restore the old trailing information */
			rvp = avp - 1;
			for (xpvp = pvpb1; *xpvp != NULL; xpvp++)
			{
				if (defaultpart && **xpvp == HOSTEND)
				{
					defaultpart = FALSE;
					rvp = avp - 1;
				}
				else if (!defaultpart || !match)
					*avp++ = *xpvp;
				if (avp >= &npvp[MAXATOM])
					goto toolong;
			}
			*avp++ = NULL;

			/*break;*/
		}

		/*
		**  Check for subroutine calls.
		**  Then copy vector back into original space.
		*/

		callsubr(npvp);

		for (avp = npvp; *avp++ != NULL;);
			subr = atoi(*++rvp);
			rvp++;

		else
			subr = -1;

		/*
		**  Copy result back to original string.
		*/

		for (avp = pvp; *rvp != NULL; rvp++)
			*avp++ = *rvp;
		*avp = NULL;

		/*
		**  If this specified a subroutine, call it.
		*/

		if (subr >= 0)
		{
# ifdef DEBUG
			if (tTd(21, 3))
				printf("-----callsubr %s\n", subr);
# endif DEBUG
			rewrite(pvp, subr);
		}

		/*
		**  Done with rewriting this pass.
		*/

		if (tTd(21, 4))
		{
			printf("rewritten as:");
			printcav(pvp);
		}
	}

	if (OpMode == MD_TEST || tTd(21, 2))
	{
		printf("rewrite: ruleset %2d returns:", ruleset);
		printcav(pvp);
	}

	return rstat;
}
/*
**  CALLSUBR -- call subroutines in rewrite vector
**
**	Parameters:
**		pvp -- pointer to token vector.
**
**	Returns:
**		none.
**
**	Side Effects:
**		pvp is modified.
*/

static void
callsubr(pvp)
	char **pvp;
{
	char **rvp;
	int subr;

	for (; *pvp != NULL; pvp++)
		if (**pvp == CALLSUBR && pvp[1] != NULL && isdigit(pvp[1][0]))
		{
			subr = atoi(pvp[1]);

			if (tTd(21, 3))
				printf("-----callsubr %d\n", subr);

			/*
			**  Take care of possible inner calls.
			*/
			callsubr(pvp+2);

			/*
			**  Move vector up over calling opcode.
			*/
			for (rvp = pvp+2; *rvp != NULL; rvp++)
				rvp[-2] = rvp[0];
			rvp[-2] = NULL;

			/*
			**  Call inferior ruleset.
			*/
			_rewrite(pvp, subr);

			break;
		}
}
/*
**  BUILDADDR -- build address from token vector.
**
**	Parameters:
**		tv -- token vector.
**		a -- pointer to address descriptor to fill.
**			If NULL, one will be allocated.
**		flags -- info regarding whether this is a sender or
**			a recipient.
**		e -- the current envelope.
**
**	Returns:
**		NULL if there was an error.
**		'a' otherwise.
**
**	Side Effects:
**		fills in 'a'
*/

struct errcodes
{
	char	*ec_name;		/* name of error code */
	int	ec_code;		/* numeric code */
} ErrorCodes[] =
{
	"usage",	EX_USAGE,
	"nouser",	EX_NOUSER,
	"nohost",	EX_NOHOST,
	"unavailable",	EX_UNAVAILABLE,
	"software",	EX_SOFTWARE,
	"tempfail",	EX_TEMPFAIL,
	"protocol",	EX_PROTOCOL,
#ifdef EX_CONFIG
	"config",	EX_CONFIG,
#endif
	NULL,		EX_UNAVAILABLE,
};

static ADDRESS *
buildaddr(tv, a, flags, e)
	register char **tv;
	register ADDRESS *a;
	int flags;
	register ENVELOPE *e;
{
	struct mailer **mp;
	register struct mailer *m;
	char *bp;
	int spaceleft;
	static MAILER errormailer;
	static char *errorargv[] = { "ERROR", NULL };
	static char buf[MAXNAME];

	if (tTd(24, 5))
	{
		printf("buildaddr, flags=%o, tv=", flags);
		printav(tv);
	}

	if (a == NULL)
		a = (ADDRESS *) xalloc(sizeof *a);
	clear((char *) a, sizeof *a);

	/* figure out what net/mailer to use */
	if (*tv == NULL || **tv != CANONNET)
	{
		syserr("554 buildaddr: no net");
badaddr:
		a->q_flags |= QBADADDR;
		a->q_mailer = &errormailer;
		if (errormailer.m_name == NULL)
		{
			/* initialize the bogus mailer */
			errormailer.m_name = "*error*";
			errormailer.m_mailer = "ERROR";
			errormailer.m_argv = errorargv;
		}
		return a;
	}
	tv++;
	if (strcasecmp(*tv, "error") == 0)
	{
		if ((**++tv & 0377) == CANONHOST)
		{
			register struct errcodes *ep;

			if (isascii(**++tv) && isdigit(**tv))
			{
				setstat(atoi(*tv));
			}
			else
			{
				for (ep = ErrorCodes; ep->ec_name != NULL; ep++)
					if (strcasecmp(ep->ec_name, *tv) == 0)
						break;
				setstat(ep->ec_code);
			}
			tv++;
		}
		else
			setstat(EX_UNAVAILABLE);
		buf[0] = '\0';
		for (; (*tv != NULL) && (**tv != CANONUSER); tv++)
		{
			if (buf[0] != '\0')
				(void) strcat(buf, " ");
			(void) strcat(buf, *tv);
		}
		if ((**tv & 0377) != CANONUSER)
			syserr("554 buildaddr: error: no user");
		cataddr(++tv, NULL, buf, sizeof buf, ' ');
		stripquotes(buf);
#ifdef LOG
		if (LogLevel > 8)
			syslog (LOG_DEBUG, "%s: Trace: $#ERROR $: %s",
				CurEnv->e_id, buf);
#endif /* LOG */
		if (isascii(buf[0]) && isdigit(buf[0]) &&
		    isascii(buf[1]) && isdigit(buf[1]) &&
		    isascii(buf[2]) && isdigit(buf[2]) &&
		    buf[3] == ' ')
		{
			char fmt[10];

			strncpy(fmt, buf, 3);
			strcpy(&fmt[3], " %s");
			usrerr(fmt, buf + 4);
		}
		else
		{
			usrerr("%s", buf);
		}
		goto badaddr;
	}

	for (mp = Mailer; (m = *mp++) != NULL; )
	{
		if (strcasecmp(m->m_name, *tv) == 0)
			break;
	}
	if (m == NULL)
	{
		syserr("554 buildaddr: unknown mailer %s", *tv);
		goto badaddr;
	}
	a->q_mailer = m;

	/* figure out what host (if any) */
	if (**++tv != CANONHOST)
	{
		a->q_host = NULL;
	}
	else
	{
		else
			a->q_host = NULL;
	}
	else
	{
		while (*++tv != NULL && **tv != CANONUSER)
			(void) strcat(buf, *tv);
		a->q_host = newstr(buf);
	}

	/* figure out the user */
	if (*tv == NULL || (**tv & 0377) != CANONUSER)
	{
		syserr("554 buildaddr: no user");
		goto badaddr;
	}
	tv++;

	/* do special mapping for local mailer */
	if (m == LocalMailer && *tv != NULL)
	{
		register char *p = *tv;

		if (*p == '"')
			p++;
		if (*p == '|')
			a->q_mailer = m = ProgMailer;
		else if (*p == '/')
			a->q_mailer = m = FileMailer;
		else if (*p == ':')
		{
			/* may be :include: */
			cataddr(tv, NULL, buf, sizeof buf, '\0');
			stripquotes(buf);
			if (strncasecmp(buf, ":include:", 9) == 0)
			{
				/* if :include:, don't need further rewriting */
				a->q_mailer = m = InclMailer;
				a->q_user = &buf[9];
				return (a);
			}
		}
	}

	if (m == LocalMailer && *tv != NULL && strcmp(*tv, "@") == 0)
	{
		tv++;
		a->q_flags |= QNOTREMOTE;
	}

	if (m->m_r_rwset > 0)
		rewrite(tv, m->m_r_rwset);
	(void) rewrite(tv, 4, 0, e);

	/* save the result for the command line/RCPT argument */
	cataddr(tv, NULL, buf, sizeof buf, '\0');
	a->q_user = buf;

	/*
	**  Do mapping to lower case as requested by mailer
	*/

	if (a->q_host != NULL && !bitnset(M_HST_UPPER, m->m_flags))
		makelower(a->q_host);
	if (!bitnset(M_USR_UPPER, m->m_flags))
		makelower(a->q_user);

	return (a);
}
/*
**  CATADDR -- concatenate pieces of addresses (putting in <LWSP> subs)
**
**	Parameters:
**		pvp -- parameter vector to rebuild.
**		evp -- last parameter to include.  Can be NULL to
**			use entire pvp.
**		buf -- buffer to build the string into.
**		sz -- size of buf.
**		spacesub -- the space separator character; if null,
**			use SpaceSub.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Destroys buf.
*/

void
cataddr(pvp, evp, buf, sz, spacesub)
	char **pvp;
	char **evp;
	char *buf;
	register int sz;
	char spacesub;
{
	bool oatomtok = FALSE;
	bool natomtok;
	register int i;
	register char *p;

	if (spacesub == '\0')
		spacesub = SpaceSub;

	if (pvp == NULL)
	{
		(void) strcpy(buf, "");
		return;
	}
	p = buf;
	sz -= 2;
	while (*pvp != NULL && (i = strlen(*pvp)) < sz)
	{
		natomtok = (toktype(**pvp) == ATM);
		if (oatomtok && natomtok)
			*p++ = spacesub;
		(void) strcpy(p, *pvp);
		oatomtok = natomtok;
		p += i;
		sz -= i + 1;
		if (pvp++ == evp)
			break;
	}
	*p = '\0';
}
/*
**  SAMEADDR -- Determine if two addresses are the same
**
**	This is not just a straight comparison -- if the mailer doesn't
**	care about the host we just ignore it, etc.
**
**	Parameters:
**		a, b -- pointers to the internal forms to compare.
**
**	Returns:
**		TRUE -- they represent the same mailbox.
**		FALSE -- they don't.
**
**	Side Effects:
**		none.
*/

bool
sameaddr(a, b)
	register ADDRESS *a;
	register ADDRESS *b;
{
	register ADDRESS *ca, *cb;

	/* if they don't have the same mailer, forget it */
	if (a->q_mailer != b->q_mailer)
		return (FALSE);

	/* if the user isn't the same, we can drop out */
	if (strcasecmp(a->q_user, b->q_user))
		return (FALSE);

	/* if we have good uids for both but they differ, these are different */
	ca = getctladdr(a);
	cb = getctladdr(b);
	if (ca != NULL && cb != NULL &&
	    bitset(QGOODUID, ca->q_flags & cb->q_flags) &&
	    ca->q_uid != cb->q_uid)
		return (FALSE);

	/* otherwise compare hosts (but be careful for NULL ptrs) */
	if (a->q_host == b->q_host)
	{
		/* probably both null pointers */
		return (TRUE);
	}
	if (a->q_host == NULL || b->q_host == NULL)
	{
		/* only one is a null pointer */
		return (FALSE);
	}
	if (strcasecmp(a->q_host, b->q_host))
		return (FALSE);

	return (TRUE);
}
/*
**  PRINTADDR -- print address (for debugging)
**
**	Parameters:
**		a -- the address to print
**		follow -- follow the q_next chain.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

void
printaddr(a, follow)
	register ADDRESS *a;
	bool follow;
{
	bool first = TRUE;
	register MAILER *m;
	MAILER pseudomailer;

	static int indent;
	register int i;

	while (a != NULL)
	{
		first = FALSE;
		for (i = indent; i > 0; i--)
			printf("\t");
		printf("%x=", a);
		(void) fflush(stdout);

		/* find the mailer -- carefully */
		m = a->q_mailer;
		if (m == NULL)
		{
			m = &pseudomailer;
			m->m_mno = -1;
			m->m_name = "NULL";
		}

		for (i = indent; i > 0; i--)
			printf("\t");
		printf("\tnext=%x, flags=%o, rmailer %d, alias=%x, sibling=%x, child=%x\n",
		       a->q_next, a->q_flags, a->q_rmailer, a->q_alias,
		       a->q_sibling, a->q_child);
		
		/* follow the chain if appropriate */
		if (!follow)
			return;
		
		indent++;
		printaddr(a->q_child, TRUE);
		indent--;
		a = a->q_sibling;
	}
	if (first)
		printf("[NULL]\n");
}

/*
**  REMOTENAME -- return the name relative to the current mailer
**
**	Parameters:
**		name -- the name to translate.
**		m -- the mailer that we want to do rewriting relative
**			to.
**		flags -- fine tune operations.
**		pstat -- pointer to status word.
**		e -- the current envelope.
**
**	Returns:
**		the text string representing this address relative to
**			the receiving mailer.
**
**	Side Effects:
**		none.
**
**	Warnings:
**		The text string returned is tucked away locally;
**			copy it if you intend to save it.
*/

char *
remotename(name, m, flags, pstat, e)
	char *name;
	MAILER *m;
	int flags;
	int *pstat;
	register ENVELOPE *e;
{
	register char **pvp;
	char *fancy;
	char *oldg = macvalue('g', e);
	int rwset;
	static char buf[MAXNAME];
	char lbuf[MAXNAME];
	char pvpbuf[PSBUFSIZE];

	if (tTd(12, 1))
		printf("remotename(%s)\n", name);

	/* don't do anything if we are tagging it as special */
	if ((senderaddress ? m->m_s_rwset : m->m_r_rwset) < 0)
		return (name);

	/*
	**  Do a heuristic crack of this name to extract any comment info.
	**	This will leave the name as a comment and a $g macro.
	*/

	if (bitset(RF_CANONICAL, flags) || bitnset(M_NOCOMMENT, m->m_flags))
		fancy = "\201g";
	else
		fancy = crackaddr(name);

	/*
	**  Turn the name into canonical form.
	**	Normally this will be RFC 822 style, i.e., "user@domain".
	**	If this only resolves to "user", and the "C" flag is
	**	specified in the sending mailer, then the sender's
	**	domain will be appended.
	*/

	pvp = prescan(name, '\0', pvpbuf, sizeof pvpbuf, NULL);
	if (pvp == NULL)
		return (name);
	if (rewrite(pvp, 3, 0, e) == EX_TEMPFAIL)
		*pstat = EX_TEMPFAIL;
	if (bitset(RF_ADDDOMAIN, flags) && e->e_fromdomain != NULL)
	{
		/* append from domain to this address */
		register char **pxp = pvp;

		/* see if there is an "@domain" in the current name */
		while (*pxp != NULL && strcmp(*pxp, "@") != 0)
			pxp++;
		if (*pxp == NULL)
		{
			/* no.... append the "@domain" from the sender */
			register char **qxq = e->e_fromdomain;

			while ((*pxp++ = *qxq++) != NULL)
				continue;
			if (rewrite(pvp, 3, 0, e) == EX_TEMPFAIL)
				*pstat = EX_TEMPFAIL;
		}
	}

	/*
	**  Do more specific rewriting.
	**	Rewrite using ruleset 1 or 2 for envelope addresses and
	**	5 or 6 for header addresses depending on whether this
	**	is a sender address or not.
	**	Then run it through any receiving-mailer-specific rulesets.
	*/

	else
	{
	if (rwset > 0)
	{
		if (rewrite(pvp, rwset, 0, e) == EX_TEMPFAIL)
			*pstat = EX_TEMPFAIL;
	}

	/*
	**  Do any final sanitation the address may require.
	**	This will normally be used to turn internal forms
	**	(e.g., user@host.LOCAL) into external form.  This
	**	may be used as a default to the above rules.
	*/

	if (rewrite(pvp, 4, 0, e) == EX_TEMPFAIL)
		*pstat = EX_TEMPFAIL;

	/*
	**  Now restore the comment information we had at the beginning.
	*/

	cataddr(pvp, NULL, lbuf, sizeof lbuf, '\0');
	define('g', lbuf, e);
	expand(fancy, buf, &buf[sizeof buf - 1], e);
	define('g', oldg, e);

	if (tTd(12, 1))
		printf("remotename => `%s'\n", buf);
	return (buf);
}
/*
**  UURELATIVIZE -- Make an address !-relative to recipient/sender nodes
**
**	Parameters:
**		from -- the sending node (usually "$k" or "$w")
**		to -- the receiving node (usually "$h")
**		pvp -- address vector
**
**	Returns:
**		none.
**
**	Side Effects:
**		The pvp is rewritten to be relative the "to" node
**		wrt the "from" node.  In other words, if the pvp
**		is headed by "to!" that part is stripped; otherwise
**		"from!" is prepended.  Exception: "to!user" addresses
**		with no '!'s in the user part are sent as is.
**
**	Bugs:
**		The pvp may overflow, but we don't catch it.
*/

static void
uurelativize(from, to, pvp)
	const char *from, *to;
	char **pvp;
{
	register char **pxp = pvp;
	char expfrom[MAXNAME], expto[MAXNAME];

	expand(from, expfrom, &expfrom[sizeof expfrom - 1], CurEnv);
	expand(to, expto, &expto[sizeof expto - 1], CurEnv);

	/*
	 * supposing that we've got something, should
	 * we add "from!" or remove "to!"?
	 */
	if (pvp[0] != NULL)
		if (pvp[1] == NULL || strcmp(pvp[1], "!") != 0 ||
		    /*strcasecmp?*/ strcmp(pvp[0], expto) != 0)
		{
			/* either local name, no UUCP address, */
			/* or not to "to!" ==> prepend address with "from!" */

			/* already there? */
			if (pvp[1] == NULL || strcmp(pvp[1], "!") != 0 ||
			    /*strcasecmp?*/ strcmp(pvp[0], expfrom) != 0)
			{

				/* no, put it there */
				while (*pxp != NULL)
					pxp++;
				do
					pxp[2] = *pxp;
				while (pxp-- != pvp);
				pvp[0] = newstr(expfrom);
				pvp[1] = "!";
			}
		}
		else
		{
			/* address is to "to!" -- remove if not "to!user" */
			for (pxp = &pvp[2];
			     *pxp != NULL && strcmp(*pxp, "!") != 0; pxp++)
				;
			if (*pxp != NULL)
				for (pxp = pvp; *pxp != NULL; pxp++)
					*pxp = pxp[2];
		}
}
/*
**  MAPLOCALUSER -- run local username through ruleset 5 for final redirection
**
**	Parameters:
**		a -- the address to map (but just the user name part).
**		sendq -- the sendq in which to install any replacement
**			addresses.
**
**	Returns:
**		none.
*/

maplocaluser(a, sendq, e)
	register ADDRESS *a;
	ADDRESS **sendq;
	ENVELOPE *e;
{
	register char **pvp;
	register ADDRESS *a1 = NULL;
	auto char *delimptr;
	char pvpbuf[PSBUFSIZE];

	if (tTd(29, 1))
	{
		printf("maplocaluser: ");
		printaddr(a, FALSE);
	}
	pvp = prescan(a->q_user, '\0', pvpbuf, sizeof pvpbuf, &delimptr);
	if (pvp == NULL)
		return;

	(void) rewrite(pvp, 5, 0, e);
	if (pvp[0] == NULL || (pvp[0][0] & 0377) != CANONNET)
		return;

	/* if non-null, mailer destination specified -- has it changed? */
	a1 = buildaddr(pvp, NULL, 0, e);
	if (a1 == NULL || sameaddr(a, a1))
		return;

	/* mark old address as dead; insert new address */
	a->q_flags |= QDONTSEND;
	if (tTd(29, 5))
	{
		printf("maplocaluser: QDONTSEND ");
		printaddr(a, FALSE);
	}
	a1->q_alias = a;
	allocaddr(a1, RF_COPYALL, NULL);
	(void) recipient(a1, sendq, e);
}
/*
**  DEQUOTE_INIT -- initialize dequote map
**
**	This is a no-op.
**
**	Parameters:
**		map -- the internal map structure.
**		args -- arguments.
**
**	Returns:
**		TRUE.
*/

bool
dequote_init(map, args)
	MAP *map;
	char *args;
{
	register char *p = args;

	for (;;)
	{
		while (isascii(*p) && isspace(*p))
			p++;
		if (*p != '-')
			break;
		switch (*++p)
		{
		  case 'a':
			map->map_app = ++p;
			break;
		}
		while (*p != '\0' && !(isascii(*p) && isspace(*p)))
			p++;
		if (*p != '\0')
			*p = '\0';
	}
	if (map->map_app != NULL)
		map->map_app = newstr(map->map_app);

	return TRUE;
}
/*
**  DEQUOTE_MAP -- unquote an address
**
**	Parameters:
**		map -- the internal map structure (ignored).
**		name -- the name to dequote.
**		av -- arguments (ignored).
**		statp -- pointer to status out-parameter.
**
**	Returns:
**		NULL -- if there were no quotes, or if the resulting
**			unquoted buffer would not be acceptable to prescan.
**		else -- The dequoted buffer.
*/

char *
dequote_map(map, name, av, statp)
	MAP *map;
	char *name;
	char **av;
	int *statp;
{
	register char *p;
	register char *q;
	register char c;
	int anglecnt;
	int cmntcnt;
	int quotecnt;
	int spacecnt;
	bool quotemode;
	bool bslashmode;

	anglecnt = 0;
	cmntcnt = 0;
	quotecnt = 0;
	spacecnt = 0;
	quotemode = FALSE;
	bslashmode = FALSE;

	for (p = q = name; (c = *p++) != '\0'; )
	{
		if (bslashmode)
		{
			bslashmode = FALSE;
			*q++ = c;
			continue;
		}

		switch (c)
		{
		  case '\\':
			bslashmode = TRUE;
			break;

		  case '(':
			cmntcnt++;
			break;

		  case ')':
			if (cmntcnt-- <= 0)
				return NULL;
			break;

		  case ' ':
			spacecnt++;
			break;
		}

		if (cmntcnt > 0)
		{
			*q++ = c;
			continue;
		}

		switch (c)
		{
		  case '"':
			quotemode = !quotemode;
			quotecnt++;
			continue;

		  case '<':
			anglecnt++;
			break;

		  case '>':
			if (anglecnt-- <= 0)
				return NULL;
			break;
		}
		*q++ = c;
	}

	if (anglecnt != 0 || cmntcnt != 0 || bslashmode ||
	    quotemode || quotecnt <= 0 || spacecnt != 0)
		return NULL;
	*q++ = '\0';
	return name;
}
