# include "sendmail.h"

SCCSID(@(#)parseaddr.c	3.54		%G%);

/*
**  PARSE -- Parse an address
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
**		copyf -- determines what shall be copied:
**			-1 -- don't copy anything.  The printname
**				(q_paddr) is just addr, and the
**				user & host are allocated internally
**				to parse.
**			0 -- copy out the parsed user & host, but
**				don't copy the printname.
**			+1 -- copy everything.
**
**	Returns:
**		A pointer to the address descriptor header (`a' if
**			`a' is non-NULL).
**		NULL on error.
**
**	Side Effects:
**		none
*/

# define DELIMCHARS	"$()<>,;\\\"\r\n"	/* word delimiters */

ADDRESS *
parse(addr, a, copyf)
	char *addr;
	register ADDRESS *a;
	int copyf;
{
	register char **pvp;
	register struct mailer *m;
	extern char **prescan();
	extern ADDRESS *buildaddr();
	static char nbuf[MAXNAME];

	/*
	**  Initialize and prescan address.
	*/

	CurEnv->e_to = addr;
# ifdef DEBUG
	if (tTd(20, 1))
		printf("\n--parse(%s)\n", addr);
# endif DEBUG

	pvp = prescan(addr, ',');
	if (pvp == NULL)
		return (NULL);

	/*
	**  Apply rewriting rules.
	**	Ruleset 0 does basic parsing.  It must resolve.
	*/

	rewrite(pvp, 0);

	/*
	**  See if we resolved to a real mailer.
	*/

	if (pvp[0][0] != CANONNET)
	{
		setstat(EX_USAGE);
		usrerr("cannot resolve name");
		return (NULL);
	}

	/*
	**  Build canonical address from pvp.
	*/

	a = buildaddr(pvp, a);
	if (a == NULL)
		return (NULL);
	m = a->q_mailer;

	/*
	**  Make local copies of the host & user and then
	**  transport them out.
	*/

	if (copyf > 0)
	{
		extern char *DelimChar;
		char savec = *DelimChar;

		*DelimChar = '\0';
		a->q_paddr = newstr(addr);
		*DelimChar = savec;
	}
	else
		a->q_paddr = addr;
	if (copyf >= 0)
	{
		if (a->q_host != NULL)
			a->q_host = newstr(a->q_host);
		else
			a->q_host = "";
		if (a->q_user != a->q_paddr)
			a->q_user = newstr(a->q_user);
	}

	/*
	**  Do UPPER->lower case mapping unless inhibited.
	*/

	if (!bitset(M_HST_UPPER, m->m_flags))
		makelower(a->q_host);
	if (!bitset(M_USR_UPPER, m->m_flags))
		makelower(a->q_user);

	/*
	**  Compute return value.
	*/

# ifdef DEBUG
	if (tTd(20, 1))
	{
		printf("parse-->");
		printaddr(a, FALSE);
	}
# endif DEBUG

	return (a);
}
/*
**  PRESCAN -- Prescan name and make it canonical
**
**	Scans a name and turns it into canonical form.  This involves
**	deleting blanks, comments (in parentheses), and turning the
**	word "at" into an at-sign ("@").  The name is copied as this
**	is done; it is legal to copy a name onto itself, since this
**	process can only make things smaller.
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
**
**	Returns:
**		A pointer to a vector of tokens.
**		NULL on error.
**
**	Side Effects:
**		none.
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
	/*OPR*/		OPR|B,	ATM|B,	QST|MB,	SPC|MB,	ONE|B,
	/*ATM*/		OPR|B,	ATM,	QST|MB,	SPC|MB,	ONE|B,
	/*QST*/		QST,	QST,	OPR|MB,	QST,	QST,
	/*SPC*/		OPR,	ATM,	QST,	SPC|M,	ONE,
	/*ONE*/		OPR,	OPR,	OPR,	OPR,	OPR,
};

# define NOCHAR		-1	/* signal nothing in lookahead token */

char	*DelimChar;		/* set to point to the delimiter */

char **
prescan(addr, delim)
	char *addr;
	char delim;
{
	register char *p;
	register char *q;
	register char c;
	char **avp;
	bool bslashmode;
	int cmntcnt;
	char *tok;
	int state;
	int newstate;
	static char buf[MAXNAME+MAXATOM];
	static char *av[MAXATOM+1];

	q = buf;
	bslashmode = FALSE;
	cmntcnt = 0;
	avp = av;
	state = OPR;
	c = NOCHAR;
	p = addr;
# ifdef DEBUG
	if (tTd(22, 45))
	{
		printf("prescan: ");
		xputs(p);
		putchar('\n');
	}
# endif DEBUG

	do
	{
		/* read a token */
		tok = q;
		for (;;)
		{
			/* store away any old lookahead character */
			if (c != NOCHAR)
			{
				/* squirrel it away */
				if (q >= &buf[sizeof buf - 5])
				{
					usrerr("Address too long");
					DelimChar = p;
					return (NULL);
				}
				*q++ = c;
			}

			/* read a new input character */
			c = *p++;
			if (c == '\0')
				break;
# ifdef DEBUG
			if (tTd(22, 101))
				printf("c=%c, s=%d; ", c, state);
# endif DEBUG

			/* chew up special characters */
			c &= ~0200;
			*q = '\0';
			if (bslashmode)
			{
				c |= 0200;
				bslashmode = FALSE;
			}
			else if (c == '\\')
			{
				bslashmode = TRUE;
				c = NOCHAR;
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
					usrerr("Unbalanced ')'");
					DelimChar = p;
					return (NULL);
				}
				else
					cmntcnt--;
			}
			else if (cmntcnt > 0)
				c = NOCHAR;

			if (c == NOCHAR)
				continue;

			/* see if this is end of input */
			if (c == delim)
				break;

			newstate = StateTab[state][toktype(c)];
# ifdef DEBUG
			if (tTd(22, 101))
				printf("ns=%02o\n", newstate);
# endif DEBUG
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
# ifdef DEBUG
			if (tTd(22, 36))
			{
				printf("tok=");
				xputs(tok);
				putchar('\n');
			}
# endif DEBUG
			if (avp >= &av[MAXATOM])
			{
				syserr("prescan: too many tokens");
				DelimChar = p;
				return (NULL);
			}
			*avp++ = tok;
		}
	} while (c != '\0' && c != delim);
	*avp = NULL;
	DelimChar = --p;
	if (cmntcnt > 0)
		usrerr("Unbalanced '('");
	else if (state == QST)
		usrerr("Unbalanced '\"'");
	else if (av[0] != NULL)
		return (av);
	return (NULL);
}
/*
**  TOKTYPE -- return token type
**
**	Parameters:
**		c -- the character in question.
**
**	Returns:
**		Its type.
**
**	Side Effects:
**		none.
*/

toktype(c)
	register char c;
{
	static char buf[50];
	static bool firstime = TRUE;

	if (firstime)
	{
		firstime = FALSE;
		expand("$o", buf, &buf[sizeof buf - 1], CurEnv);
		(void) strcat(buf, DELIMCHARS);
	}
	if (c == MATCHCLASS || c == MATCHREPL)
		return (ONE);
	if (c == '"')
		return (QST);
	if (!isascii(c))
		return (ATM);
	if (isspace(c) || c == ')')
		return (SPC);
	if (iscntrl(c) || index(buf, c) != NULL)
		return (OPR);
	return (ATM);
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
**	MATCHONE, MATCHCLASS) then the address in avp matched is
**	saved away in the match vector (pointed to by 'mvp').
**
**	When a match between avp & pvp does not match, we try to
**	back out.  If we back up over a MATCHONE or a MATCHCLASS
**	we must also back out the match in mvp.  If we reach a
**	MATCHANY or MATCHZANY we just extend the match and start
**	over again.
**
**	When we finally match, we rewrite the address vector
**	and try over again.
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

struct match
{
	char	**first;	/* first token matched */
	char	**last;		/* last token matched */
};

# define MAXMATCH	9	/* max params per rewrite */


rewrite(pvp, ruleset)
	char **pvp;
	int ruleset;
{
	register char *ap;		/* address pointer */
	register char *rp;		/* rewrite pointer */
	register char **avp;		/* address vector pointer */
	register char **rvp;		/* rewrite vector pointer */
	register struct match *mlp;	/* cur ptr into mlist */
	register struct rewrite *rwr;	/* pointer to current rewrite rule */
	struct match mlist[MAXMATCH];	/* stores match on LHS */
	char *npvp[MAXATOM+1];		/* temporary space for rebuild */
	extern bool sameword();

# ifdef DEBUG
	if (tTd(21, 2))
	{
		printf("rewrite: ruleset %d, original pvp:", ruleset);
		printav(pvp);
	}
# endif DEBUG

	/*
	**  Run through the list of rewrite rules, applying
	**	any that match.
	*/

	for (rwr = RewriteRules[ruleset]; rwr != NULL; )
	{
# ifdef DEBUG
		if (tTd(21, 12))
		{
			printf("-----trying rule:");
			printav(rwr->r_lhs);
		}
# endif DEBUG

		/* try to match on this rule */
		mlp = mlist;
		rvp = rwr->r_lhs;
		avp = pvp;
		while ((ap = *avp) != NULL || *rvp != NULL)
		{
			rp = *rvp;
# ifdef DEBUG
			if (tTd(21, 35))
			{
				printf("ap=");
				xputs(ap);
				printf(", rp=");
				xputs(rp);
				printf("\n");
			}
# endif DEBUG
			if (rp == NULL)
			{
				/* end-of-pattern before end-of-address */
				goto backup;
			}
			if (ap == NULL && *rp != MATCHZANY)
			{
				/* end-of-input */
				break;
			}

			switch (*rp)
			{
				register STAB *s;
				register int class;

			  case MATCHCLASS:
				/* match any token in a class */
				class = rp[1];
				if (!isalpha(class))
					goto backup;
				if (isupper(class))
					class -= 'A';
				else
					class -= 'a';
				s = stab(ap, ST_CLASS, ST_FIND);
				if (s == NULL || (s->s_class & (1L << class)) == 0)
					goto backup;

				/* explicit fall-through */

			  case MATCHONE:
			  case MATCHANY:
				/* match exactly one token */
				mlp->first = avp;
				mlp->last = avp++;
				mlp++;
				break;

			  case MATCHZANY:
				/* match zero or more tokens */
				mlp->first = avp;
				mlp->last = avp - 1;
				mlp++;
				break;

			  default:
				/* must have exact match */
				if (!sameword(rp, ap))
					goto backup;
				avp++;
				break;
			}

			/* successful match on this token */
			rvp++;
			continue;

		  backup:
			/* match failed -- back up */
			while (--rvp >= rwr->r_lhs)
			{
				rp = *rvp;
				if (*rp == MATCHANY || *rp == MATCHZANY)
				{
					/* extend binding and continue */
					avp = ++mlp[-1].last;
					avp++;
					rvp++;
					break;
				}
				avp--;
				if (*rp == MATCHONE || *rp == MATCHCLASS)
				{
					/* back out binding */
					mlp--;
				}
			}

			if (rvp < rwr->r_lhs)
			{
				/* total failure to match */
				break;
			}
		}

		/*
		**  See if we successfully matched
		*/

		if (rvp >= rwr->r_lhs && *rvp == NULL)
		{
			rvp = rwr->r_rhs;
# ifdef DEBUG
			if (tTd(21, 12))
			{
				printf("-----rule matches:");
				printav(rvp);
			}
# endif DEBUG

			/* see if this is a "subroutine" call */
			rp = *rvp;
			if (*rp == CALLSUBR)
			{
				rp = *++rvp;
# ifdef DEBUG
				if (tTd(21, 3))
					printf("-----callsubr %s\n", rp);
# endif DEBUG
				rewrite(pvp, atoi(rp));
				rwr = rwr->r_next;
				continue;
			}
			else if (*rp == CANONUSER)
			{
				rvp++;
				rwr = rwr->r_next;
			}
			else if (*rp == CANONHOST)
			{
				rvp++;
				rwr = NULL;
			}
			else if (*rp == CANONNET)
				rwr = NULL;

			/* substitute */
			for (avp = npvp; *rvp != NULL; rvp++)
			{
				rp = *rvp;
				if (*rp == MATCHREPL)
				{
					register struct match *m;
					register char **pp;

					m = &mlist[rp[1] - '1'];
# ifdef DEBUG
					if (tTd(21, 15))
					{
						printf("$%c:", rp[1]);
						pp = m->first;
						while (pp <= m->last)
						{
							printf(" %x=\"", *pp);
							(void) fflush(stdout);
							printf("%s\"", *pp++);
						}
						printf("\n");
					}
# endif DEBUG
					pp = m->first;
					while (pp <= m->last)
					{
						if (avp >= &npvp[MAXATOM])
						{
							syserr("rewrite: expansion too long");
							return;
						}
						*avp++ = *pp++;
					}
				}
				else
				{
					if (avp >= &npvp[MAXATOM])
					{
						syserr("rewrite: expansion too long");
						return;
					}
					*avp++ = rp;
				}
			}
			*avp++ = NULL;
			bmove((char *) npvp, (char *) pvp, (avp - npvp) * sizeof *avp);
# ifdef DEBUG
			if (tTd(21, 4))
			{
				printf("rewritten as:");
				printav(pvp);
			}
# endif DEBUG
		}
		else
		{
# ifdef DEBUG
			if (tTd(21, 10))
				printf("----- rule fails\n");
# endif DEBUG
			rwr = rwr->r_next;
		}
	}

# ifdef DEBUG
	if (tTd(21, 2))
	{
		printf("rewrite: ruleset %d returns:", ruleset);
		printav(pvp);
	}
# endif DEBUG
}
/*
**  BUILDADDR -- build address from token vector.
**
**	Parameters:
**		tv -- token vector.
**		a -- pointer to address descriptor to fill.
**			If NULL, one will be allocated.
**
**	Returns:
**		NULL if there was an error.
**		'a' otherwise.
**
**	Side Effects:
**		fills in 'a'
*/

ADDRESS *
buildaddr(tv, a)
	register char **tv;
	register ADDRESS *a;
{
	static char buf[MAXNAME];
	struct mailer **mp;
	register struct mailer *m;
	extern bool sameword();

	if (a == NULL)
		a = (ADDRESS *) xalloc(sizeof *a);
	clear((char *) a, sizeof *a);

	/* figure out what net/mailer to use */
	if (**tv != CANONNET)
	{
		syserr("buildaddr: no net");
		return (NULL);
	}
	tv++;
	if (sameword(*tv, "error"))
	{
		if (**++tv != CANONUSER)
			syserr("buildaddr: error: no user");
		buf[0] = '\0';
		while (*++tv != NULL)
		{
			if (buf[0] != '\0')
				(void) strcat(buf, " ");
			(void) strcat(buf, *tv);
		}
		usrerr(buf);
		return (NULL);
	}
	for (mp = Mailer; (m = *mp++) != NULL; )
	{
		if (sameword(m->m_name, *tv))
			break;
	}
	if (m == NULL)
	{
		syserr("buildaddr: unknown net %s", *tv);
		return (NULL);
	}
	a->q_mailer = m;

	/* figure out what host (if any) */
	tv++;
	if (!bitset(M_LOCAL, m->m_flags))
	{
		if (**tv++ != CANONHOST)
		{
			syserr("buildaddr: no host");
			return (NULL);
		}
		buf[0] = '\0';
		while (*tv != NULL && **tv != CANONUSER)
			(void) strcat(buf, *tv++);
		a->q_host = newstr(buf);
	}
	else
		a->q_host = NULL;

	/* figure out the user */
	if (**tv != CANONUSER)
	{
		syserr("buildaddr: no user");
		return (NULL);
	}
	cataddr(++tv, buf, sizeof buf);
	a->q_user = buf;

	return (a);
}
/*
**  CATADDR -- concatenate pieces of addresses (putting in <LWSP> subs)
**
**	Parameters:
**		pvp -- parameter vector to rebuild.
**		buf -- buffer to build the string into.
**		sz -- size of buf.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Destroys buf.
*/

cataddr(pvp, buf, sz)
	char **pvp;
	char *buf;
	register int sz;
{
	bool oatomtok = FALSE;
	bool natomtok = FALSE;
	register int i;
	register char *p;

	p = buf;
	sz--;
	while (*pvp != NULL && (i = strlen(*pvp)) < sz)
	{
		natomtok = (toktype(**pvp) == ATM);
		if (oatomtok && natomtok)
			*p++ = SPACESUB;
		(void) strcpy(p, *pvp);
		oatomtok = natomtok;
		p += i;
		sz -= i;
		pvp++;
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
**		wildflg -- if TRUE, 'a' may have no user specified,
**			in which case it is to match anything.
**
**	Returns:
**		TRUE -- they represent the same mailbox.
**		FALSE -- they don't.
**
**	Side Effects:
**		none.
*/

bool
sameaddr(a, b, wildflg)
	register ADDRESS *a;
	register ADDRESS *b;
	bool wildflg;
{
	/* if they don't have the same mailer, forget it */
	if (a->q_mailer != b->q_mailer)
		return (FALSE);

	/* if the user isn't the same, we can drop out */
	if ((!wildflg || a->q_user[0] != '\0') && strcmp(a->q_user, b->q_user) != 0)
		return (FALSE);

	/* if the mailer ignores hosts, we have succeeded! */
	if (bitset(M_LOCAL, a->q_mailer->m_flags))
		return (TRUE);

	/* otherwise compare hosts (but be careful for NULL ptrs) */
	if (a->q_host == NULL || b->q_host == NULL)
		return (FALSE);
	if (strcmp(a->q_host, b->q_host) != 0)
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

# ifdef DEBUG

printaddr(a, follow)
	register ADDRESS *a;
	bool follow;
{
	bool first = TRUE;

	static int indent;
	register int i;

	while (a != NULL)
	{
		first = FALSE;
		for (i = indent; i > 0; i--)
			printf("\t");
		printf("%x=", a);
		(void) fflush(stdout);
		printf("%s: mailer %d (%s), host `%s', user `%s'\n", a->q_paddr,
		       a->q_mailer->m_mno, a->q_mailer->m_name, a->q_host, a->q_user);
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

# endif DEBUG
/*
**  REMOTENAME -- return the name relative to the current mailer
**
**	Parameters:
**		name -- the name to translate.
**		m -- the mailer that we want to do rewriting relative
**			to.
**		senderaddress -- if set, uses the sender rewriting rules
**			rather than the recipient rewriting rules.
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
remotename(name, m, senderaddress)
	char *name;
	struct mailer *m;
	bool senderaddress;
{
	register char **pvp;
	char *fancy;
	extern char *macvalue();
	char *oldg = macvalue('g');
	static char buf[MAXNAME];
	char lbuf[MAXNAME];
	extern char **prescan();
	extern char *crackaddr();

# ifdef DEBUG
	if (tTd(12, 1))
		printf("remotename(%s)\n", name);
# endif DEBUG

	/*
	**  First put this address into canonical form.
	**	First turn it into a macro.
	**	Then run it through ruleset 1 or 2, depending on whether
	**		it is a sender or a recipient address.
	**	If the mailer defines a rewriting set, run it through
	**		there next.
	*/

	/* save away the extraneous pretty stuff */
	fancy = crackaddr(name);

	/* now run through ruleset four */
	pvp = prescan(name, '\0');
	if (pvp == NULL)
		return (name);
	if (senderaddress)
	{
		rewrite(pvp, 1);
		if (m->m_s_rwset > 0)
			rewrite(pvp, m->m_s_rwset);
	}
	else
	{
		rewrite(pvp, 2);
		if (m->m_r_rwset > 0)
			rewrite(pvp, m->m_r_rwset);
	}

	/* now add any comment info we had before back */
	cataddr(pvp, lbuf, sizeof lbuf);
	define('g', lbuf);
	expand(fancy, buf, &buf[sizeof buf - 1], CurEnv);
	define('g', oldg);

# ifdef DEBUG
	if (tTd(12, 1))
		printf("remotename => `%s'\n", buf);
# endif DEBUG
	return (buf);
}
/*
**  CANONNAME -- make name canonical
**
**	This is used for SMTP and misc. printing.  Given a print
**	address, it strips out comments, etc., and puts on exactly
**	one set of brackets.
**
**	Parameters:
**		name -- the name to make canonical.
**
**	Returns:
**		pointer to canonical name.
**
**	Side Effects:
**		none.
**
**	Warning:
**		result is saved in static buf; future calls will trash it.
*/

char *
canonname(name)
	char *name;
{
	static char nbuf[MAXNAME];
	register char **pvp;

	pvp = prescan(name, '\0');
	rewrite(pvp, 3);
	cataddr(pvp, nbuf, sizeof nbuf);
	return (nbuf);
}
