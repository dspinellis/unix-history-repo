# include "sendmail.h"

static char	SccsId[] = "@(#)parseaddr.c	3.19	%G%";

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
**
**	Called By:
**		main
**		sendto
**		alias
**		savemail
*/

# define DELIMCHARS	"$()<>,;\\\"\r\n"	/* word delimiters */
# define SPACESUB	('.'|0200)		/* substitution for <lwsp> */

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

	/*
	**  Initialize and prescan address.
	*/

	To = addr;
# ifdef DEBUG
	if (Debug)
		printf("\n--parse(%s)\n", addr);
# endif DEBUG

	pvp = prescan(addr, '\0');
	if (pvp == NULL)
		return (NULL);

	/*
	**  Apply rewriting rules.
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
	m = Mailer[a->q_mailer];

	/*
	**  Make local copies of the host & user and then
	**  transport them out.
	*/

	if (copyf > 0)
		a->q_paddr = newstr(addr);
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
	if (Debug)
		printf("parse(\"%s\"): host \"%s\" user \"%s\" mailer %d\n",
		    addr, a->q_host, a->q_user, a->q_mailer);
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
**			are moving in place; set buflim to high core.
**
**	Returns:
**		A pointer to a vector of tokens.
**		NULL on error.
**
**	Side Effects:
**		none.
*/

# define OPER		1
# define ATOM		2
# define EOTOK		3
# define QSTRING	4
# define SPACE		5
# define DOLLAR		6
# define GETONE		7

char **
prescan(addr, delim)
	char *addr;
	char delim;
{
	register char *p;
	static char buf[MAXNAME+MAXATOM];
	static char *av[MAXATOM+1];
	char **avp;
	bool space;
	bool bslashmode;
	int cmntcnt;
	int brccnt;
	register char c;
	char *tok;
	register char *q;
	register int state;
	int nstate;
	extern char lower();

	space = FALSE;
	q = buf;
	bslashmode = FALSE;
	cmntcnt = brccnt = 0;
	avp = av;
	state = OPER;
	for (p = addr; *p != '\0' && *p != delim; )
	{
		/* read a token */
		tok = q;
		while ((c = *p++) != '\0' && c != delim)
		{
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
				continue;
			}
			else if (c == '"')
			{
				if (state == QSTRING)
					state = OPER;
				else
					state = QSTRING;
				break;
			}

			nstate = toktype(c);
			switch (state)
			{
			  case QSTRING:		/* in quoted string */
				break;

			  case ATOM:		/* regular atom */
				state = nstate;
				if (state != ATOM)
				{
					state = EOTOK;
					p--;
				}
				break;

			  case GETONE:		/* grab one character */
				state = OPER;
				break;

			  case EOTOK:		/* after atom or q-string */
				state = nstate;
				if (state == SPACE)
					continue;
				break;

			  case SPACE:		/* linear white space */
				state = nstate;
				space = TRUE;
				continue;

			  case OPER:		/* operator */
				if (nstate == SPACE)
					continue;
				state = nstate;
				break;

			  case DOLLAR:		/* $- etc. */
				state = OPER;
				switch (c)
				{
				  case '$':		/* literal $ */
					break;

				  case '+':		/* match anything */
					c = MATCHANY;
					state = GETONE;
					break;

				  case '-':		/* match one token */
					c = MATCHONE;
					state = GETONE;
					break;

				  case '=':		/* match one token of class */
					c = MATCHCLASS;
					state = GETONE;
					break;

				  case '#':		/* canonical net name */
					c = CANONNET;
					break;

				  case '@':		/* canonical host name */
					c = CANONHOST;
					break;

				  case ':':		/* canonical user name */
					c = CANONUSER;
					break;

				  default:
					c = '$';
					state = OPER;
					p--;
					break;
				}
				break;

			  default:
				syserr("prescan: unknown state %d", state);
			}

			if (state == OPER)
				space = FALSE;
			else if (state == EOTOK)
				break;
			if (c == '$' && delim == '\t')
			{
				state = DOLLAR;
				continue;
			}

			/* squirrel it away */
			if (q >= &buf[sizeof buf - 5])
			{
				usrerr("Address too long");
				return (NULL);
			}
			if (space)
				*q++ = SPACESUB;
			*q++ = c;

			/* decide whether this represents end of token */
			if (state == OPER)
				break;
		}
		if (c == '\0' || c == delim)
			p--;

		/* new token */
		if (tok == q)
			continue;
		*q++ = '\0';

		c = tok[0];
		if (c == '(')
		{
			cmntcnt++;
			continue;
		}
		else if (c == ')')
		{
			if (cmntcnt <= 0)
			{
				usrerr("Unbalanced ')'");
				return (NULL);
			}
			else
			{
				cmntcnt--;
				continue;
			}
		}
		else if (cmntcnt > 0)
			continue;

		*avp++ = tok;

		/* we prefer <> specs */
		if (c == '<')
		{
			if (brccnt < 0)
			{
				usrerr("multiple < spec");
				return (NULL);
			}
			brccnt++;
			space = FALSE;
			if (brccnt == 1)
			{
				/* we prefer using machine readable name */
				q = buf;
				*q = '\0';
				avp = av;
				continue;
			}
		}
		else if (c == '>')
		{
			if (brccnt <= 0)
			{
				usrerr("Unbalanced `>'");
				return (NULL);
			}
			else
				brccnt--;
			if (brccnt <= 0)
			{
				brccnt = -1;
				continue;
			}
		}

		/*
		**  Turn "at" into "@",
		**	but only if "at" is a word.
		*/

		if (lower(tok[0]) == 'a' && lower(tok[1]) == 't' && tok[2] == '\0')
		{
			tok[0] = '@';
			tok[1] = '\0';
		}
	}
	*avp = NULL;
	if (cmntcnt > 0)
		usrerr("Unbalanced '('");
	else if (brccnt > 0)
		usrerr("Unbalanced '<'");
	else if (state == QSTRING)
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
		(void) expand("$o", buf, &buf[sizeof buf - 1]);
		strcat(buf, DELIMCHARS);
	}
	if (!isascii(c))
		return (ATOM);
	if (isspace(c))
		return (SPACE);
	if (iscntrl(c) || index(buf, c) != NULL)
		return (OPER);
	return (ATOM);
}
/*
**  REWRITE -- apply rewrite rules to token vector.
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
	char	**firsttok;	/* first token matched */
	char	**lasttok;	/* last token matched */
	char	name;		/* name of parameter */
};

# define MAXMATCH	8	/* max params per rewrite */


rewrite(pvp, ruleset)
	char **pvp;
	int ruleset;
{
	register char *ap;		/* address pointer */
	register char *rp;		/* rewrite pointer */
	register char **avp;		/* address vector pointer */
	register char **rvp;		/* rewrite vector pointer */
	struct rewrite *rwr;
	struct match mlist[MAXMATCH];
	char *npvp[MAXATOM+1];		/* temporary space for rebuild */
	extern bool sameword();

# ifdef DEBUG
	if (Debug > 10)
	{
		printf("rewrite: original pvp:\n");
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
		if (Debug > 10)
		{
			printf("-----trying rule:\n");
			printav(rwr->r_lhs);
		}
# endif DEBUG

		/* try to match on this rule */
		clrmatch(mlist);
		for (rvp = rwr->r_lhs, avp = pvp; *avp != NULL; )
		{
			ap = *avp;
			rp = *rvp;

			if (rp == NULL)
			{
				/* end-of-pattern before end-of-address */
				goto fail;
			}

			switch (*rp)
			{
				register STAB *s;
				register int class;

			  case MATCHONE:
				/* match exactly one token */
				setmatch(mlist, rp[1], avp, avp);
				break;

			  case MATCHANY:
				/* match any number of tokens */
				setmatch(mlist, rp[1], (char **) NULL, avp);
				break;

			  case MATCHCLASS:
				/* match any token in a class */
				class = rp[1];
				if (!isalpha(class))
					goto fail;
				if (isupper(class))
					class -= 'A';
				else
					class -= 'a';
				s = stab(ap, ST_CLASS, ST_FIND);
				if (s == NULL || (s->s_class & (1 << class)) == 0)
					goto fail;
				break;

			  default:
				/* must have exact match */
				if (!sameword(rp, ap))
					goto fail;
				break;
			}

			/* successful match on this token */
			avp++;
			rvp++;
			continue;

		  fail:
			/* match failed -- back up */
			while (--rvp >= rwr->r_lhs)
			{
				rp = *rvp;
				if (*rp == MATCHANY)
					break;

				/* can't extend match: back up everything */
				avp--;

				if (*rp == MATCHONE)
				{
					/* undo binding */
					setmatch(mlist, rp[1], (char **) NULL, (char **) NULL);
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
# ifdef DEBUG
			if (Debug > 10)
			{
				printf("-----rule matches:\n");
				printav(rwr->r_rhs);
			}
# endif DEBUG

			/* substitute */
			for (rvp = rwr->r_rhs, avp = npvp; *rvp != NULL; rvp++)
			{
				rp = *rvp;
				if (*rp == MATCHANY)
				{
					register struct match *m;
					register char **pp;
					extern struct match *findmatch();

					m = findmatch(mlist, rp[1]);
					if (m != NULL)
					{
						pp = m->firsttok;
						do
						{
							*avp++ = *pp;
						} while (pp++ != m->lasttok);
					}
				}
				else
					*avp++ = rp;
			}
			*avp++ = NULL;
			bmove((char *) npvp, (char *) pvp, (avp - npvp) * sizeof *avp);
# ifdef DEBUG
			if (Debug)
			{
				char **vp;

				printf("rewritten as `");
				for (vp = pvp; *vp != NULL; vp++)
					xputs(*vp);
				printf("'\n");
			}
# endif DEBUG
			if (pvp[0][0] == CANONNET)
				break;
		}
		else
		{
# ifdef DEBUG
			if (Debug > 10)
				printf("----- rule fails\n");
# endif DEBUG
			rwr = rwr->r_next;
		}
	}
}
/*
**  SETMATCH -- set parameter value in match vector
**
**	Parameters:
**		mlist -- list of match values.
**		name -- the character name of this parameter.
**		first -- the first location of the replacement.
**		last -- the last location of the replacement.
**
**		If last == NULL, delete this entry.
**		If first == NULL, extend this entry (or add it if
**			it does not exist).
**
**	Returns:
**		nothing.
**
**	Side Effects:
**		munges with mlist.
*/

setmatch(mlist, name, first, last)
	struct match *mlist;
	char name;
	char **first;
	char **last;
{
	register struct match *m;
	struct match *nullm = NULL;

	for (m = mlist; m < &mlist[MAXMATCH]; m++)
	{
		if (m->name == name)
			break;
		if (m->name == '\0')
			nullm = m;
	}

	if (m >= &mlist[MAXMATCH])
		m = nullm;

	if (last == NULL)
	{
		m->name = '\0';
		return;
	}

	if (m->name == '\0')
	{
		if (first == NULL)
			m->firsttok = last;
		else
			m->firsttok = first;
	}
	m->name = name;
	m->lasttok = last;
}
/*
**  FINDMATCH -- find match in mlist
**
**	Parameters:
**		mlist -- list to search.
**		name -- name to find.
**
**	Returns:
**		pointer to match structure.
**		NULL if no match.
**
**	Side Effects:
**		none.
*/

struct match *
findmatch(mlist, name)
	struct match *mlist;
	char name;
{
	register struct match *m;

	for (m = mlist; m < &mlist[MAXMATCH]; m++)
	{
		if (m->name == name)
			return (m);
	}

	return (NULL);
}
/*
**  CLRMATCH -- clear match list
**
**	Parameters:
**		mlist -- list to clear.
**
**	Returns:
**		none.
**
**	Side Effects:
**		mlist is cleared.
*/

clrmatch(mlist)
	struct match *mlist;
{
	register struct match *m;

	for (m = mlist; m < &mlist[MAXMATCH]; m++)
		m->name = '\0';
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
**		'a'
**
**	Side Effects:
**		fills in 'a'
*/

ADDRESS *
buildaddr(tv, a)
	register char **tv;
	register ADDRESS *a;
{
	register int i;
	static char buf[MAXNAME];
	struct mailer **mp;
	register struct mailer *m;

	if (a == NULL)
		a = (ADDRESS *) xalloc(sizeof *a);
	a->q_flags = 0;
	a->q_home = NULL;

	/* figure out what net/mailer to use */
	if (**tv != CANONNET)
		syserr("buildaddr: no net");
	tv++;
	for (mp = Mailer, i = 0; (m = *mp++) != NULL; i++)
	{
		if (strcmp(m->m_name, *tv) == 0)
			break;
	}
	if (m == NULL)
		syserr("buildaddr: unknown net %s", *tv);
	a->q_mailer = i;

	/* figure out what host (if any) */
	tv++;
	if (!bitset(M_LOCAL, m->m_flags))
	{
		if (**tv != CANONHOST)
			syserr("buildaddr: no host");
		tv++;
		a->q_host = *tv;
		tv++;
	}
	else
		a->q_host = NULL;

	/* figure out the user */
	if (**tv != CANONUSER)
		syserr("buildaddr: no user");
	buf[0] = '\0';
	while (**++tv != NULL)
		(void) strcat(buf, *tv);
	a->q_user = buf;

	return (a);
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
	if (bitset(M_LOCAL, Mailer[a->q_mailer]->m_flags))
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

printaddr(a, follow)
	register ADDRESS *a;
	bool follow;
{
	while (a != NULL)
	{
		printf("addr@%x: ", a);
		(void) fflush(stdout);
		printf("%s: mailer %d (%s), host `%s', user `%s'\n", a->q_paddr,
		       a->q_mailer, Mailer[a->q_mailer]->m_name, a->q_host, a->q_user);
		printf("\tnext=%x flags=%o, rmailer %d\n", a->q_next,
		       a->q_flags, a->q_rmailer);

		if (!follow)
			return;
		a = a->q_next;
	}
}
