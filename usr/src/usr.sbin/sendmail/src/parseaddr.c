# include "sendmail.h"

SCCSID(@(#)parseaddr.c	3.41.1.1		%G%);

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

	/*
	**  Initialize and prescan address.
	*/

	CurEnv->e_to = addr;
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
	if (a == NULL)
		return (NULL);
	m = a->q_mailer;

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
# define ONEMORE	6
# define GETONE		7
# define MACRO		8

char **
prescan(addr, delim)
	char *addr;
	char delim;
{
	register char *p;
	static char buf[MAXNAME+MAXATOM];
	static char *av[MAXATOM+1];
	char **avp;
	bool bslashmode;
	int cmntcnt;
	int brccnt;
	register char c;
	char *tok;
	register char *q;
	register int state;
	int nstate;
	extern char lower();

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
				if (nstate != ATOM)
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
				break;

			  case OPER:		/* operator */
				if (nstate == SPACE)
					continue;
				state = nstate;
				break;

			  case ONEMORE:		/* $- etc. */
				state = GETONE;
				break;

			  default:
				syserr("prescan: unknown state %d", state);
			}

			if (state == EOTOK || state == SPACE)
				break;

			/* squirrel it away */
			if (q >= &buf[sizeof buf - 5])
			{
				usrerr("Address too long");
				return (NULL);
			}
			*q++ = c;

			/* decide whether this represents end of token */
			if (state == OPER || state == GETONE)
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

		/* we prefer <> specs */
		if (c == '<')
		{
			if (brccnt < 0)
			{
				usrerr("multiple < spec");
				return (NULL);
			}
			brccnt++;
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

		if (avp >= &av[MAXATOM])
		{
			syserr("prescan: too many tokens");
			return (NULL);
		}
		*avp++ = tok;
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
		expand("$o", buf, &buf[sizeof buf - 1], CurEnv);
		strcat(buf, DELIMCHARS);
	}
	if (c == MATCHCLASS || c == MATCHREPL)
		return (ONEMORE);
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
**	This routine is an ordered production system.  Each rewrite
**	rule has a LHS (called the pattern) and a RHS (called the
**	rewrite); 'rwr' points the the current rewrite rule.
**
**	For each rewrite rule, 'avp' points the address vector we
**	are trying to match against, and 'pvp' points to the pattern.
**	If pvp points to a special match value (MATCHANY, MATCHONE,
**	MATCHCLASS) then the address in avp matched is saved away
**	in the match vector (pointed to by 'mvp').
**
**	When a match between avp & pvp does not match, we try to
**	back out.  If we back up over a MATCHONE or a MATCHCLASS
**	we must also back out the match in mvp.  If we reach a
**	MATCHANY we just extend the match and start over again.
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
	struct rewrite *rwr;		/* pointer to current rewrite rule */
	struct match mlist[MAXMATCH];	/* stores match on LHS */
	struct match *mlp;		/* cur ptr into mlist */
	char *npvp[MAXATOM+1];		/* temporary space for rebuild */
	extern bool sameword();

# ifdef DEBUG
	if (Debug > 9)
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
		mlp = mlist;
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
				if (s == NULL || (s->s_class & (1L << class)) == 0)
					goto fail;

				/* explicit fall-through */

			  case MATCHONE:
			  case MATCHANY:
				/* match exactly one token */
				mlp->first = mlp->last = avp++;
				mlp++;
				break;

			  default:
				/* must have exact match */
				if (!sameword(rp, ap))
					goto fail;
				avp++;
				break;
			}

			/* successful match on this token */
			rvp++;
			continue;

		  fail:
			/* match failed -- back up */
			while (--rvp >= rwr->r_lhs)
			{
				rp = *rvp;
				if (*rp == MATCHANY)
				{
					/* extend binding and continue */
					mlp[-1].last = avp++;
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
				if (*rp == MATCHREPL)
				{
					register struct match *m;
					register char **pp;

					m = &mlist[rp[1] - '1'];
# ifdef DEBUG
					if (Debug > 13)
					{
						printf("$%c:", rp[1]);
						pp = m->first;
						do
						{
							printf(" %x=\"", *pp);
							(void) fflush(stdout);
							printf("%s\"", *pp);
						} while (pp++ != m->last);
						printf("\n");
					}
# endif DEBUG
					pp = m->first;
					do
					{
						if (avp >= &npvp[MAXATOM])
						{
							syserr("rewrite: expansion too long");
							return;
						}
						*avp++ = *pp;
					} while (pp++ != m->last);
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
			if (Debug > 3)
			{
				char **vp;

				printf("rewritten as `");
				for (vp = pvp; *vp != NULL; vp++)
				{
					if (vp != pvp)
						printf("_");
					xputs(*vp);
				}
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
				strcat(buf, " ");
			strcat(buf, *tv);
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
			strcat(buf, *tv++);
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
		natomtok = (toktype(**pvp) == ATOM);
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
