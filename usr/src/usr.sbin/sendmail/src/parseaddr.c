/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)parseaddr.c	6.20 (Berkeley) %G%";
#endif /* not lint */

# include "sendmail.h"

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
**		copyf -- determines what shall be copied:
**			-1 -- don't copy anything.  The printname
**				(q_paddr) is just addr, and the
**				user & host are allocated internally
**				to parse.
**			0 -- copy out the parsed user & host, but
**				don't copy the printname.
**			+1 -- copy everything.
**		delim -- the character to terminate the address, passed
**			to prescan.
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
# define DELIMCHARS	"\201()<>,;\\\"\r\n"	/* word delimiters */

ADDRESS *
parseaddr(addr, a, copyf, delim, e)
	char *addr;
	register ADDRESS *a;
	int copyf;
	char delim;
	register ENVELOPE *e;
{
	register char **pvp;
	char pvpbuf[PSBUFSIZE];
	extern char **prescan();
	extern ADDRESS *buildaddr();
	extern bool invalidaddr();

	/*
	**  Initialize and prescan address.
	*/

	e->e_to = addr;
	if (tTd(20, 1))
		printf("\n--parseaddr(%s)\n", addr);

	if (invalidaddr(addr))
	{
		if (tTd(20, 1))
			printf("parseaddr-->bad address\n");
		return NULL;
	}

	pvp = prescan(addr, delim, pvpbuf);
	if (pvp == NULL)
	{
		if (tTd(20, 1))
			printf("parseaddr-->NULL\n");
		return (NULL);
	}

	/*
	**  Apply rewriting rules.
	**	Ruleset 0 does basic parsing.  It must resolve.
	*/

	rewrite(pvp, 3);
	rewrite(pvp, 0);

	/*
	**  See if we resolved to a real mailer.
	*/

	if ((pvp[0][0] & 0377) != CANONNET)
	{
		setstat(EX_USAGE);
		syserr("554 cannot resolve name");
		return (NULL);
	}

	/*
	**  Build canonical address from pvp.
	*/

	a = buildaddr(pvp, a);
	if (a == NULL)
		return (NULL);

	/*
	**  Make local copies of the host & user and then
	**  transport them out.
	*/

	allocaddr(a, copyf, addr);

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
invalidaddr(addr)
	register char *addr;
{
	for (; *addr != '\0'; addr++)
	{
		if ((*addr & 0340) != 0200)
			continue;
		setstat(EX_USAGE);
		usrerr("553 Address contained invalid control characters");
		return TRUE;
	}
	return FALSE;
}
/*
**  ALLOCADDR -- do local allocations of address on demand.
**
**	Also lowercases the host name if requested.
**
**	Parameters:
**		a -- the address to reallocate.
**		copyf -- the copy flag (see parseaddr for description).
**		paddr -- the printname of the address.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Copies portions of a into local buffers as requested.
*/

allocaddr(a, copyf, paddr)
	register ADDRESS *a;
	int copyf;
	char *paddr;
{
	register MAILER *m = a->q_mailer;

	if (copyf > 0 && paddr != NULL)
	{
		extern char *DelimChar;
		char savec = *DelimChar;

		*DelimChar = '\0';
		a->q_paddr = newstr(paddr);
		*DelimChar = savec;
	}
	else
		a->q_paddr = paddr;
	if (copyf >= 0)
	{
		if (a->q_host != NULL)
			a->q_host = newstr(a->q_host);
		else
			a->q_host = "";
		if (a->q_user != a->q_paddr)
			a->q_user = newstr(a->q_user);
	}

	if (a->q_paddr == NULL)
		a->q_paddr = a->q_user;

	/*
	**  Convert host name to lower case if requested.
	**	User name will be done later.
	*/

	if (!bitnset(M_HST_UPPER, m->m_flags))
		makelower(a->q_host);
}
/*
**  LOWERADDR -- map UPPER->lower case on addresses as requested.
**
**	Parameters:
**		a -- address to be mapped.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

loweraddr(a)
	register ADDRESS *a;
{
	register MAILER *m = a->q_mailer;

	if (!bitnset(M_USR_UPPER, m->m_flags))
		makelower(a->q_user);
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
**
**	Returns:
**		A pointer to a vector of tokens.
**		NULL on error.
**
**	Side Effects:
**		sets DelimChar to point to the character matching 'delim'.
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

# define NOCHAR		-1	/* signal nothing in lookahead token */

char	*DelimChar;		/* set to point to the delimiter */

char **
prescan(addr, delim, pvpbuf)
	char *addr;
	char delim;
	char pvpbuf[];
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
	static char *av[MAXATOM+1];
	extern int errno;

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
			if (c != NOCHAR)
			{
				/* see if there is room */
				if (q >= &pvpbuf[PSBUFSIZE - 5])
				{
					usrerr("553 Address too long");
					DelimChar = p;
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
					usrerr("553 Unbalanced '\"'");
					c = '"';
				}
				else if (cmntcnt > 0)
				{
					usrerr("553 Unbalanced '('");
					c = ')';
				}
				else if (anglecnt > 0)
				{
					c = '>';
					usrerr("553 Unbalanced '<'");
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
				c |= 0200;
				bslashmode = FALSE;
				continue;
			}

			if (c == '\\')
			{
				bslashmode = TRUE;
				c = NOCHAR;
				continue;
			}
			else if (state == QST)
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
					usrerr("553 Unbalanced ')'");
					DelimChar = p;
					return (NULL);
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
					usrerr("553 Unbalanced '>'");
					DelimChar = p;
					return (NULL);
				}
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
				DelimChar = p;
				return (NULL);
			}
			*avp++ = tok;
		}
	} while (c != '\0' && (c != delim || anglecnt > 0));
	*avp = NULL;
	DelimChar = --p;
	if (tTd(22, 12))
	{
		printf("prescan==>");
		printav(av);
	}
	if (av[0] != NULL)
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
	register int c;
{
	static char buf[50];
	static bool firstime = TRUE;

	if (firstime)
	{
		firstime = FALSE;
		(void) expand("$o", buf, &buf[sizeof buf - 1]);
		(void) strcat(buf, DELIMCHARS);
	}
	c &= 0377;
	if (c == MATCHCLASS || c == MATCHREPL || c == MATCHNCLASS)
		return (ONE);
	if (c == '"')
		return (QST);
	if ((c & 0340) == 0200)
		return (OPR);
	if (!isascii(c))
		return (ATM);
	if (isspace(c) || c == ')')
		return (SPC);
	if (strchr(buf, c) != NULL)
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

	if (OpMode == MD_TEST || tTd(21, 2))
	{
		printf("rewrite: ruleset %2d   input:", ruleset);
		printav(pvp);
	}
	if (ruleset < 0 || ruleset >= MAXRWSETS)
	{
		syserr("554 rewrite: illegal ruleset number %d", ruleset);
		return;
	}
	if (pvp == NULL)
		return;

	/*
	**  Run through the list of rewrite rules, applying
	**	any that match.
	*/

	for (rwr = RewriteRules[ruleset]; rwr != NULL; )
	{
		int loopcount = 0;

		if (tTd(21, 12))
		{
			printf("-----trying rule:");
			printav(rwr->r_lhs);
		}

		/* try to match on this rule */
		mlp = mlist;
		rvp = rwr->r_lhs;
		avp = pvp;
		while ((ap = *avp) != NULL || *rvp != NULL)
		{
			if (++loopcount > 100)
			{
				syserr("554 Infinite loop in ruleset %d", ruleset);
				printf("workspace: ");
				printav(pvp);
				break;
			}
			rp = *rvp;
			if (tTd(21, 35))
			{
				printf("rp=");
				xputs(rp);
				printf(", ap=");
				xputs(ap);
				printf("\n");
			}
			if (rp == NULL)
			{
				/* end-of-pattern before end-of-address */
				goto backup;
			}
			if (ap == NULL && (*rp & 0377) != MATCHZANY &&
			    (*rp & 0377) != CANONHOST)
			{
				/* end-of-input */
				break;
			}

			switch (*rp & 0377)
			{
				register STAB *s;

			  case MATCHCLASS:
			  case MATCHNCLASS:
				/* match any token in (not in) a class */
				s = stab(ap, ST_CLASS, ST_FIND);
				if (s == NULL || !bitnset(rp[1], s->s_class))
				{
					if ((*rp & 0377) == MATCHCLASS)
						goto backup;
				}
				else if ((*rp & 0377) == MATCHNCLASS)
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

			  case CANONHOST:
				/* match zero tokens */
				break;

			  default:
				/* must have exact match */
				if (strcasecmp(rp, ap))
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
				if ((*rp & 0377) == MATCHANY ||
				    (*rp & 0377) == MATCHZANY)
				{
					/* extend binding and continue */
					avp = ++mlp[-1].last;
					avp++;
					rvp++;
					break;
				}
				avp--;
				if ((*rp & 0377) == MATCHONE ||
				    (*rp & 0377) == MATCHCLASS ||
				    (*rp & 0377) == MATCHNCLASS)
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

		if (rvp < rwr->r_lhs || *rvp != NULL)
		{
			if (tTd(21, 10))
				printf("----- rule fails\n");
			rwr = rwr->r_next;
			continue;
		}

		rvp = rwr->r_rhs;
		if (tTd(21, 12))
		{
			printf("-----rule matches:");
			printav(rvp);
		}

		rp = *rvp;
		if ((*rp & 0377) == CANONUSER)
		{
			rvp++;
			rwr = rwr->r_next;
		}
		else if ((*rp & 0377) == CANONHOST)
		{
			rvp++;
			rwr = NULL;
		}
		else if ((*rp & 0377) == CANONNET)
			rwr = NULL;

		/* substitute */
		for (avp = npvp; *rvp != NULL; rvp++)
		{
			register struct match *m;
			register char **pp;

			rp = *rvp;
			if ((*rp & 0377) == MATCHREPL)
			{
				/* substitute from LHS */
				m = &mlist[rp[1] - '1'];
				if (m < mlist || m >= mlp)
				{
					syserr("554 rewrite: ruleset %d: replacement $%c out of bounds",
						ruleset, rp[1]);
					return;
				}
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
				pp = m->first;
				while (pp <= m->last)
				{
					if (avp >= &npvp[MAXATOM])
					{
						syserr("554 rewrite: expansion too long");
						return;
					}
					*avp++ = *pp++;
				}
			}
			else
			{
				/* vanilla replacement */
				if (avp >= &npvp[MAXATOM])
				{
	toolong:
					syserr("554 rewrite: expansion too long");
					return;
				}
				*avp++ = rp;
			}
		}
		*avp++ = NULL;

		/*
		**  Check for any hostname/keyword lookups.
		*/

		for (rvp = npvp; *rvp != NULL; rvp++)
		{
			char **hbrvp;
			char **xpvp;
			int trsize;
			char *olddelimchar;
			char *replac;
			int endtoken;
			STAB *map;
			char *mapname;
			char **key_rvp;
			char **arg_rvp;
			char **default_rvp;
			char buf[MAXNAME + 1];
			char *pvpb1[MAXATOM + 1];
			char *argvect[10];
			char pvpbuf[PSBUFSIZE];
			extern char *DelimChar;

			if ((**rvp & 0377) != HOSTBEGIN &&
			    (**rvp & 0377) != LOOKUPBEGIN)
				continue;

			/*
			**  Got a hostname/keyword lookup.
			**
			**	This could be optimized fairly easily.
			*/

			hbrvp = rvp;
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
					cataddr(xpvp, replac,
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
				cataddr(xpvp, replac,
					&pvpbuf[sizeof pvpbuf] - replac, 
					'\0');
				*++arg_rvp = replac;
			}
			*++arg_rvp = NULL;

			/* save the remainder of the input string */
			trsize = (int) (avp - rvp + 1) * sizeof *rvp;
			bcopy((char *) rvp, (char *) pvpb1, trsize);

			/* look it up */
			cataddr(key_rvp, buf, sizeof buf, '\0');
			argvect[0] = buf;
			if (map != NULL && bitset(MF_VALID, map->s_map.map_flags))
			{
				int bsize = sizeof buf - 1;

				if (map->s_map.map_app != NULL)
					bsize -= strlen(map->s_map.map_app);
				replac = (*map->s_map.map_class->map_lookup)(&map->s_map,
						buf, sizeof buf - 1, argvect);
				if (replac != NULL && map->s_map.map_app != NULL)
					strcat(replac, map->s_map.map_app);
			}
			else
				replac = NULL;

			/* if no replacement, use default */
			if (replac == NULL && default_rvp != NULL)
			{
				char buf2[sizeof buf];

				/* rewrite the default with % translations */
				cataddr(default_rvp, buf2, sizeof buf2, '\0');
				map_rewrite(buf2, sizeof buf2, buf, sizeof buf,
					argvect);
				replac = buf;
			}

			if (replac == NULL)
			{
				xpvp = key_rvp;
			}
			else
			{
				/* scan the new replacement */
				olddelimchar = DelimChar;
				xpvp = prescan(replac, '\0', pvpbuf);
				DelimChar = olddelimchar;
				if (xpvp == NULL)
				{
					syserr("553 rewrite: cannot prescan map value: %s", replac);
					return;
				}
			}

			/* append it to the token list */
			for (avp = hbrvp; *xpvp != NULL; xpvp++)
			{
				*avp++ = newstr(*xpvp);
				if (avp >= &npvp[MAXATOM])
					goto toolong;
			}

			/* restore the old trailing information */
			for (xpvp = pvpb1; (*avp++ = *xpvp++) != NULL; )
				if (avp >= &npvp[MAXATOM])
					goto toolong;

			break;
		}

		/*
		**  Check for subroutine calls.
		*/

		if (**npvp == CALLSUBR)
		{
			bcopy((char *) &npvp[2], (char *) pvp,
				(int) (avp - npvp - 2) * sizeof *avp);
			if (tTd(21, 3))
				printf("-----callsubr %s\n", npvp[1]);
			rewrite(pvp, atoi(npvp[1]));
		}
		else
		{
			bcopy((char *) npvp, (char *) pvp,
				(int) (avp - npvp) * sizeof *avp);
		}
		if (tTd(21, 4))
		{
			printf("rewritten as:");
			printav(pvp);
		}
	}

	if (OpMode == MD_TEST || tTd(21, 2))
	{
		printf("rewrite: ruleset %2d returns:", ruleset);
		printav(pvp);
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

ADDRESS *
buildaddr(tv, a)
	register char **tv;
	register ADDRESS *a;
{
	struct mailer **mp;
	register struct mailer *m;
	char *bp;
	int spaceleft;
	static char buf[MAXNAME];

	if (a == NULL)
		a = (ADDRESS *) xalloc(sizeof *a);
	bzero((char *) a, sizeof *a);

	/* figure out what net/mailer to use */
	if ((**tv & 0377) != CANONNET)
	{
		syserr("554 buildaddr: no net");
		return (NULL);
	}
	tv++;
	if (!strcasecmp(*tv, "error"))
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
		if ((**tv & 0377) != CANONUSER)
			syserr("554 buildaddr: error: no user");
		cataddr(++tv, buf, sizeof buf, ' ');
		stripquotes(buf);
		usrerr(buf);
		return (NULL);
	}

	for (mp = Mailer; (m = *mp++) != NULL; )
	{
		if (!strcasecmp(m->m_name, *tv))
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
	if (!bitnset(M_LOCALMAILER, m->m_flags))
	{
		if ((**tv & 0377) != CANONHOST)
		{
			syserr("554 buildaddr: no host");
			return (NULL);
		}
		bp = buf;
		spaceleft = sizeof buf - 1;
		while (*++tv != NULL && (**tv & 0377) != CANONUSER)
		{
			int i = strlen(*tv);

			if (i > spaceleft)
			{
				/* out of space for this address */
				if (spaceleft >= 0)
					syserr("554 buildaddr: host too long (%.40s...)",
						buf);
				i = spaceleft;
				spaceleft = 0;
			}
			if (i <= 0)
				continue;
			bcopy(*tv, bp, i);
			bp += i;
			spaceleft -= i;
		}
		*bp = '\0';
		a->q_host = newstr(buf);
	}
	else
		a->q_host = NULL;

	/* figure out the user */
	if (*tv == NULL || (**tv & 0377) != CANONUSER)
	{
		syserr("554 buildaddr: no user");
		return (NULL);
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
			cataddr(tv, buf, sizeof buf, '\0');
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

	/* rewrite according recipient mailer rewriting rules */
	rewrite(tv, 2);
	if (m->m_re_rwset > 0)
		rewrite(tv, m->m_re_rwset);
	rewrite(tv, 4);

	/* save the result for the command line/RCPT argument */
	cataddr(tv, buf, sizeof buf, '\0');
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
**		spacesub -- the space separator character; if null,
**			use SpaceSub.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Destroys buf.
*/

cataddr(pvp, buf, sz, spacesub)
	char **pvp;
	char *buf;
	register int sz;
	char spacesub;
{
	bool oatomtok = FALSE;
	bool natomtok = FALSE;
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
	/* if they don't have the same mailer, forget it */
	if (a->q_mailer != b->q_mailer)
		return (FALSE);

	/* if the user isn't the same, we can drop out */
	if (strcmp(a->q_user, b->q_user) != 0)
		return (FALSE);

	/* if the mailer ignores hosts, we have succeeded! */
	if (bitnset(M_LOCALMAILER, a->q_mailer->m_flags))
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
	bool first = TRUE;
	register MAILER *m;
	MAILER pseudomailer;

	while (a != NULL)
	{
		first = FALSE;
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

		printf("%s: mailer %d (%s), host `%s', user `%s', ruser `%s'\n",
		       a->q_paddr, m->m_mno, m->m_name,
		       a->q_host, a->q_user, a->q_ruser? a->q_ruser: "<null>");
		printf("\tnext=%x, flags=%o, alias %x\n", a->q_next, a->q_flags,
		       a->q_alias);
		printf("\thome=\"%s\", fullname=\"%s\"\n", a->q_home,
		       a->q_fullname);

		if (!follow)
			return;
		a = a->q_next;
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
**		senderaddress -- if set, uses the sender rewriting rules
**			rather than the recipient rewriting rules.
**		header -- set if this address is in the header, rather
**			than an envelope header.
**		canonical -- if set, strip out any comment information,
**			etc.
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
remotename(name, m, senderaddress, header, canonical, e)
	char *name;
	struct mailer *m;
	bool senderaddress;
	bool header;
	bool canonical;
	register ENVELOPE *e;
{
	register char **pvp;
	char *fancy;
	extern char *macvalue();
	char *oldg = macvalue('g', e);
	int rwset;
	static char buf[MAXNAME];
	char lbuf[MAXNAME];
	char pvpbuf[PSBUFSIZE];
	extern char **prescan();
	extern char *crackaddr();

	if (tTd(12, 1))
		printf("remotename(%s)\n", name);

	/* don't do anything if we are tagging it as special */
	if (senderaddress)
		rwset = header ? m->m_sh_rwset : m->m_se_rwset;
	else
		rwset = header ? m->m_rh_rwset : m->m_re_rwset;
	if (rwset < 0)
		return (name);

	/*
	**  Do a heuristic crack of this name to extract any comment info.
	**	This will leave the name as a comment and a $g macro.
	*/

	if (canonical || bitnset(M_NOCOMMENT, m->m_flags))
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

	pvp = prescan(name, '\0', pvpbuf);
	if (pvp == NULL)
		return (name);
	rewrite(pvp, 3);
	if (e->e_fromdomain != NULL)
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
			rewrite(pvp, 3);
		}
	}

	/*
	**  Do more specific rewriting.
	**	Rewrite using ruleset 1 or 2 depending on whether this is
	**		a sender address or not.
	**	Then run it through any receiving-mailer-specific rulesets.
	*/

	if (senderaddress)
		rewrite(pvp, 1);
	else
		rewrite(pvp, 2);
	if (rwset > 0)
		rewrite(pvp, rwset);

	/*
	**  Do any final sanitation the address may require.
	**	This will normally be used to turn internal forms
	**	(e.g., user@host.LOCAL) into external form.  This
	**	may be used as a default to the above rules.
	*/

	rewrite(pvp, 4);

	/*
	**  Now restore the comment information we had at the beginning.
	*/

	cataddr(pvp, lbuf, sizeof lbuf, '\0');
	define('g', lbuf, e);
	expand(fancy, buf, &buf[sizeof buf - 1], e);
	define('g', oldg, e);

	if (tTd(12, 1))
		printf("remotename => `%s'\n", buf);
	return (buf);
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
	char pvpbuf[PSBUFSIZE];

	if (tTd(29, 1))
	{
		printf("maplocaluser: ");
		printaddr(a, FALSE);
	}
	pvp = prescan(a->q_user, '\0', pvpbuf);
	if (pvp == NULL)
		return;

	rewrite(pvp, 5);
	if (pvp[0] == NULL || (pvp[0][0] & 0377) != CANONNET)
		return;

	/* if non-null, mailer destination specified -- has it changed? */
	a1 = buildaddr(pvp, NULL);
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
	allocaddr(a1, 1, NULL);
	(void) recipient(a1, sendq, e);
}
