# include <stdio.h>
# include <ctype.h>
# include "dlvrmail.h"

static char	SccsId[] = "@(#)parse.c	2.5	1/8/81";

/*
**  PARSE -- Parse an address
**
**	Parses an address and breaks it up into three parts: a
**	net to transmit the message on, the host to transmit it
**	to, and a user on that host.  These are loaded into an
**	addrq header with the values squirreled away if necessary.
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

# define DELIMCHARS	"()<>@!.,;:\\\" \t\r\n"	/* word delimiters */
# define SPACESUB	('.'|0200)		/* substitution for <lwsp> */

addrq *
parse(addr, a, copyf)
	char *addr;
	register addrq *a;
	int copyf;
{
	register char *p;
	register struct parsetab *t;
	extern struct parsetab ParseTab[];
	static char buf[MAXNAME];
	register char c;
	register char *q;
	bool got_one;
	extern char *prescan();
	extern char *xalloc();
	char **pvp;

	/*
	**  Initialize and prescan address.
	*/

	To = addr;
	if (prescan(addr, buf, &buf[sizeof buf], '\0') == NULL)
		return (NULL);

	/*
	**  Scan parse table.
	**	Look for the first entry designating a character
	**		that is contained in the address.
	**	Arrange for q to point to that character.
	**	Check to see that there is only one of the char
	**		if it must be unique.
	**	Find the last one if the host is on the RHS.
	**	Insist that the host name is atomic.
	**	If just doing a map, do the map and then start all
	**		over.
	*/

 rescan:
	got_one = FALSE;
	for (t = ParseTab; t->p_char != '\0'; t++)
	{
		q = NULL;
		for (p = buf; (c = *p) != '\0'; p++)
		{
			/* find the end of this token */
			while (isalnum(c) || c == '-' || c == '_')
				c = *++p;
			if (c == '\0')
				break;

			if (c == t->p_char)
			{
				got_one = TRUE;

				/* do mapping as appropriate */
				if (flagset(P_MAP, t->p_flags))
				{
					*p = t->p_arg[0];
					if (flagset(P_ONE, t->p_flags))
						goto rescan;
					else
						continue;
				}

				/* arrange for q to point to it */
				if (q != NULL && flagset(P_ONE, t->p_flags))
				{
					usrerr("multichar error");
					ExitStat = EX_USAGE;
					return (NULL);
				}
				if (q == NULL || flagset(P_HLAST, t->p_flags))
					q = p;
			}
			else
			{
				/* insist that host name is atomic */
				if (flagset(P_HLAST, t->p_flags))
					q = NULL;
				else
					break;
			}
		}

		if (q != NULL)
			break;
	}

	/*
	**  If we matched nothing cleanly, but we did match something
	**  somewhere in the process of scanning, then we have a
	**  syntax error.  This can happen on things like a@b:c where
	**  @ has a right host and : has a left host.
	**
	**  We also set `q' to the null string, in case someone forgets
	**  to put the P_MOVE bit in the local mailer entry of the
	**  configuration table.
	*/

	if (q == NULL)
	{
		q = "";
		if (got_one)
		{
			usrerr("syntax error");
			ExitStat = EX_USAGE;
			return (NULL);
		}
	}

	/*
	**  Interpret entry.
	**	t points to the entry for the mailer we will use.
	**	q points to the significant character.
	*/

	if (a == NULL)
		a = (addrq *) xalloc(sizeof *a);
	if (copyf > 0)
	{
		p = xalloc((unsigned) strlen(addr) + 1);
		strcpy(p, addr);
		a->q_paddr = p;
	}
	else
		a->q_paddr = addr;
	a->q_mailer = &Mailer[t->p_mailer];

	if (flagset(P_MOVE, t->p_flags))
	{
		/* send the message to another host & retry */
		a->q_host = t->p_arg;
		if (copyf >= 0)
		{
			p = xalloc((unsigned) strlen(buf) + 1);
			strcpy(p, buf);
			a->q_user = p;
		}
		else
			a->q_user = buf;
	}
	else
	{
		/*
		**  Make local copies of the host & user and then
		**  transport them out.
		*/

		*q++ = '\0';
		if (flagset(P_HLAST, t->p_flags))
		{
			a->q_host = q;
			a->q_user = buf;
		}
		else
		{
			a->q_host = buf;
			a->q_user = q;
		}

		/*
		**  Don't go to the net if already on the target host.
		**	This is important on the berkeley network, since
		**	it get confused if we ask to send to ourselves.
		**	For nets like the ARPANET, we probably will have
		**	the local list set to NULL to simplify testing.
		**	The canonical representation of the name is also set
		**	to be just the local name so the duplicate letter
		**	suppression algorithm will work.
		*/

		if ((pvp = a->q_mailer->m_local) != NULL)
		{
			while (*pvp != NULL)
			{
				auto char buf2[MAXNAME];

				strcpy(buf2, a->q_host);
				if (!flagset(P_HST_UPPER, t->p_flags))
					makelower(buf2);
				if (strcmp(*pvp++, buf2) == 0)
				{
					strcpy(buf2, a->q_user);
					p = a->q_paddr;
					if (parse(buf2, a, -1) == NULL)
					{
						To = addr;
						return (NULL);
					}
					To = a->q_paddr = p;
					break;
				}
			}
		}

		/* make copies if specified */
		if (copyf >= 0)
		{
			p = xalloc((unsigned) strlen(a->q_host) + 1);
			strcpy(p, a->q_host);
			a->q_host = p;
			p = xalloc((unsigned) strlen(a->q_user) + 1);
			strcpy(p, a->q_user);
			a->q_user = p;
		}
	}

	/*
	**  Do UPPER->lower case mapping unless inhibited.
	*/

	if (!flagset(P_HST_UPPER, t->p_flags))
		makelower(a->q_host);
	if (!flagset(P_USR_UPPER, t->p_flags))
		makelower(a->q_user);

	/*
	**  Compute return value.
	*/

# ifdef DEBUG
	if (Debug)
		printf("parse(\"%s\"): host \"%s\" user \"%s\" mailer %d\n",
		    addr, a->q_host, a->q_user, t->p_mailer);
# endif DEBUG

	return (a);
}
/*
**  MAKELOWER -- Translate a line into lower case
**
**	Parameters:
**		p -- the string to translate.  If NULL, return is
**			immediate.
**
**	Returns:
**		none.
**
**	Side Effects:
**		String pointed to by p is translated to lower case.
**
**	Called By:
**		parse
*/

makelower(p)
	register char *p;
{
	register char c;

	if (p == NULL)
		return;
	for (; (c = *p) != '\0'; p++)
		if ((c & 0200) == 0 && isupper(c))
			*p = c - 'A' + 'a';
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
**		buf -- the buffer to copy it into.
**		buflim -- the last usable address in the buffer
**			(which will old a null byte).  Normally
**			&buf[sizeof buf - 1].
**		delim -- the delimiter for the address, normally
**			'\0' or ','; \0 is accepted in any case.
**			are moving in place; set buflim to high core.
**
**	Returns:
**		A pointer to the terminator of buf.
**		NULL on error.
**
**	Side Effects:
**		buf gets clobbered.
**
**	Called By:
**		parse
**		maketemp
*/

char *
prescan(addr, buf, buflim, delim)
	char *addr;
	char *buf;
	char *buflim;
	char delim;
{
	register char *p;
	bool space;
	bool quotemode;
	bool bslashmode;
	bool delimmode;
	int cmntcnt;
	int brccnt;
	register char c;
	register char *q;
	extern bool any();

	space = FALSE;
	delimmode = TRUE;
	q = buf;
	bslashmode = quotemode = FALSE;
	cmntcnt = brccnt = 0;
	for (p = addr; (c = *p++) != '\0'; )
	{
		/* chew up special characters */
		*q = '\0';
		if (bslashmode)
		{
			c |= 0200;
			bslashmode = FALSE;
		}
		else if (c == '"')
			quotemode = !quotemode;
		else if (c == '\\')
		{
			bslashmode++;
			continue;
		}
		else if (quotemode)
			c |= 0200;
		else if (c == delim)
			break;
		else if (c == '(')
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
		if (cmntcnt > 0)
			continue;
		else if (isascii(c) && isspace(c) && (space || delimmode))
			continue;
		else if (c == '<')
		{
			if (brccnt < 0)
			{
				usrerr("multiple < spec");
				return (NULL);
			}
			brccnt++;
			delimmode = TRUE;
			space = FALSE;
			if (brccnt == 1)
			{
				/* we prefer using machine readable name */
				q = buf;
				*q = '\0';
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
		**	By the way, I violate the ARPANET RFC-733
		**	standard here, by assuming that 'space' delimits
		**	atoms.  I assume that is just a mistake, since
		**	it violates the spirit of the semantics
		**	of the document.....
		*/

		if (delimmode && (c == 'a' || c == 'A') &&
		    (p[0] == 't' || p[0] == 'T') &&
		    (any(p[1], DELIMCHARS) || p[1] <= 040))
		{
			c = '@';
			p++;
		}

		if (delimmode = any(c, DELIMCHARS))
			space = FALSE;

		/* if not a space, squirrel it away */
		if ((!isascii(c) || !isspace(c)) && brccnt >= 0)
		{
			if (q >= buflim-1)
			{
				usrerr("Address too long");
				return (NULL);
			}
			if (space)
				*q++ = SPACESUB;
			*q++ = c;
		}
		space = isascii(c) && isspace(c);
	}
	*q = '\0';
	if (c == '\0')
		p--;
	if (cmntcnt > 0)
		usrerr("Unbalanced '('");
	else if (quotemode)
		usrerr("Unbalanced '\"'");
	else if (brccnt > 0)
		usrerr("Unbalanced '<'");
	else if (buf[0] != '\0')
		return (p);
	return (NULL);
}
