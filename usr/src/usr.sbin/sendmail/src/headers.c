# include <errno.h>
# include "sendmail.h"

SCCSID(@(#)headers.c	3.31		%G%);

/*
**  CHOMPHEADER -- process and save a header line.
**
**	Called by collect and by readcf to deal with header lines.
**
**	Parameters:
**		line -- header as a text line.
**		def -- if set, this is a default value.
**
**	Returns:
**		flags for this header.
**
**	Side Effects:
**		The header is saved on the header list.
**		Contents of 'line' are destroyed.
*/

chompheader(line, def)
	char *line;
	bool def;
{
	register char *p;
	register HDR *h;
	HDR **hp;
	char *fname;
	char *fvalue;
	struct hdrinfo *hi;
	u_long mopts;
	extern u_long mfencode();
	extern char *crackaddr();
	extern ADDRESS *sendto();

# ifdef DEBUG
	if (tTd(31, 6))
		printf("chompheader: %s\n", line);
# endif DEBUG

	/* strip off options */
	mopts = 0;
	p = line;
	if (*p == '?')
	{
		/* have some */
		register char *q = index(p + 1, *p);
		
		if (q != NULL)
		{
			*q++ = '\0';
			mopts = mfencode(p + 1);
			p = q;
		}
		else
			syserr("chompheader: syntax error, line \"%s\"", line);
	}

	/* find canonical name */
	fname = p;
	p = index(p, ':');
	fvalue = &p[1];
	while (isspace(*--p))
		continue;
	*++p = '\0';
	makelower(fname);

	/* strip field value on front */
	if (*fvalue == ' ')
		fvalue++;

	/* search header list for this header */
	for (hp = &CurEnv->e_header, h = CurEnv->e_header; h != NULL; hp = &h->h_link, h = h->h_link)
	{
		if (strcmp(fname, h->h_field) == 0 && bitset(H_DEFAULT, h->h_flags))
			break;
	}

	/* see if it is a known type */
	for (hi = HdrInfo; hi->hi_field != NULL; hi++)
	{
		if (strcmp(hi->hi_field, fname) == 0)
			break;
	}

	/* if this means "end of header" quit now */
	if (bitset(H_EOH, hi->hi_flags))
		return (hi->hi_flags);

	/* count Received: lines to avoid loops (simulate hop counts) */
	if (bitset(H_TRACE, hi->hi_flags))
		HopCount++;

	/* create/fill in a new node */
	if (h == NULL || bitset(H_FORCE, h->h_flags))
	{
		/* create a new node */
		h = (HDR *) xalloc(sizeof *h);
		h->h_field = newstr(fname);
		h->h_value = NULL;
		h->h_link = *hp;
		h->h_mflags = mopts | hi->hi_mflags;
		*hp = h;
	}
	h->h_flags = hi->hi_flags;
	if (def)
		h->h_flags |= H_DEFAULT;
	else if (mopts == 0)
		h->h_flags &= ~H_CHECK;
	if (h->h_value != NULL)
		free(h->h_value);
		(void) sendto(h->h_value, 0, (ADDRESS *) NULL, 0);

	/* hack to see if this is a new format message */
	if (bitset(H_RCPT, h->h_flags) &&
	    (index(fvalue, ',') != NULL || index(fvalue, '(') != NULL ||
	     index(fvalue, '<') != NULL) || index(fvalue, ';') != NULL)
		CurEnv->e_oldstyle = FALSE;

	/* send to this person if we so desire */
	if (!def && GrabTo && bitset(H_RCPT, h->h_flags))
		sendto(h->h_value, 0, (ADDRESS *) NULL, &CurEnv->e_sendqueue);

	return (h->h_flags);
}
/*
**  ADDHEADER -- add a header entry to the end of the queue.
**
**	This bypasses the special checking of chompheader.
**
**	Parameters:
**		field -- the name of the header field.
**		value -- the value of the field.  It must be lower-cased.
**		e -- the envelope to add them to.
**
**	Returns:
**		none.
**
**	Side Effects:
**		adds the field on the list of headers for this envelope.
*/

addheader(field, value, e)
	char *field;
	char *value;
	ENVELOPE *e;
{
	register HDR *h;
	register struct hdrinfo *hi;
	HDR **hp;

	/* find info struct */
	for (hi = HdrInfo; hi->hi_field != NULL; hi++)
	{
		if (strcmp(field, hi->hi_field) == 0)
			break;
	}

	/* find current place in list -- keep back pointer? */
	for (hp = &e->e_header; (h = *hp) != NULL; hp = &h->h_link)
	{
		if (strcmp(field, h->h_field) == 0)
			break;
	}

	/* allocate space for new header */
	h = (HDR *) xalloc(sizeof *h);
	h->h_field = field;
	h->h_value = newstr(value);
	h->h_link = *hp;
	h->h_flags = hi->hi_flags | H_DEFAULT;
	h->h_mflags = hi->hi_mflags;
	*hp = h;
}
/*
**  HVALUE -- return value of a header.
**
**	Only "real" fields (i.e., ones that have not been supplied
**	as a default) are used.
**
**	Parameters:
**		field -- the field name.
**
**	Returns:
**		pointer to the value part.
**		NULL if not found.
**
**	Side Effects:
**		sets the H_USED bit in the header if found.
*/

char *
hvalue(field)
	char *field;
{
	register HDR *h;

	for (h = CurEnv->e_header; h != NULL; h = h->h_link)
	{
		if (!bitset(H_DEFAULT, h->h_flags) && strcmp(h->h_field, field) == 0)
		{
			h->h_flags |= H_USED;
			return (h->h_value);
		}
	}
	return (NULL);
}
/*
**  HRVALUE -- return pointer to header descriptor.
**
**	Like hvalue except returns header descriptor block and isn't
**	picky about "real" headers.
**
**	Parameters:
**		field -- name of field we are interested in.
**
**	Returns:
**		pointer to header descriptor.
**
**	Side Effects:
**		none.
*/

HDR *
hrvalue(field)
	char *field;
{
	register HDR *h;

	for (h = CurEnv->e_header; h != NULL; h = h->h_link)
	{
		if (strcmp(h->h_field, field) == 0)
			return (h);
	}
	return (NULL);
}
/*
**  ISHEADER -- predicate telling if argument is a header.
**
**	A line is a header if it has a single word followed by
**	optional white space followed by a colon.
**
**	Parameters:
**		s -- string to check for possible headerness.
**
**	Returns:
**		TRUE if s is a header.
**		FALSE otherwise.
**
**	Side Effects:
**		none.
*/

bool
isheader(s)
	register char *s;
{
	if (!isalnum(*s))
		return (FALSE);
	while (!isspace(*s) && *s != ':' && *s != '\0')
		s++;
	while (isspace(*s))
		s++;
	return (*s == ':');
}
/*
**  GETXPART -- extract the "signature" part of an address line.
**
**	Try to extract the full name from a general address
**	field.  We take anything which is a comment as a
**	first choice.  Failing in that, we see if there is
**	a "machine readable" name (in <angle brackets>); if
**	so we take anything preceeding that clause.
**
**	If we blow it here it's not all that serious.
**
**	Parameters:
**		p -- line to crack.
**
**	Returns:
**		signature part.
**		NULL if no signature part.
**
**	Side Effects:
**		none.
*/

char *
getxpart(p)
	register char *p;
{
	register char *q;
	register char *rval = NULL;

	q = index(p, '(');
	if (q != NULL)
	{
		int parenlev = 0;

		for (p = q; *p != '\0'; p++)
		{
			if (*p == '(')
				parenlev++;
			else if (*p == ')' && --parenlev <= 0)
				break;
		}
		if (*p == ')')
		{
			*p = '\0';
			if (*++q != '\0')
				rval = newstr(q);
			*p = ')';
		}
	}
	else if ((q = index(p, '<')) != NULL)
	{
		char savec;

		while (*--q == ' ')
			continue;
		while (isspace(*p))
			p++;
		savec = *++q;
		*q = '\0';
		if (*p != '\0')
			rval = newstr(p);
		*q = savec;
	}

	return (rval);
}
/*
**  EATHEADER -- run through the stored header and extract info.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Sets a bunch of global variables from information
**		in the collected header.
*/

eatheader()
{
	register HDR *h;
	register char *p;
	char buf[MAXLINE];

# ifdef DEBUG
	if (tTd(32, 2))
	{
		extern char *capitalize();

		printf("----- collected header -----\n");
		for (h = CurEnv->e_header; h != NULL; h = h->h_link)
			printf("%s: %s\n", capitalize(h->h_field), h->h_value);
		printf("----------------------------\n");
	}
# endif DEBUG

	/* message priority */
	if (!QueueRun)
	{
		/* adjust total priority by message priority */
		CurEnv->e_msgpriority = CurEnv->e_msgsize;
		p = hvalue("precedence");
		if (p != NULL)
			CurEnv->e_class = priencode(p);
		else
			CurEnv->e_class = PRI_NORMAL;
		CurEnv->e_msgpriority -= CurEnv->e_class * WKPRIFACT;
	}

	/* return receipt to */
	p = hvalue("return-receipt-to");
	if (p != NULL)
		CurEnv->e_receiptto = p;

	/* from person */
	if (ArpaMode)
	{
		register struct hdrinfo *hi = HdrInfo;

		for (p = NULL; p == NULL && hi->hi_field != NULL; hi++)
		{
			if (bitset(H_FROM, hi->hi_flags))
				p = hvalue(hi->hi_field);
		}
		if (p != NULL)
			setfrom(p, (char *) NULL);
	}

	/* full name of from person */
	p = hvalue("full-name");
	if (p != NULL)
		define('x', p);

	/* date message originated */
	p = hvalue("posted-date");
	if (p == NULL)
		p = hvalue("date");
	if (p != NULL)
	{
		define('a', p);
		/* we don't have a good way to do canonical conversion ....
		define('d', newstr(arpatounix(p)));
		.... so we will ignore the problem for the time being */
	}
}
/*
**  PRIENCODE -- encode external priority names into internal values.
**
**	Parameters:
**		p -- priority in ascii.
**
**	Returns:
**		priority as a numeric level.
**
**	Side Effects:
**		none.
*/

struct prio
{
	char	*pri_name;	/* external name of priority */
	int	pri_val;	/* internal value for same */
};

static struct prio	Prio[] =
{
	"alert",		PRI_ALERT,
	"quick",		PRI_QUICK,
	"first-class",		PRI_FIRSTCL,
	"normal",		PRI_NORMAL,
	"second-class",		PRI_SECONDCL,
	"third-class",		PRI_THIRDCL,
	"junk",			PRI_JUNK,
	NULL,			PRI_NORMAL,
};

priencode(p)
	char *p;
{
	register struct prio *pl;
	extern bool sameword();

	for (pl = Prio; pl->pri_name != NULL; pl++)
	{
		if (sameword(p, pl->pri_name))
			break;
	}
	return (pl->pri_val);
}
/*
**  CRACKADDR -- parse an address and turn it into a macro
**
**	This doesn't actually parse the address -- it just extracts
**	it and replaces it with "$g".  The parse is totally ad hoc
**	and isn't even guaranteed to leave something syntactically
**	identical to what it started with.  However, it does leave
**	something semantically identical.
**
**	The process is kind of strange.  There are a number of
**	interesting cases:
**		1.  comment <address> comment	==> comment <$g> comment
**		2.  address			==> address
**		3.  address (comment)		==> $g (comment)
**		4.  (comment) address		==> (comment) $g
**	And then there are the hard cases....
**		5.  add (comment) ress		==> $g (comment)
**		6.  comment <address (comment)>	==> comment <$g (comment)>
**		7.    .... etc ....
**
**	Parameters:
**		addr -- the address to be cracked.
**
**	Returns:
**		a pointer to the new version.
**
**	Side Effects:
**		none.
**
**	Warning:
**		The return value is saved in local storage and should
**		be copied if it is to be reused.
*/

char *
crackaddr(addr)
	register char *addr;
{
	register char *p;
	register int i;
	static char buf[MAXNAME];
	char *rhs;
	bool gotaddr;
	register char *bp;

# ifdef DEBUG
	if (tTd(33, 1))
		printf("crackaddr(%s)\n", addr);
# endif DEBUG

	strcpy(buf, "");
	rhs = NULL;

	/*
	**  See if we have anything in angle brackets.  If so, that is
	**  the address part, and the rest is the comment.
	*/

	p = index(addr, '<');
	if (p != NULL)
	{
		/* copy the beginning of the addr field to the buffer */
		*p = '\0';
		strcpy(buf, addr);
		strcat(buf, "<");
		*p = '<';

		/* find the matching right angle bracket */
		addr = ++p;
		for (i = 0; *p != '\0'; p++)
		{
			switch (*p)
			{
			  case '<':
				i++;
				break;

			  case '>':
				i--;
				break;
			}
			if (i < 0)
				break;
		}

		/* p now points to the closing quote (or a null byte) */
		if (*p != '\0')
		{
			/* make rhs point to the extra stuff at the end */
			rhs = p;
			*p++ = '\0';
		}
	}

	/*
	**  Now parse the real address part.  "addr" points to the (null
	**  terminated) version of what we are inerested in; rhs points
	**  to the extra stuff at the end of the line, if any.
	*/

	p = addr;

	/* now strip out comments */
	bp = &buf[strlen(buf)];
	gotaddr = FALSE;
	for (; *p != '\0'; p++)
	{
		if (*p == '(')
		{
			/* copy to matching close paren */
			*bp++ = *p++;
			for (i = 0; *p != '\0'; p++)
			{
				*bp++ = *p;
				switch (*p)
				{
				  case '(':
					i++;
					break;

				  case ')':
					i--;
					break;
				}
				if (i < 0)
					break;
			}
			continue;
		}

		/*
		**  If this is the first "real" character we have seen,
		**  then we put the "$g" in the buffer now.
		*/

		if (isspace(*p))
			*bp++ = *p;
		else if (!gotaddr)
		{
			strcpy(bp, "$g");
			bp += 2;
			gotaddr = TRUE;
		}
	}

	/* hack, hack.... strip trailing blanks */
	do
	{
		*bp-- = '\0';
	} while (isspace(*bp));
	bp++;

	/* put any right hand side back on */
	if (rhs != NULL)
	{
		*rhs = '>';
		strcpy(bp, rhs);
	}

# ifdef DEBUG
	if (tTd(33, 1))
		printf("crackaddr=>`%s'\n", buf);
# endif DEBUG

	return (buf);
}
