# include <errno.h>
# include "sendmail.h"

SCCSID(@(#)headers.c	3.18		%G%);

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
	extern bool isheader();
	char *fname;
	char *fvalue;
	struct hdrinfo *hi;
	u_long mopts;
	extern u_long mfencode();
	extern ADDRESS *sendto();

	/* strip off trailing newline */
	p = rindex(line, '\n');
	if (p != NULL)
		*p = '\0';

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

	/* hack, hack -- save From: line specially */
	if (!def && !QueueRun && strcmp(fname, "from") == 0)
	{
		CurEnv->e_origfrom = newstr(fvalue);
		return (0);
	}

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

	/* don't put timestamps in every queue run */
	if (QueueRun && h != NULL && bitset(H_FORCE, h->h_flags))
		return (h->h_flags);

	/* create/fill in a new node */
	if (h == NULL || bitset(H_FORCE, h->h_flags))
	{
		/* create a new node */
		h = (HDR *) xalloc(sizeof *h);
		h->h_field = newstr(fname);
		h->h_value = NULL;
		h->h_link = *hp;
		h->h_flags = hi->hi_flags;
		h->h_mflags = mopts | hi->hi_mflags;
		*hp = h;
	}
	if (def)
		h->h_flags |= H_DEFAULT;
	else if (mopts == 0)
		h->h_flags &= ~H_CHECK;
	if (h->h_value != NULL)
		free(h->h_value);
	h->h_value = newstr(fvalue);
		(void) sendto(h->h_value, 0, (ADDRESS *) NULL, 0);

	/* hack to see if this is a new format message */
	if (bitset(H_RCPT, h->h_flags) &&
	    (index(fvalue, ',') != NULL || index(fvalue, '(') != NULL ||
	     index(fvalue, '<') != NULL))
		CurEnv->e_oldstyle = FALSE;

	return (h->h_flags);
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
**
**	Bugs:
**		According to RFC733, there should be a newline
**		permitted after the word but before the colon.
**		We don't seem to support that.....
*/

bool
isheader(s)
	register char *s;
{
	if (!isalnum(*s))
		return (FALSE);
	while (!isspace(*s) && *s != ':')
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
