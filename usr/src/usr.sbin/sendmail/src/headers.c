# include <errno.h>
# include "sendmail.h"

static char	SccsId[] = "@(#)headers.c	3.3	%G%";

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

	/* strip off trailing newline */
	p = rindex(line, '\n');
	if (p != NULL)
		*p = '\0';

	/* find canonical name */
	fname = line;
	p = index(line, ':');
	fvalue = &p[1];
	while (isspace(*--p))
		continue;
	*++p = '\0';
	makelower(fname);

	/* strip field value on front */
	if (*fvalue == ' ')
		fvalue++;

	/* search header list for this header */
	for (hp = &Header, h = Header; h != NULL; hp = &h->h_link, h = h->h_link)
	{
		if (strcmp(fname, h->h_field) == 0 && bitset(H_DEFAULT, h->h_flags) &&
		    !bitset(H_FORCE, h->h_flags))
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

	/* create/fill in a new node */
	if (h == NULL)
	{
		/* create a new node */
		*hp = h = (HDR *) xalloc(sizeof *h);
		h->h_field = newstr(fname);
		h->h_value = NULL;
		h->h_link = NULL;
		h->h_flags = hi->hi_flags;
		h->h_mflags = hi->hi_mflags;
	}
	if (def)
		h->h_flags |= H_DEFAULT;
	else
		h->h_flags &= ~H_CHECK;
	if (h->h_value != NULL)
		free(h->h_value);
	h->h_value = newstr(fvalue);

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

	for (h = Header; h != NULL; h = h->h_link)
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
	while (!isspace(*s) && *s != ':')
		s++;
	while (isspace(*s))
		s++;
	return (*s == ':');
}
