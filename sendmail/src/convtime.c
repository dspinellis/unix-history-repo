/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)convtime.c	5.3 (Berkeley) 6/30/88";
#endif /* not lint */

# include <ctype.h>
# include "useful.h"

/*
**  CONVTIME -- convert time
**
**	Takes a time as an ascii string with a trailing character
**	giving units:
**	  s -- seconds
**	  m -- minutes
**	  h -- hours
**	  d -- days (default)
**	  w -- weeks
**	For example, "3d12h" is three and a half days.
**
**	Parameters:
**		p -- pointer to ascii time.
**
**	Returns:
**		time in seconds.
**
**	Side Effects:
**		none.
*/

time_t
convtime(p)
	char *p;
{
	register time_t t, r;
	register char c;

	r = 0;
	while (*p != '\0')
	{
		t = 0;
		while (isdigit(c = *p++))
			t = t * 10 + (c - '0');
		if (c == '\0')
			p--;
		switch (c)
		{
		  case 'w':		/* weeks */
			t *= 7;

		  case 'd':		/* days */
		  default:
			t *= 24;

		  case 'h':		/* hours */
			t *= 60;

		  case 'm':		/* minutes */
			t *= 60;

		  case 's':		/* seconds */
			break;
		}
		r += t;
	}

	return (r);
}
/*
**  PINTVL -- produce printable version of a time interval
**
**	Parameters:
**		intvl -- the interval to be converted
**		brief -- if TRUE, print this in an extremely compact form
**			(basically used for logging).
**
**	Returns:
**		A pointer to a string version of intvl suitable for
**			printing or framing.
**
**	Side Effects:
**		none.
**
**	Warning:
**		The string returned is in a static buffer.
*/

# define PLURAL(n)	((n) == 1 ? "" : "s")

char *
pintvl(intvl, brief)
	time_t intvl;
	bool brief;
{
	static char buf[256];
	register char *p;
	int wk, dy, hr, mi, se;

	if (intvl == 0 && !brief)
		return ("zero seconds");

	/* decode the interval into weeks, days, hours, minutes, seconds */
	se = intvl % 60;
	intvl /= 60;
	mi = intvl % 60;
	intvl /= 60;
	hr = intvl % 24;
	intvl /= 24;
	if (brief)
		dy = intvl;
	else
	{
		dy = intvl % 7;
		intvl /= 7;
		wk = intvl;
	}

	/* now turn it into a sexy form */
	p = buf;
	if (brief)
	{
		if (dy > 0)
		{
			(void) sprintf(p, "%d+", dy);
			p += strlen(p);
		}
		(void) sprintf(p, "%02d:%02d:%02d", hr, mi, se);
		return (buf);
	}

	/* use the verbose form */
	if (wk > 0)
	{
		(void) sprintf(p, ", %d week%s", wk, PLURAL(wk));
		p += strlen(p);
	}
	if (dy > 0)
	{
		(void) sprintf(p, ", %d day%s", dy, PLURAL(dy));
		p += strlen(p);
	}
	if (hr > 0)
	{
		(void) sprintf(p, ", %d hour%s", hr, PLURAL(hr));
		p += strlen(p);
	}
	if (mi > 0)
	{
		(void) sprintf(p, ", %d minute%s", mi, PLURAL(mi));
		p += strlen(p);
	}
	if (se > 0)
	{
		(void) sprintf(p, ", %d second%s", se, PLURAL(se));
		p += strlen(p);
	}

	return (buf + 2);
}
