# include "sendmail.h"
# include "conf.h"

SCCSID(@(#)macro.c	3.11.1.1		%G%);

/*
**  EXPAND -- macro expand a string using $x escapes.
**
**	Parameters:
**		s -- the string to expand.
**		buf -- the place to put the expansion.
**		buflim -- the buffer limit, i.e., the address
**			of the last usable position in buf.
**		e -- envelope in which to work.
**
**	Returns:
**		End of interpolated output.
**
**	Side Effects:
**		none.
*/

expand(s, buf, buflim, e)
	register char *s;
	register char *buf;
	char *buflim;
	register ENVELOPE *e;
{
	extern char *expand2();

	(void) expand2(s, buf, buflim, e);
}


char *
expand2(s, buf, buflim, e)
	register char *s;
	register char *buf;
	char *buflim;
	register ENVELOPE *e;
{
	register char *q;
	char xbuf[BUFSIZ];
	register char *xp = xbuf;
	bool skipping;		/* set if conditionally skipping output */
	bool gotone = FALSE;	/* set if any expansion done */

# ifdef DEBUG
	if (Debug > 3)
	{
		printf("expand(");
		xputs(s);
		printf(")\n");
	}
# endif DEBUG

	skipping = FALSE;
	for (; *s != '\0'; s++)
	{
		char c;

		/*
		**  Check for non-ordinary (special?) character.
		**	'q' will be the interpolated quantity.
		*/

		q = NULL;
		c = *s;
		switch (c)
		{
		  case CONDIF:		/* see if var set */
			c = *++s;
			skipping = e->e_macro[c] == NULL;
			continue;

		  case CONDELSE:	/* change state of skipping */
			skipping = !skipping;
			continue;

		  case CONDFI:		/* stop skipping */
			skipping = FALSE;
			continue;

		  case '$':		/* macro interpolation */
			c = *++s;
			q = e->e_macro[c & 0177];
			if (q == NULL && c != '$')
				continue;
			gotone = TRUE;
			break;
		}

		/*
		**  Interpolate q or output one character
		*/

		if (skipping)
			continue;
		while (xp < &xbuf[sizeof xbuf])
		{
			if (q == NULL)
			{
				*xp++ = c;
				break;
			}
			if (*q == NULL)
				break;
			*xp++ = *q++;
		}
	}
	*xp = '\0';

# ifdef DEBUG
	if (Debug > 3)
	{
		printf("expand ==> '");
		xputs(xbuf);
		printf("'\n");
	}
# endif DEBUG

	/* recurse as appropriate */
	if (gotone)
		return (expand2(xbuf, buf, buflim, e));

	/* copy results out */
	for (q = buf, xp = xbuf; xp != '\0' && q < buflim-1; )
		*q++ = *xp++;
	*q = '\0';

	return (q);
}
/*
**  DEFINE -- define a macro.
**
**	this would be better done using a #define macro.
**
**	Parameters:
**		n -- the macro name.
**		v -- the macro value.
**
**	Returns:
**		none.
**
**	Side Effects:
**		CurEnv->e_macro[n] is defined.
**
**	Notes:
**		There is one macro for each ASCII character,
**		although they are not all used.  The currently
**		defined macros are:
**
**		$a   date in ARPANET format (preferring the Date: line
**		     of the message)
**		$b   the current date (as opposed to the date as found
**		     the message) in ARPANET format
**		$c   hop count
**		$d   (current) date in UNIX (ctime) format
**		$f   raw from address
**		$g   translated from address
**		$h   to host
**		$i   official SMTP hostname, used in messages+
**		$l   UNIX-style from line+
**		$n   name of sendmail ("MAILER-DAEMON" on local
**		     net typically)+
**		$o   delimiters ("operators") for address tokens+
**		$p   my process id in decimal
**		$q   the string that becomes an address -- this is
**		     normally used to combine $g & $x.
**		$r   protocol used to talk to sender
**		$s   sender's host name
**		$t   the current time in seconds since 1/1/1970
**		$u   to user
**		$v   version number of sendmail
**		$x   signature (full name) of from person
**		$y   the tty id of our terminal
**		$z   home directory of to person
**
**		Macros marked with + must be defined in the
**		configuration file and are used internally, but
**		are not set.
**
**		There are also some macros that can be used
**		arbitrarily to make the configuration file
**		cleaner.  In general all upper-case letters
**		are available.
*/

define(n, v)
	char n;
	char *v;
{
# ifdef DEBUG
	if (Debug > 3)
	{
		printf("define(%c as ", n);
		xputs(v);
		printf(")\n");
	}
# endif DEBUG
	CurEnv->e_macro[n & 0177] = v;
}
/*
**  MACVALUE -- return uninterpreted value of a macro.
**
**	Parameters:
**		n -- the name of the macro.
**
**	Returns:
**		The value of n.
**
**	Side Effects:
**		none.
*/

char *
macvalue(n)
	char n;
{
	return (CurEnv->e_macro[n & 0177]);
}
