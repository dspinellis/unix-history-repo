# include "useful.h"
# include "conf.h"

SCCSID(@(#)macro.c	3.9		%G%);

char	*Macro[128];
extern int	Debug;

/*
**  EXPAND -- macro expand a string using $x escapes.
**
**	Parameters:
**		s -- the string to expand.
**		buf -- the place to put the expansion.
**		buflim -- the buffer limit, i.e., the address
**			of the last usable position in buf.
**
**	Returns:
**		End of interpolated output.
**
**	Side Effects:
**		none.
*/

char *
expand(s, buf, buflim)
	register char *s;
	register char *buf;
	char *buflim;
{
	register char *bp;
	bool skipping;		/* set if conditionally skipping output */

# ifdef DEBUG
	if (Debug > 3)
		printf("expand(%s)\n", s);
# endif DEBUG

	skipping = FALSE;
	for (bp = buf; *s != '\0'; s++)
	{
		register char *q;

		/*
		**  Check for non-ordinary (special?) character --
		**  always escaped with dollar sign.
		**	'q' will be the interpolated quantity.
		*/

		q = NULL;
		if (*s == '$')
		{
			char c;

			c = *++s;
			switch (c)
			{
			  case '?':	/* see if var set */
				c = *++s;
				skipping = Macro[c] == NULL;
				break;

			  case '|':	/* else */
				skipping = !skipping;
				break;

			  case '.':	/* end if */
				skipping = FALSE;
				break;

			  default:
				q = Macro[c & 0177];
				break;
			}
			if (q == NULL && c != '$')
				continue;
		}

		/*
		**  Interpolate q or output one character
		*/

		if (skipping)
			continue;
		if (q != NULL)
			bp = expand(q, bp, buflim);
		else if (bp < buflim - 1)
			*bp++ = *s;
	}
	*bp = '\0';

# ifdef DEBUG
	if (Debug > 3)
		printf("expand ==> '%s'\n", buf);
# endif DEBUG

	return (bp);
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
**		Macro[n] is defined.
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
		printf("define(%c as %s)\n", n, v);
# endif DEBUG
	Macro[n & 0177] = v;
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
	return (Macro[n & 0177]);
}
