# include <ctype.h>
# include "useful.h"

SCCSID(@(#)convtime.c	3.2		%G%);

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

	r = 0;
	while (*p != '\0')
	{
		t = 0;
		while (isdigit(*p))
			t = t * 10 + (*p++ - '0');
		switch (*p++)
		{
		  case 'w':		/* weeks */
			t *= 7;

		  case '\0':
			p--;
			/* fall through... */

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
