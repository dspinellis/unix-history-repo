# include <sys/types.h>
# include <ctype.h>

static char	SccsId[] =	"@(#)convtime.c	3.1	%G%";

/*
**  CONVTIME -- convert time
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
	register time_t t;

	t = 0;
	while (isdigit(*p))
		t = t * 10 + (*p++ - '0');
	switch (*p)
	{
	  case 'w':		/* weeks */
		t *= 7;

	  case 'd':		/* days */
	  case '\0':
	  default:
		t *= 24;

	  case 'h':		/* hours */
		t *= 60;

	  case 'm':		/* minutes */
		t *= 60;

	  case 's':		/* seconds */
		break;
	}

	return (t);
}
