# include <time.h>
# ifndef V6
# include <sys/types.h>
# include <sys/timeb.h>
# endif

static char SccsId[] = "@(#)arpadate.c	3.1	%G%";

/*
**  ARPADATE -- Create date in ARPANET format
**
**	Parameters:
**		ud -- unix style date string.  if NULL, one is created.
**
**	Returns:
**		pointer to an ARPANET date field
**
**	Side Effects:
**		none
**
**	WARNING:
**		date is stored in a local buffer -- subsequent
**		calls will overwrite.
*/

# ifdef V6
# define DST_NAME	"PDT"
# define STD_NAME	"PST"
# endif

# define NULL		0

char *
arpadate(ud)
	register char *ud;
{
	register char *p;
	register char *q;
	static char b[40];
	extern char *ctime();
	register int i;
	extern struct tm *localtime();
# ifdef V6
	long t;
# else
	struct timeb t;
	extern struct timeb *ftime();
	extern char *timezone();
# endif

# ifdef V6
	time(&t);
	if (ud == NULL)
		ud = ctime(&t);
# else
	ftime(&t);
	if (ud == NULL)
		ud = ctime(&t.time);
# endif

	q = b;

	p = &ud[0];		/* Mon */
	*q++ = *p++;
	*q++ = *p++;
	*q++ = *p++;
	*q++ = ',';
	*q++ = ' ';

	p = &ud[8];		/* 16 */
	if (*p == ' ')
		p++;
	else
		*q++ = *p++;
	*q++ = *p++;
	*q++ = '-';

	p = &ud[4];		/* Sep */
	*q++ = *p++;
	*q++ = *p++;
	*q++ = *p++;
	*q++ = '-';

	p = &ud[20];		/* 1979 */
	for (i = 4; i > 0; i--)
		*q++ = *p++;
	*q++ = ' ';

	p = &ud[11];		/* 01:03:52 */
	for (i = 8; i > 0; i--)
		*q++ = *p++;

				/* -PST or -PDT */
# ifdef V6
	if (localtime(&t)->tm_isdst)
		p = DST_NAME;
	else
		p = STD_NAME;
# else
	p = timezone(t.timezone, localtime(&t.time)->tm_isdst);
# endif V6
	if (p[3] != '\0')
	{
		/* hours from GMT */
		p += 3;
		*q++ = *p++;
		if (p[1] == ':')
			*q++ = '0';
		else
			*q++ = *p++;
		*q++ = *p++;
		p++;		/* skip ``:'' */
		*q++ = *p++;
		*q++ = *p++;
	}
	else
	{
		*q++ = '-';
		*q++ = *p++;
		*q++ = *p++;
		*q++ = *p++;
	}

	*q = '\0';

	return (b);
}
