# include <time.h>

/*
**  ARPADATE -- Create date in ARPANET format
**
**	Parameters:
**		none
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
**
**	Requires:
**		time (sys)
**		ctime (sys)
**		strcpy (sys)
**		strcat (sys)
**		localtime (sys)
**
**	History:
**		1/15/80 -- written.
*/

arpadate()
{
	register char *ud;	/* the unix date */
	long t;
	extern struct tm *localtime();
	register char *p;
	static char b[40];

	time(&t);
	ud = ctime(&t);

	ud[3] = ud[7] = ud[10] = ud[19] = ud[24] = '\0';
	p = &ud[8];		/* 16 */
	if (*p == ' ')
		p++;
	strcpy(b, p);
	strcat(b, " ");
	strcat(b, &ud[4]);	/* Sep */
	strcat(b, " ");
	strcat(b, &ud[20]);	/* 1979 */
	strcat(b, " ");
	strcat(b, &ud[11]);	/* 01:03:52 */
	if (localtime(&t)->tm_isdst)
		strcat(b, "-PDT");
	else
		strcat(b, "-PST");
	return (b);
}
