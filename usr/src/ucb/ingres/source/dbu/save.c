# include	<pv.h>
# include	<ingres.h>
# include	<aux.h>
# include 	<func.h>
# include	<opsys.h>
# ifdef xV7_UNIX
# include	<sys/timeb.h>
# endif xV7_UNIX
# include	<sccs.h>

SCCSID(@(#)save.c	7.1	2/5/81)

extern	short	tTdbu[];
extern	int	save();
extern	int	null_fn();

struct fn_def SaveFn =
{
	"SAVE",
	save,
	null_fn,		/* initialization function */
	null_fn,
	NULL,
	0,
	tTdbu,
	100,
	'Z',
	0
};
/*
**  SAVE RELATION UNTIL DATE
**
**	This function arranges to save a named relation until a
**	specified date.
**
**	Parameters:		(pv_type is PV_STR for all of them)
**	0 -- relation name
**	1 -- month (1 -> 12 or "jan" -> "dec" or a variety of other codes)
**	2 -- day (1 -> 31)
**	3 -- year (1970 -> ?)
**
**	Uses trace flag 44
**	Uses error messages 56xx
*/

int	DmSize[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

save(parmc, parmv)
int	parmc;
PARM	parmv[];
{
	long		date;
	register int	i;
	extern DESC	Reldes;
	TID		tid;
	extern char	*Usercode;
	struct relation	relk, relt;
	int		day, month, year;
# ifdef xV7_UNIX
	struct timeb	timeb;
# else xV7_UNIX
	extern int	timezone;	/* defined by ctime(3) */
# endif xV7_UNIX
	extern int	dysize();	/* ditto */

	/*
	**  Validate parameters.
	**
	**	According to my pocket calculator, a 31 bit number will
	**	hold 70 years of accuracy -- hence the 2035 cutoff.  If
	**	this code is still around in 2035, I apologize in
	**	advance.
	*/

	if (atoi(parmv[3].pv_val.pv_str, &year) || year < 1970 || year > 2035)
		return (error(5603, parmv[3].pv_val.pv_str, 0));	/* bad year */
	if (monthcheck(parmv[1].pv_val.pv_str, &month))
		return (error(5601, parmv[1].pv_val.pv_str, 0));	/* bad month */
	if (atoi(parmv[2].pv_val.pv_str, &day) || day < 1 || day > monthsize(--month, year))
		return (error(5602, parmv[2].pv_val.pv_str, 0));	/* bad day */

	/* convert date */
	/* "date" will be # of days from 1970 for a while */
	date = 0;

	/* do year conversion */
	for (i = 1970; i < year; i++)
	{
		date += dysize(i);
	}

	/* do month conversion */
	for (i = 0; i < month; i++)
		date += DmSize[i];
	/* once again, allow for leapyears */
	if (month >= 2 && year % 4 == 0 && year % 100 != 0)
		date += 1;

	/* do day conversion */
	date += day - 1;

	/* we now convert date to be the # of hours since 1970 */
	date *= 24;

	/* do daylight savings computations */
	/*  <<<<< none now >>>>> */

	/* convert to seconds */
	date *= 60 * 60;

	/* adjust to local time */
# ifdef xV7_UNIX
	ftime(&timeb);
	date += ((long) timeb.timezone) * 60;
# else xV7_UNIX
	date += timezone;
# endif xV7_UNIX

#	ifdef xZTR1
	if (tTf(45, 1))
		printf("%s", ctime(&date));
#	endif

	/* let's check and see if the relation exists */
	opencatalog("relation", 2);
	clearkeys(&Reldes);
	setkey(&Reldes, &relk, parmv[0].pv_val.pv_str, RELID);
	setkey(&Reldes, &relk, Usercode, RELOWNER);
	if (getequal(&Reldes, &relk, &relt, &tid))
	{
		return (error(5604, parmv[0].pv_val.pv_str, 0));	/* relation not found */
	}

	/* check that it is not a system catalog */
	if (relt.relstat & S_CATALOG)
		return (error(5600, parmv[0].pv_val.pv_str, 0));	/* cannot save sys rel */
	/* got it; lets change the date */
	relt.relsave = date;

#	ifdef xZTR2
	if (tTf(45, 2))
	{
		printup(&Reldes, &relt);
	}
#	endif

	if ((i = replace(&Reldes, &tid, &relt, 0)) < 0)
		syserr("SAVE: replace %d", i);

	/* that's all folks.... */
	return (0);
}
/*
**  MONTHCHECK
*/

struct monthtab
{
	char	*code;
	int	month;
};

struct monthtab	Monthtab[] =
{
	"jan",		1,
	"feb",		2,
	"mar",		3,
	"apr",		4,
	"may",		5,
	"jun",		6,
	"jul",		7,
	"aug",		8,
	"sep",		9,
	"oct",		10,
	"nov",		11,
	"dec",		12,
	"january",	1,
	"february",	2,
	"march",	3,
	"april",	4,
	"june",		6,
	"july",		7,
	"august",	8,
	"september",	9,
	"october",	10,
	"november",	11,
	"december",	12,
	0
};

monthcheck(input, output)
char	*input;
int	*output;
{
	register struct monthtab	*p;
	int				month;

	/* month can be an integer, or an alphanumeric code */
	if (atoi(input, &month) == 0)
	{
		*output = month;
		return (month < 1 || month > 12);
	}
	for (p = Monthtab; p->code; p++)
	{
		if (sequal(input, p->code))
		{
			*output = p->month;
			return (0);
		}
	}
	return (1);
}
/*
**  MONTHSIZE -- determine size of a particular month
*/

monthsize(month, year)
int	month;
int	year;
{
	register int	size;
	extern int	dysize();	/* defined in ctime */

	size = DmSize[month];
	if (month == 1 && dysize(year) == 366)
		/* This is February of a leap year */
		size++;

	return (size);

}
