/* $Header$ */

#include <ctype.h>
#include "date.h"
#include "null.h"
#include "yesno.h"
/*
 * date template character codes
 */
#define	L	1			/* a lower case chararacter */
#define U	2			/* an upper case character */
#define	S	3			/* a space */
#define	D	4			/* a digit */
#define	O	5			/* an optional digit or space */
#define	C	6			/* a colon */
#define	N	7			/* a newline */
/*
 * date templates
 */
static char ctypes[] =
	{U,L,L,S,U,L,L,S,O,D,S,D,D,C,D,D,C,D,D,S,D,D,D,D,0};
static char tmztypes[] =
	{U,L,L,S,U,L,L,S,O,D,S,D,D,C,D,D,C,D,D,S,U,U,U,S,D,D,D,D,0};
static char *mon_tab[] =
	{
	"illegal month",
	"Jan",
	"Feb",
	"Mar",
	"Apr",
	"May",
	"Jun",
	"Jul",
	"Aug",
	"Sep",
	"Oct",
	"Nov",
	"Dec"
	};
/*
 * cmatch() matchs a date against a given template. Returns constant 1 if
 * there is a match, otherwise 0.
 */
static
cmatch(date, template)
	register char *date;		/* date string */
	register char *template;	/* template array */
{
	register int c;			/* comparison character */

	for (c = *date++; c != '\0' && *template != 0; c= *date++)
		switch (*template++)
			{
		case L:
			if (!islower(c))
				return(0);
			break;
		case U:
			if (!isupper(c))
				return(0);
			break;
		case S:
			if (c != ' ')
				return(0);
			break;
		case D:
			if (!isdigit(c))
				return(0);
			break;
		case O:
			if (c != ' ' && !isdigit(c))
				return(0);
			break;
		case C:
			if (c != ':')
				return(0);
			break;
		case N:
			if (c != '\n')
				return(0);
			break;
		}
	if (c != '\0' || *template != 0)
		return(0);
	return(1);
}



/*
 * isdate() tests if date is a ctime(3) generated date string.
 * The ctypes template is used as the criterion of correctness. Also
 * a possible trailing timezone is checked by the tmztype template.
 * Returns constant 1 if valid, otherwise 0.
 */
isdate(date)
	char *date;			/* date string */
{
	int cmatch();			/* match date against template */

	if (cmatch(date, ctypes) == 1)
		return(1);
	return(cmatch(date, tmztypes));
}



/*
 * parsedate() breaks down the time returned by ctime(3) into a DATE
 * struct. Returns constant NO if a bad date, otherwise YES.
 */
parsedate(cdate, dt)
	char *cdate;			/* ctime(3) generated date string */
	DATE *dt;			/* pointer to broken-down time struct */
{
	int atoi();			/* convert alpha string to integer */
	int cmatch();			/* match date against template */
	int strncmp();			/* compare n characters in string */

	if (cmatch(cdate, ctypes) == 1)
		{
		dt->t_year = atoi(cdate+20);
		}
	else if (cmatch(cdate, tmztypes) == 1)
		{
		dt->t_year = atoi(cdate+24);
		}
	else	{
		return(NO);
		}
	for (dt->t_mon = 12; dt->t_mon > 0; dt->t_mon--)
		if (strncmp(mon_tab[dt->t_mon], cdate+4, 3) == 0)
			break;
	if (dt->t_mon <= 0)
		return(NO);
	dt->t_day = atoi(cdate+8);
	dt->t_hour = atoi(cdate+11);
	dt->t_min = atoi(cdate+14);
	dt->t_sec = atoi(cdate+17);
	return(YES);
}
