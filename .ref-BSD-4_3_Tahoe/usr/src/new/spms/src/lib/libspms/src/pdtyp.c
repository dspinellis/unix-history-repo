/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include <ctype.h>
#include "null.h"
#include "path.h"
#include "yesno.h"

/*
 * ispdt() returns 1 if project directory type label is legal,
 * otherwise zero.
 */
ispdt(type)
	char *type;			/* type label */
{
	register char *tp;		/* type label pointer */
	register int body = NO;		/* does type label have a body? */

	for (tp = type; *tp != '\0' && *tp != '.'; tp++)
		switch (*tp)
			{
			case ' ':
			case '\t':
				continue;
			default:
				body = YES;
				break;
			}
	if (body == NO)
		return(0);
	if (*tp == '.')
		{
		tp++;
		if (*tp == '-' || *tp == '+')
			tp++;
		while (isdigit(*tp))
			tp++;
		}
	return((*tp == '\0') ? 1 : 0);
}



/*
 * pdtcpy() copies a project directory type pointed to by tb, to type.
 * Returns type.
 */
char *
pdtcpy(type, tb)
	register char *type;		/* receiving type buffer */
	register char *tb;		/* type pointer */
{
	register int i;			/* type buffer index */

	for (i = 0; *tb != _PDTSC && *tb != '\0'; i++)
		type[i] = *tb++;
	type[i] = '\0';
	return(type);
}



/*
 * pdtfind() compares project directory type label type with the type
 * labels stored in type label buffer tb and returns a pointer to the
 * position of the type label in the buffer if found, otherwise NULL.
 */
char *
pdtfind(type, tb)
	char *type;			/* type label */
	register char *tb;		/* type label buffer */
{
	char *tskip();			/* skip to next type label */
	int tcmp();			/* compare type labels */

	for (; *tb != '\0'; tb = tskip(tb))
		{
		if (tcmp(type, tb) == 0)
			return(tb);
		}
	return(NULL);
}



/*
 * pdtinsert() inserts a type in type label buffer tb. The type labels
 * within tb are assumed to be sorted into ascending order. tb must
 * be big enough to hold type.
 */
void
pdtinsert(type, tb)
	register char *type;		/* type label */
	char *tb;			/* type label buffer */
{
	register char *tp;		/* type label buffer pointer */
	char *tskip();			/* skip to next type label */
	char *strcpy();			/* string copy */
	int strlen();			/* string length */
	int typlen;			/* type label length */
	int tcmp();			/* compare type labels */
	void tstretch();		/* stretch type label buffer */
	
	for (tp = tb; *tp != '\0'; tp = tskip(tp))
		if (tcmp(type, tp) < 0)
			break;
	typlen = strlen(type);
	if (*tp == '\0')		/* append to the buffer */
		if (tp > tb)
			{
			tstretch(tp, typlen+1);
			*tp = _PDTSC;
			strcpy(tp+1, type);
			}
		else	{		/* empty buffer */
			tstretch(tp, typlen);
			strcpy(tp, type);
			}
	else	{			/* insert in the buffer */
		tstretch(tp, typlen+1);
		strcpy(tp, type);
		tp[typlen] = _PDTSC;
		}
}



/*
 * pdtrm() removes type label type from type label buffer tb.
 */
void
pdtrm(type, tb)
	char *type;			/* type label to be removed */
	register char *tb;		/* type label buffer */
{
	register char *tp;		/* type label buffer pointer */
	char *tskip();			/* skip to next type label */
	int tcmp();			/* compare type labels */
	int tlen();			/* length of type label */
	int typlen;			/* type label length */
	void tstretch();		/* stretch type label buffer */
	
	for (tp = tb; *tp != '\0'; tp = tskip(tp))
		if (tcmp(type, tp) == 0)
			{
			typlen = tlen(tp);
			if (tp[typlen] == _PDTSC)
				tstretch(tp, -(typlen+1));
			else
				tstretch(tp, -typlen);
			if (tp > tb && tp[0] == '\0' && tp[-1] == _PDTSC)
				tp[-1] = '\0';
			break;
			}
}



/*
 * tcmp() compares project directory type labels and returns an integer
 * greater than, equal to, or less than 0, depending on whether type is
 * lexicographically greater than, equal to, or less than the type
 * pointed to by tp.
 */
static int
tcmp(type, tp)
	register char *type;		/* type label to be compared */
	register char *tp;		/* type pointer */
{
	for (; *tp == *type && *type != '\0'; tp++, type++)
		continue;
	if (*type == '\0' && (*tp == _PDTSC || *tp == '.' || *tp == '\0'))
		return(0);
	return(*type - *tp);
}



/*
 * tlen() returns the length of the type label pointed to by type label
 * buffer pointer tp.
 */
static int
tlen(tp)
	register char *tp;		/* type label buffer pointer */
{
	register int n;			/* length counter */

	for (n = 0; *tp != _PDTSC && *tp != '\0'; tp++, n++)
		continue;
	return(n);
}



/*
 * tskip() advances type pointer, tp, to the next project directory type
 * label.
 */
static char *
tskip(tp)
	register char *tp;		/* type label pointer */
{
	while (*tp != _PDTSC && *tp != '\0')
		tp++;
	if (*tp == _PDTSC)
		tp++;
	return(tp);
}



/*
 * tstretch() stretches type label buffer by n characters just before the point
 * marked by type pointer tp. Negative n shrinks buffer by n characters.
 */
static void
tstretch(tp, n)
	register char *tp;		/* type label buffer pointer */
	int n;				/* stretch amount */
{
	register char *lowertp;		/* lower roving type label buffer ptr */
	register char *uppertp;		/* upper roving type label buffer ptr */
	
	if (n > 0)
		{
		for (lowertp = tp; *lowertp != '\0'; lowertp++)
			continue;
		uppertp = lowertp + n;
		while (lowertp >= tp)
			*uppertp-- = *lowertp--;
		}
	else if (n < 0)
		{
		for (uppertp = tp; *uppertp != '\0'; uppertp++)
			continue;
		lowertp = tp - n;
		if (lowertp >= uppertp)
			*tp = '\0';
		while (lowertp <= uppertp)
			*tp++ = *lowertp++;
		}
}
