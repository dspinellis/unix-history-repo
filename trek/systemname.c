# include	"trek.h"

/*
**  RETRIEVE THE STARSYSTEM NAME
**
**	Very straightforward, this routine just gets the starsystem
**	name.  It returns zero if none in the specified quadrant
**	(which, by the way, is passed it).
**
**	This routine knows all about such things as distressed
**	starsystems, etc.
*/

char *systemname(q1)
struct quad	*q1;
{
	register struct quad	*q;
	register int		i;

	q = q1;

	i = q->systemname;
	if (i & Q_DISTRESSED)
		i = Event[i & Q_SYSTEM].systemname;

	i =& Q_SYSTEM;
	if (i == 0)
		return (0);
	return (Systemname[i]);
}
