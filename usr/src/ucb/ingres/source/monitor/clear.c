# include	"monitor.h"
# include	<ingres.h>
# include	<aux.h>
# include	<sccs.h>

SCCSID(@(#)clear.c	7.1	2/5/81)



/*
**  Clear query buffer
**	Flag f is set if called explicitly (with \q) and is
**	clear if called automatically.
**
**	Uses trace flag 3
*/

clear(f)
{
	Autoclear = 0;

	/* TRUNCATE FILE & RETURN */
	if (freopen(Qbname, "w", Qryiop) == NULL)
		syserr("clear: open");
	if (Nodayfile >= 0 && f)
		printf("\07go\n");
	if (f)
		clrline(0);
	Notnull = 0;
}
