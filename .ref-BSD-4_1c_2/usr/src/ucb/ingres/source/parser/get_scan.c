# include	<ingres.h>
# include	"scanner.h"
# include	<sccs.h>

SCCSID(@(#)get_scan.c	7.1	2/5/81)

/*
** GET_SCAN -- gets characters from monitor
**
**	Parameters:
**		mode --
**	   	    modes are:
**			NORMAL = read normally
**			PRIME = prime the pipe
**			SYNC = sync (or flush) the pipe
**
**	Returns:
**		character or '\0' on eof
**
**	Trace Flags:
**		Getscan ~~ 54.0
*/

get_scan(mode)
int	mode;
{
	register int		ctr;
	char			c;

	extern int		Pctr;		/* vble for backup stack in scanner */
# ifdef	xPTR3
	tTfp(54, 0, "get_scan: mode %d ", mode);
# endif

	switch (mode)
	{
	    case NORMAL:
		ctr = readmon(&c, 1);
		break;

	    case PRIME:
		Pctr = 0;
		ctr = 0;
		break;

	    case SYNC:				/* flush pipe */
		while (readmon(&c, 1) > 0);
		ctr = 0;
		break;

	    default:
		syserr("bad arg '%d' in get_scan", mode);
	}

# ifdef	xPTR3
	tTfp(54, 1, " ctr %d: '%c' (0%o).\n", ctr & 0377, c, c);
# endif

	return (ctr ? c : 0);
}
