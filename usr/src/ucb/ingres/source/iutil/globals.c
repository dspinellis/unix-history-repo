# include	<sccs.h>

SCCSID(@(#)globals.c	7.1	2/5/81)

/*
**  GLOBALS -- INGRES globals which belong everywhere
**
**	Variables in this module should be included by everything
**	INGRES knows about.  The real purpose of this module is
**	so that actual definition of space can occur here (and
**	everything can be 'extern' everywhere else).
**
**	Defines:
**		Alockdes -- the lock descriptor for the concurrency
**			device.
*/






int	Alockdes	= -1;	/* the concurrency device descriptor */
