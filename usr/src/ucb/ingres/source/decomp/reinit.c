# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	<lock.h>
# include	<sccs.h>

SCCSID(@(#)reinit.c	7.1	2/5/81)

/*
** REINIT -- reinitialize decomp upon end of query, error, or interrupt.
**	All open relations are closed, temp relations destroyed,
**	and relation locks released.
*/

reinit()
{
	closers();
	if (Lockrel)	/* release all relation locks */
		unlall();
}
