# include	<ingres.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)closer.c	7.1	2/5/81)

/*
**	closer - close a relation
**
**	CLOSER is used to close a relation which was opened by OPENR.
**	CLOSER should always be called once for each OPENR.
**
**	function values:
**
**		<0  fatal error
**		 0  success
**		 1  relation was not open
*/

closer(d)
DESC	*d;
{
	register DESC	*dx;
	register int	i;

	dx = d;
#	ifdef xATR1
	if (tTf(21, 8))
		printf("closer: %.14s,%ld\n", dx->reldum.relid, dx->reladds);
#	endif

	if (i = noclose(dx))
		return (i);

	flush_rel(dx, TRUE);	/* No error is possible since noclose()
				** has already flushed any pages
				*/

	if (close(dx->relfp))	/*close the relation*/
		i = acc_err(AMCLOSE_ERR);

	dx->relopn = 0;
	return (i);
}
