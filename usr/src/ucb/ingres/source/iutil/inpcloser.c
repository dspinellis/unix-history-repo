# include	<ingres.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)inpcloser.c	7.1	2/5/81)

/*
**	inpcloser - close an input relation
**
**	The relation must have been opened by openr with 
**		mode 0 (read only)
**
**	return values:
**		<0 fatal error
**		 0 success
**		 1 relation was not open
**		 2 relation was opened in write mode
**
**	Trace Flags:
**		21.10-11
*/

inpcloser(d)
register DESC	*d;
{
	register int	i;

#	ifdef xATR1
	if (tTf(21, 10))
		printf("inpcloser: %.14s\n", d->reldum.relid);
#	endif
	if (abs(d->relopn) != (d->relfp + 1) * 5)
		/* relation not open */
		return (1);

	if (d->relopn < 0)
		return (2);	/* relation open in write mode */

	i = flush_rel(d, TRUE);	/* flush and reset all pages */

	if (close(d->relfp))
		i = acc_err(AMCLOSE_ERR);
	d->relopn = 0;
	return (i);
}
