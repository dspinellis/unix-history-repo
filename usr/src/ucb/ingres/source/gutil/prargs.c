# include	<sccs.h>

SCCSID(@(#)prargs.c	7.1	2/5/81)

/*
**  PRARGS -- print argument list
**
**	Takes an argument list such as expected by any main()
**	or the DBU routines and prints them on the standard
**	output for debugging purposes.
**
**	Parameters:
**		pc -- parameter count.
**		pv -- parameter vector (just like to main()).
**
**	Returns:
**		nothing
**
**	Side Effects:
**		output to stdout only.
*/

prargs(pc, pv)
int	pc;
char	**pv;
{
	register char	**p;
	register char	c;
	int		n;
	register char	*q;

	n = pc;
	printf("#args=%d:\n", n);
	for (p = pv; n-- > 0; p++)
	{
		q = *p;
		while ((c = *q++) != 0)
			xputchar(c);
		putchar('\n');
	}
}
