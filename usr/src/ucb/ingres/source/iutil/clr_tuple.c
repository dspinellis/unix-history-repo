# include	<ingres.h>
# include	<symbol.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)clr_tuple.c	7.1	2/5/81)


/*
**	Clr_tuple initializes all character domains
**	to blank and all numeric domains to zero.
*/

clr_tuple(desc, tuple)
struct descriptor	*desc;
char			*tuple;
{
	register struct descriptor	*d;
	register char			*tup;
	register int			i;
	int				j, pad;

	d = desc;

	for (i = 1; i <= d->reldum.relatts; i++)
	{
		if (d->relfrmt[i] == CHAR)
			pad = ' ';
		else
			pad = 0;

		tup = &tuple[d->reloff[i]];
		j = d->relfrml[i] & I1MASK;

		while (j--)
			*tup++ = pad;
	}
}
