# include	<ingres.h>
# include	<access.h>
# include	<symbol.h>
# include	<aux.h>
# include	<sccs.h>

SCCSID(@(#)compare.c	7.1	2/5/81)

icompare(ax, bx, frmt, frml)
char	*ax, *bx, frmt, frml;
{
	register ANYTYPE	*a, *b;
	register int		length;
	ANYTYPE			atemp, btemp;

	length = frml & I1MASK;
	if (frmt == CHAR)
		return (scompare(ax, length, bx, length));
	a = &atemp;
	b = &btemp;
	bmove(ax, (char *) a, length);
	bmove(bx, (char *) b, length);
	if (bequal((char *) a, (char *) b, length))
		return (0);
	switch (frmt)
	{
	  case INT:
		switch (length)
		{
		  case 1:
			return (a->i1type - b->i1type);
		  case 2:
			return (a->i2type - b->i2type);
		  case 4:
			return (a->i4type > b->i4type ? 1 : -1);
		}
		break;

	  case FLOAT:
		if (frml == 4)
			return (a->f4type > b->f4type ? 1 : -1);
		else
			return (a->f8type > b->f8type ? 1 : -1);
		break;
	}
	syserr("compare: t=%d,l=%d", frmt, frml);
	/*NOTREACHED*/
}
/*
**  KCOMPARE -- key compare
**
**	compares all domains indicated by SETKEY in the tuple to the
**	corressponding domains in the key.
**	the comparison is done according to the format of the domain
**	as specified in the descriptor.
**
**	function values:
**		<0 tuple < key
** 		=0 tuple = key
**		>0 tuple > key
*/

kcompare (dx, tuple, key)
DESC	*dx;			/*relation descriptor	*/
char	tuple[MAXTUP];		/*tuple to be compared	*/
char	key[MAXTUP];		/*second tuple or key	*/
{
	register int	i, tmp;
	register DESC	*d;

	d = dx;
	for (i = 1; i <= d->reldum.relatts; i++)
		if (d->relgiven[i])
			if (tmp = icompare(&tuple[d->reloff[i]],
			        &key[d->reloff[i]],
			        d->relfrmt[i],
				d->relfrml[i]))
			{
				return (tmp);
			}
	return (0);
}
