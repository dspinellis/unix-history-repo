# include	<ingres.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)rnum.c	7.1	2/5/81)


/*
**	Internal numbers are used in decomp to
**	represent relation names. The numbers
**	from 0 to FIRSTNUM-1 refer to the names
**	stored in De.de_name_table[].
**
**	The number from FIRSTNUM to LASTNUM have
**	names which are computed from aa, ab, etc.
*/




/*
**	Assign an internal number rnum to name.
*/

rnum_assign(name)
char	*name;
{
	register int	i;

	for (i = 0; i < FIRSTNUM; i++)
		if (De.de_num_used[i] == 0)
		{
			bmove(name, De.de_name_table[i], MAXNAME);
			De.de_num_used[i]++;
			return (i);
		}
	syserr("rnum_assign:no room");
	return (-1);
}
/*
**	Allocate the next available name
*/

rnum_alloc()
{
	register int	i;
	register char	*cp;

	cp = &De.de_num_used[FIRSTNUM];
	for (i = FIRSTNUM; i < LASTNUM; i++)
		if (*cp++ == 0)
		{
			--cp;
			(*cp)++;
			return (i);
		}
	syserr("no free names");
	return (-1);
}
/*
**	Convert internal relation number
**	to its real name. Guarantee '\0' at end.
*/

char *
rnum_convert(num)
int	num;
{
	register int	i;
	register char	*ret, *cp;
	static char	temp[MAXNAME+1];
	extern char	*Fileset;
	extern char	*concat();

	i = num;
	if (i > LASTNUM || De.de_num_used[i] == 0)
		syserr("no name for %d", i);

	ret = temp;

	if (i < FIRSTNUM)
	{
		bmove(De.de_name_table[i], ret, MAXNAME);
	}
	else
	{
		/* compute temp name */
		cp = concat("_SYS", Fileset, ret);
		pad(ret, MAXNAME);
		i -= FIRSTNUM;
		*cp++ = i/26 + 'a';
		*cp = i%26 + 'a';
	}
	return (ret);
}
/*
**	Remove a num from the used list
*/

rnum_remove(num)
int	num;
{
	register char	*cp;

	cp = &De.de_num_used[num];

	if (*cp == 0)
		syserr("cant remove %d", num);
	*cp = 0;
}
/*
**	returns number of largest assigned temp number.
**	zero if none
*/

rnum_last()
{
	register int	i;

	for (i = LASTNUM - 1; i >= FIRSTNUM; i--)
	{
		if (De.de_num_used[i])
		{
			return (i);
		}
	}

	return (0);
}
/*
**	Predicate to check whether rnum is a temporary relation or not
*/

rnum_temp(rnum)
int	rnum;
{
	register int	i;

	i = rnum;

	return (i >= FIRSTNUM || bequal("_SYS", rnum_convert(i), 4));
}
/*
**	Clear tag fields from previous query
*/

rnum_init()
{
	register char	*cp;
	register int	i;

	cp = De.de_num_used;
	i = FIRSTNUM;
	while (--i)
		*cp++ = 0;
}
