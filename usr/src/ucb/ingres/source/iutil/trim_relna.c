# include	<ingres.h>
# include	<sccs.h>

SCCSID(@(#)trim_relna.c	7.1	2/5/81)

/*
**  TRIM_RELNM -- trim blanks from relation name for printing
**
**	A relation name (presumably in 'ingresname' format: MAXNAME
**	characters long with no terminating null byte) has the
**	trailing blanks trimmed off of it into a local buffer, so
**	that it can be printed neatly.
**
**	Parameters:
**		name -- a pointer to the relation name
**
**	Returns:
**		a pointer to the trimmed relation name.
**
**	Side Effects:
**		none
*/

char *
trim_relname(name)
char	*name;
{
	register char	*old, *new;
	register int	i;
	static char	trimname[MAXNAME + 1];

	old = name;
	new = trimname;
	i = MAXNAME;

	while (i--)
		if ((*new++ = *old++) == ' ')
		{
			new--;
			break;
		}

	*new = '\0';

	return (trimname);
}
