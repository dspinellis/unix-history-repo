# include	<sccs.h>

SCCSID(@(#)setflag.c	7.1	2/5/81)

/*
**  SET FLAG
**
**	This routine sets flags from argv.  You give arguments
**	of the argv, the flag to be detected, and the default
**	for that flag (if it is not supplied).  The return value
**	is the flag value.   For example:
**		setflag(argv, 'x', 2);
**	returns zero if the "-x" flag is stated, and one if the
**	"+x" flag is stated.  It returns 2 if the flag is not
**	stated at all.
*/

# define	NULL	0

setflag(argv, flagch, def)
char	**argv;
char	flagch;
int	def;
{
	register char	**p;
	register char	*q;
	register int	rtval;

	rtval = -1;
	for (p = &argv[1]; *p != NULL; p++)
	{
		q = *p;
		if (q[1] != flagch)
			continue;
		if (*q != '-' && *q != '+')
			continue;
		rtval = (q[0] == '+');
	}
	if (rtval < 0)
		rtval = def;
	return (rtval);
}
