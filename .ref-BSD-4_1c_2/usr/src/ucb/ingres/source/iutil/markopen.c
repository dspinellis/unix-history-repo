# include	<useful.h>
# include	<opsys.h>
# include	<sccs.h>

SCCSID(@(#)markopen.c	7.1	2/5/81)

/*
**  MARKOPEN -- mark all open files
**
**	Marked files will not be closed later.
**
**	Parameters:
**		ovect -- pointer to bitmap of open files.
**
**	Returns:
**		none
**
**	Side Effects:
**		Sets *ovect to represent the open files.
*/

long	CmOfiles;	/* default set of files, used all over */

markopen(ovect)
register long	*ovect;
{
	register int	i;
	register int	j;
	extern int	errno;
	struct stat	sbuf;

	if (ovect == NULL)
		ovect = &CmOfiles;

	*ovect = 0;
	for (i = 0; i < NOFILE; i++)
	{
		if (fstat(i, &sbuf) >= 0)
			*ovect |= 1 << i;
	}
	errno = 0;
}
/*
**  CLOSEALL -- close all open files (except marked files)
**
**	Parameters:
**		tell -- if set, report files that are open and should
**			not have been.
**		ovect -- vector of files to leave open.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		none
*/

closeall(tell, ovect)
register int	tell;
register long	ovect;
{
	register int	i;

	ovect |= CmOfiles;

	for (i = 0; i < NOFILE; i++)
	{
		if (!bitset(1 << i, ovect))
			if (close(i) >= 0 && tell)
				lprintf("File %d open\n", i);
	}
}
