# include	<stdio.h>
# include	<sccs.h>

SCCSID(@(#)IIsyserr.c	7.1	2/5/81)


/*
**  SYSERR -- SYStem ERRor message print and abort
**
**	Syserr acts like a printf with up to five arguments.
**
**	If the first argument to syserr is not zero,
**	the message "SYSERR:" is prepended.
**
**	If the extern variable `IIproc_name' is assigned to a
**	string, that string is prepended to the message.
**
**	All arguments must be null-terminated.
**
**	The function pointed to by `Exitfn' is then called.
**	It is initialized to be `exit'.
*/

extern char	*IIproc_name;

IIsyserr(pv)
char	*pv;
{
	int		pid;
	register char	**p;
	extern int	errno;
	register int	usererr;

	p = &pv;
	printf("\n");
	usererr = pv == 0;

	if (!usererr)
	{
		if (IIproc_name)
			printf("%s ", IIproc_name);
		printf("SYSERR: ");
	}
	else
		p++;
	printf(p[0], p[1], p[2], p[3], p[4], p[5]);
	printf("\n");
	if (!usererr && errno)
		printf("\tsystem error %d\n", errno);
	fflush(stdout);
	exit (-1);
}
