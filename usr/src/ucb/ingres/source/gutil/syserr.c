# include	<stdio.h>
# include	<sccs.h>

SCCSID(@(#)syserr.c	7.1	2/5/81)

/*
**  SYSERR -- SYStem ERRor message print and abort
**
**	Syserr acts like a printf with up to five arguments.
**
**	If the first argument to syserr is not zero,
**	the message "SYSERR:" is prepended.
**
**	If the extern variable `Proc_name' is assigned to a
**	string, that string is prepended to the message.
**
**	All arguments must be null-terminated.
**
**	The function pointed to by `ExitFn' is then called.
**	It is initialized to be `exit'.
*/

char	*Proc_name;
int	Accerror;
extern	exit(), abort();
int	(*ExitFn)()	 = exit;

syserr(pv)
char	*pv;
{
	int		pid;
	register char	**p;
	extern int	errno;
	register int	usererr;
	register int	exitvalue;

	p = &pv;
	printf("\n");
	usererr = pv == 0;

	if (!usererr)
	{
		if (Proc_name)
			printf("%s ", Proc_name);
		printf("\007SYSERR: ");
	}
	else
		p++;
	printf(p[0], p[1], p[2], p[3], p[4], p[5]);
	printf("\007\n");
	exitvalue = -1;
	if (!usererr)
	{
		if (errno)
		{
			exitvalue = errno;
			printf("\tUNIX error %d\n", exitvalue);
		}
		if (Accerror != 0)
		{
			printf("\taccess method error %d\n", Accerror);
		}
	}
	fflush(stdout);
	if (ExitFn == exit)
		ExitFn = abort;
	(*ExitFn)(exitvalue);
}
