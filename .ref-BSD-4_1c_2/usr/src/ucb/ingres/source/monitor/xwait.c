# include	"monitor.h"
# include	<ingres.h>
# include	<aux.h>
# include	<sccs.h>

SCCSID(@(#)xwait.c	7.1	2/5/81)



/*
**  WAIT FOR PROCESS TO DIE
**
**	Used in edit() and shell() to terminate the subprocess.
**	Waits on pid "Xwaitpid".  If this is zero, xwait() returns
**	immediately.
**
**	This is called by "catchsig()" so as to properly terminate
**	on a rubout while in one of the subsystems.
**
**	Trace Flags:
**		41
*/

xwait()
{
	int		status;
	register int	i;
	register char	c;

#	ifdef xMTR2
	if (tTf(41, 0))
		printf("xwait: [%d]\n", Xwaitpid);
#	endif
	if (Xwaitpid == 0)
	{
		cgprompt();
		return;
	}
	while ((i = wait(&status)) != -1)
	{
#		ifdef xMTR2
		if (tTf(41, 1))
			printf("pid %d stat %d\n", i, status);
#		endif
		if (i == Xwaitpid)
			break;
	}

	Xwaitpid = 0;

	/* reopen query buffer */
	if ((Qryiop = fopen(Qbname, "a")) == NULL)
		syserr("xwait: open %s", Qbname);
	Notnull = 1;

	cgprompt();
}
