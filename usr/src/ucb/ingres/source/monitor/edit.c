# include	"monitor.h"
# include	<ingres.h>
# include	<aux.h>
# include	<opsys.h>
# include	<sccs.h>

SCCSID(@(#)edit.c	7.1	2/5/81)



/*
**  CALL TEXT EDITOR
**
**	The UNIX text editor is called.  The actual call is to
**	the macro {editor}.  If that fails, /bin/ed is called.
**	This routine suppressed the autoclear function.
**
**	Uses trace flag 4
*/

edit()
{
	register int	i;
	register char	*p;
	register char	*editfile;
	extern char	*getfilenm(), *macro();

	editfile = getfilenm();
	if (*editfile == 0)
		editfile = Qbname;

	Autoclear = 0;
	fclose(Qryiop);

	/* FORK SENTRY PROCESS & INVOKE THE EDITOR */
	if ((Xwaitpid = fork()) < 0)
		syserr("edit: fork");
	if (Xwaitpid == 0)
	{
		setuid(getuid());
#		ifndef xB_UNIX
		setgid(getgid());
#		endif
		for (i = 3; i < NOFILE; i++)
			close(i);
		p = macro("{editor}");
		if (p != 0)
		{
			execl(p, p, editfile, 0);
			printf("Cannot call %s; using /bin/ed\n", p);
		}
		execl("/bin/ed", "ed", editfile, 0);
		syserr("edit: exec");
	}

	/* WAIT FOR SENTRY TO DIE */
	if (Nodayfile >= 0)
		printf(">>ed\n");
	xwait();
}
