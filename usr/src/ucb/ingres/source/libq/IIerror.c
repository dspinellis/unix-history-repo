# include	<ingres.h>
# include	<symbol.h>
# include	"IIglobals.h"
# include	<sccs.h>

SCCSID(%W%	%G%)


/*
**	IIerror -- std. equel error reporting routine.
**
**		IIerror is called either directly by a
**		routine detecting an error or by
**		IIrdpipe where INGRES returns an error.
**
**		In the case of an appropriate error, IIerrflag is set to
**		prevent the reading of the ovqp data pipe. This happens
**		on errors that happen during retrieves to C vars.
**
**		Errno is the error number: 1??? is an equel error
**		others are Ingres errors.
**
**		Argc and argv are structured as in Unix main program calls.
**
**		The error is printed if the call (*IIprint_err)()
**		returns > 0. Otherwise no error message is printed.
**		The error message that is printed is the one corresponding
**		to the number returned by (*IIprint_err)().
*/

extern	IIret_err();
int	(*IIprint_err)() =	IIret_err;


IIerror(errno, argc, argv)
int	errno, argc;
char	*argv [];
{
	register int		i;

#	ifdef xETR1
	if (IIdebug > 1)
		printf("ent IIerror : errno %d, argc %d\n", errno, argc);
#	endif

	IIerrflag = errno;
	if (!(errno = (*IIprint_err)(errno)))
	{
#		ifdef xETR2
		if (IIdebug > 1)
			printf("IIerror : IIprint_err returned 0\n");
#		endif
		/* if must restore printing turned off in IIw_left()
		 * or IIw_right(), then do so.
		 */
		if (IIo_print)
		{
			IIprint_err = IIo_print;
			IIo_print = 0;
		}
		return;
	}

	if (errno > 2000)
		printf("INGRES ERROR: ");
	else
		printf("EQUEL ERROR: ");

	if (!IIp_err(errno, argc, argv))
	{
		/* couldn't process error in IIp_err() */
		printf(" %d with parameters:",errno);
		for (i = 0; i < argc; )
			printf(" %s", argv [i++]);
		printf("\n");
	}
	if ( errno == 3 )
		abort();
	if (IIproc_name)
		printf("On file %s line %d.\n", IIproc_name, IIline_no);
}

/*
**  IIret_err -- returns its single argument for IIerror.
**
*/

IIret_err(err)
int	err;
{
	return (err);
}

/*
**  IIno_err -- returns 0. Called from IIerror
**		(through (*IIprint_err)())
**		to supress error message printing.
*/

IIno_err(err)
int	err;
{
	return (0);
}
