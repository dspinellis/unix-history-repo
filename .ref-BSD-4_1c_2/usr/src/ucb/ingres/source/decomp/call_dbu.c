# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	<pv.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)call_dbu.c	7.1	2/5/81)

int Synconly, Error_flag;


/*
**	Call the appropriate dbu with the arguments
**	given in the globals Pc and Pv. Code is a
**	number identifing which dbu to call. Errflag
**	indicates whether an error return from the dbu
**	is possibly expected.
**
**	If errflag is FALSE then call_dbu will syserr on an error
**	If errflag is TRUE then call_dbu will return error value
**
**	Trace Flags:
**		60
*/

call_dbu(code, errflag)
int	code;
bool	errflag;
{
#	ifdef xDTR1
	if (tTf(60, 0))
		printf("Calling DBU %d\n", code);
#	endif

	Error_flag = 0;
	call(code, NULL);
	if (Error_flag != 0 && !errflag)
		syserr("call_dbu:%d,ret %d", code, Error_flag);
	return(Error_flag);
}



/*
**	Proc_error is called if an error
**	block is encountered.
**	Otherwise the error block(s) are passed on up.
*/

/*ARGSUSED*/
catcherr(pc, pv)
int	pc;
PARM	*pv;
{
	extern int	Error_flag;

	Error_flag = pv[0].pv_val.pv_int;
}
