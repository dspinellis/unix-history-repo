# include <ingres.h> 
# include <symbol.h>
# include <range.h>
# include "globs.h"
# include	<sccs.h>

SCCSID(@(#)de_init.c	7.1	2/5/81)

/*ARGSUSED*/
de_init(argc, argv)
int	argc;
char	**argv;
{
	Batchupd = setflag(argv, 'b', 1);

	/*
	** Do the necessary decomp initialization. This includes
	** buffering standard output (if i/d system) and giving
	** access methods more pages (if i/d system).
	** init_decomp is defined in either call_ovqp or call_ovqp70.
	*/

	init_decomp();
}
/*
**  RUBPROC -- process a rubout signal
**
**	Called from the principle rubout catching routine
**	when a rubout is to be processed. Notice that rubproc
**	must return to its caller and not call reset itself.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		reinitializes the state of the world.
**
**	Called By:
**		rubcatch
*/


de_rubproc()
{
	extern int	Equel;

	/*
	** Sync with equel if we have the equel pipe.
	**	This can happen only if ovqp and decomp
	**	are combined.
	*/
/*
	if (W_front >= 0 && Equel)
	Error_flag = pv[0].pv_val.pv_int;
		wrpipe(P_INT, &pipebuf, W_front);
*/

	endovqp(RUBACK);
	reinit();
	return;
}
