# include	<ingres.h>
# include	<symbol.h>
# include	<tree.h>
# include	<pv.h>
# include	"parser.h"
# include	<sccs.h>

SCCSID(@(#)call_tree.c	7.1	2/5/81)

/*
**  CALL_TREE -- call the appropriate module below
**
**	Call_tree prepends a TREE node to the leftmost node on the tree,
**	adds the tree to the PARM, and does a CM call().
**
**	Parameters:
**		qmode -- qmode of query
**		dest -- module to call
**		err_fcn() -- function to call on error
**
**	Returns:
**		nothing
**
**	Trace Flags:
**		call_tree ~~ 44.0, 44.4
*/

call_tree(qmode, dest, err_fcn)
register int	qmode;
int		dest;
int		(*err_fcn)();
{
	extern int	Resrng;
	extern QTREE	*Lastree;

#	ifdef	xPTR2
	tTfp(44, 0, "call_tree: qm=%d, dest=%d\n", qmode, dest);
#	endif

	Qt.qt_qmode = qmode;

#	ifdef	xPTR2

	if (tTf(44, 4))
		if (Resrng >= 0)
			printf("resvarno:%d\n", Resrng);
#	endif

	Qt.qt_resvar = Resrng;

	/* the following attaches the TREE node to the far left of the tree */

	tlprepend(tree(NULL, NULL, TREE, 0), Lastree);

	setp(PV_QTREE, Lastree);

	call(dest, err_fcn);

#	ifdef	xPTR2
	tTfp(44, 5, "Call_tree: call returned\n");
#	endif
}
