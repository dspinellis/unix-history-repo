# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	<catalog.h>
# include	<pv.h>
# include	<func.h>
# include	"qrymod.h"
# include	<sccs.h>

SCCSID(@(#)d_view.c	7.1	2/5/81)



/*
**  D_VIEW -- define view
**
**	This procedure connects the tree in with the relation catalog
**	and inserts the view tree into the tree catalog.
**
**	The information in the pipe is expected to come as follows:
**		create for view, with S_VIEW bit set so that a
**			physical relation is not created.
**		define tree, which will put the translation tree
**			into the 'tree' catalog.
**		define view, which will connect the two together.
**			The first two absolutely must be done before
**			this step can be called.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		I/O in 'tree' catalog.
**
**	Trace Flags:
**		39
*/

extern DESC	Reldes;

extern		d_view(), null_fn();
extern short	tTqm[80];

struct fn_def	DefViewFn =
{
	"DVIEW",
	d_view,
	null_fn,
	null_fn,
	NULL,
	0,
	tTqm,
	80,
	'Q',
	0
};




d_view(pc, pv)
int	pc;
PARM	*pv;
{
	char		viewid[MAXNAME + 1];
	struct relation	relkey, reltup;
	register QTREE	*t;
	register int	i;
	struct tup_id	tid;
	int		treeid;

	/*
	**  Read parameters.
	*/

	if (pv->pv_type != PV_STR)
		syserr("d_view: viewid");
	pmove(pv->pv_val.pv_str, viewid, MAXNAME, ' ');
	pv++;

	if (pv->pv_type != PV_QTREE)
		syserr("d_view: tree");
	t = (QTREE *) pv->pv_val.pv_qtree;
	pv++;
	
#	ifdef xQTR3
	/* do some extra validation */
	if (Qt.qt_qmode != mdVIEW)
		syserr("d_view: Qt.qt_qmode %d", Qt.qt_qmode);
	if (Qt.qt_resvar < 0)
		syserr("d_view: Rv %d", Qt.qt_resvar);
	if (Qt.qt_rangev[Qt.qt_resvar].rngvdesc == NULL ||
	    !bequal(Qt.qt_rangev[Qt.qt_resvar].rngvdesc->reldum.relid, viewid, MAXNAME))
		syserr("d_view: rangev %d %.14s", Qt.qt_rangev[Qt.qt_resvar].rngvdesc,
		    Qt.qt_rangev[Qt.qt_resvar].rngvdesc->reldum.relid);
#	endif

	declare(Qt.qt_resvar, NULL);
	Qt.qt_resvar = -1;
	Qt.qt_qmode = -1;

	/* output tree to tree catalog */
	treeid = puttree(t, viewid, Usercode, mdVIEW);
}
