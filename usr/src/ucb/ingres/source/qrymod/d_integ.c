# include	<ingres.h>
# include	<aux.h>
# include	<catalog.h>
# include	<tree.h>
# include	<symbol.h>
# include	<pv.h>
# include	<resp.h>
# include	<func.h>
# include	"qrymod.h"
# include	<sccs.h>

SCCSID(@(#)d_integ.c	7.1	2/5/81)


/*
**  D_INTEG -- define integrity constraint
**
**	An integrity constraint (as partially defined by the last
**	tree defined by d_tree) is defined.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		Activity in 'relation' and 'integrities' catalogs.
**
**	Trace Flags:
**		49
*/

extern DESC	Intdes;
extern DESC	Reldes;

extern		d_integ(), null_fn();
extern short	tTqm[80];

struct fn_def	DefIntFn =
{
	"DINTEG",
	d_integ,
	null_fn,
	null_fn,
	NULL,
	0,
	tTqm,
	80,
	'Q',
	0
};



d_integ(pc, pv)
int	pc;
PARM	*pv;
{
	register int		i;
	register QTREE		*t;		/* definition tree */
	struct integrity	inttup;
	struct tup_id		tid;
	register int		rv;		/* result variable */
	struct relation		relkey;
	struct relation		reltup;

	if (pv[0].pv_type != PV_QTREE)
		syserr("d_integ: tree");
	t = pv[0].pv_val.pv_qtree;
	rv = Qt.qt_resvar;

	/*
	**  Check for valid environment.
	**	The tree must exist, have a qualification, and have
	**	no target list.  The query mode must be mdINTEG.
	**
	**	User level stuff checks to see that this is single
	**	variable aggregate free, since that's all we know
	**	about thusfar.  Also, the relation in question must
	**	not be a view.
	*/

#	ifdef xQTR3
	if (t == NULL)
		syserr("d_integ: NULL tree");
	if ((i = t->right->sym.type) != AND)
		syserr("d_integ: qual %d", i);
	if ((i = t->left->sym.type) != TREE)
		syserr("d_integ: TL %d", i);
	if (Qt.qt_qmode != mdINTEG)
		syserr("d_integ: Qmode %d", Qt.qt_qmode);
#	endif
	
	/* check for aggregates */
	if (aggcheck(t))
		qmerror(3490, -1, rv, 0);	/* aggregates in qual */

	/* check for multi-variable */
	for (i = 0; i < MAXRANGE; i++)
	{
		if (Qt.qt_rangev[i].rngvdesc == NULL)
			continue;
		if (i != rv)
		{
#			ifdef xQTR3
			if (tTf(49, 1))
				printf("d_integ: Rv %d(%.14s) i %d(%.14s)\n",
				    rv, Qt.qt_rangev[rv].rngvdesc->reldum.relid,
				    i, Qt.qt_rangev[i].rngvdesc->reldum.relid);
#			endif
			qmerror(3491, -1, rv, 0);	/* too many vars */
		}
	}

	/* check for the resultvariable being a real relation */
	if (bitset(S_VIEW, Qt.qt_rangev[rv].rngvdesc->reldum.relstat))
		qmerror(3493, -1, rv, 0);	/* is a view */
	
	/* guarantee that you own this relation */
	if (!bequal(Usercode, Qt.qt_rangev[rv].rngvdesc->reldum.relowner, 2))
		qmerror(3494, -1, rv, 0);	/* don't own reln */

	/*
	**  Guarantee that the integrity constraint is true now.
	**	This involves issuing a retrieve statement for the
	**	inverse of the qualification.  The target list is
	**	already null, so we will get nothing printed out
	**	(only a return status).
	**
	**	We reset resp_tups if ok so that the user isn't annoyed
	**	by a tuple count.  On error, it is a count of the
	**	number of tuples that don't satisfy.
	*/

	Qt.qt_qmode = mdRETR;
	Qt.qt_resvar = -1;

	/* issue the invert of the query */
	issueinvert(t);
	if (Resp.resp_tups != 0)
		qmerror(3492, -1, rv, 0);	/* constraint not satisfied */
	Resp.resp_tups = -1;

	/*
	**  Set up the rest of the environment.
	*/

	opencatalog("integrities", 2);
	clr_tuple(&Intdes, &inttup);
	Qt.qt_resvar = -1;
	Qt.qt_qmode = -1;

	/*
	**  Set up integrity relation tuple.
	**	The qualification will be scanned, and a set of
	**	domains referenced will be created.  Other stuff
	**	is filled in from the range table and from the
	**	parser.
	**
	**	The tree is actually inserted into the tree catalog
	**	in this step.  Extra information is cleared here.
	*/

	inttup.intresvar = rv;
	bmove(Qt.qt_rangev[rv].rngvdesc->reldum.relid, inttup.intrelid, MAXNAME);
	bmove(Qt.qt_rangev[rv].rngvdesc->reldum.relowner, inttup.intrelowner, 2);
	makeidset(rv, t, inttup.intdomset);
	inttup.inttree = puttree(t, inttup.intrelid, inttup.intrelowner, mdINTEG);

	/*
	**  Insert tuple into integrity catalog.
	*/

	i = insert(&Intdes, &tid, &inttup, FALSE);
	if (i < 0)
		syserr("d_integ: insert");
	if (noclose(&Intdes) != 0)
		syserr("d_integ: noclose int");

	/*
	**  Update relstat S_INTEG bit.
	*/

	if (!bitset(S_INTEG, Qt.qt_rangev[rv].rngvdesc->reldum.relstat))
	{
		opencatalog("relation", 2);
		setkey(&Reldes, &relkey, inttup.intrelid, RELID);
		setkey(&Reldes, &relkey, inttup.intrelowner, RELOWNER);
		i = getequal(&Reldes, &relkey, &reltup, &tid);
		if (i != 0)
			syserr("d_integ: geteq");
		reltup.relstat |= S_INTEG;
		i = replace(&Reldes, &tid, &reltup, FALSE);
		if (i != 0)
			syserr("d_integ: replace");
		if (noclose(&Reldes) != 0)
			syserr("d_integ: noclose rel");
	}

	return (0);
}


makeidset(varno, tree, dset)
int	varno;
QTREE	*tree;
int	dset[8];
{
	register int	vn;
	register QTREE	*t;

	vn = varno;
	t = tree;

	while (t != NULL)
	{
		if (t->sym.type == VAR && t->sym.value.sym_var.varno == vn)
			lsetbit(t->sym.value.sym_var.attno, dset);
		
		/* handle left subtree recursively */
		makeidset(vn, t->left, dset);

		/* handle right subtree iteratively */
		t = t->right;
	}
}
