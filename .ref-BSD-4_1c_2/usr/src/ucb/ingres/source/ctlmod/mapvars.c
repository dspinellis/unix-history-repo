# include	<ingres.h>
# include	<symbol.h>
# include	<tree.h>
# include	<sccs.h>

SCCSID(@(#)mapvars.c	7.1	2/5/81)

/*
**  MAPVARS -- remap varno's to be unique in 'tree' tree
**
**	A tree is scanned for VAR nodes; when found, the
**	mapping defined in Qt.qt_remap[] is applied.  This is done so that
**	varno's as defined in trees in the 'tree' catalog will be
**	unique with respect to varno's in the user's query tree.  For
**	example, if the view definition uses variable 1 and the user's
**	query also uses variable 1, the routine 'readqry' will (after
**	calling 'declare' to assign a new slot), put the index of this
**	new slot into the corresponding entry of Qt.qt_remap;
**	in this example, Qt.qt_remap[1] == 3.  This routine does the actual
**	mapping in the tree.
**
**	Parameters:
**		tree -- pointer to tree to be remapped.
**
**	Returns:
**		none
**
**	Side Effects:
**		the tree pointed to by 'tree' is modified according
**		to Qt.qt_remap[].
**
**	Trace Flags:
**		7.4-7.7
*/

mapvars(tree)
QTREE	*tree;
{
	register QTREE	*t;
	register int	i;

	t = tree;
#	ifdef xQTR3
	if (tTf(7, 4) && t != NULL && t->sym.type == ROOT)
	{
		printf("mapvars:");
		treepr(t);
		for (i = 0; i < MAXVAR + 1; i++)
			if (Qt.qt_rangev[i].rngvdesc != NULL && Qt.qt_remap[i] >= 0)
				printf("\t%d => %d\n", i, Qt.qt_remap[i]);
	}
#	endif

	while (t != NULL)
	{
		/* map right subtree */
		mapvars(t->right);

		/* check this node */
		if (t->sym.type == VAR)
			t->sym.value.sym_var.varno = Qt.qt_remap[t->sym.value.sym_var.varno];

		/* map left subtree (iteratively) */
		t = t->left;
	}
}
