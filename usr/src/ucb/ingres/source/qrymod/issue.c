# include	<ingres.h>
# include	<aux.h>
# include	<symbol.h>
# include	<tree.h>
# include	<pv.h>
# include	"qrymod.h"
# include	<sccs.h>

SCCSID(@(#)issue.c	7.1	2/5/81)

/*
**  ISSUE -- issue query to rest of system
**
**	This function issues a query to the rest of the INGRES system.
**	The sync from below is read, but not passed up.
**
**	Parameters:
**		tree -- pointer to tree to issue.
**
**	Returns:
**		none.
**
**	Side Effects:
**		A query is executed.
**
**	Trace Flags:
**		71
*/

issue(state, tree)
int	state;
QTREE	*tree;
{
#	ifdef xQTR2
	if (tTf(71, 0))
		printf("issue:\n");
#	endif

	initp();
	setp(PV_QTREE, tree, 0);
	call(state, NULL);
}
/*
**  ISSUEINVERT -- issue a query, but invert the qualification
**
**	This routine is similar to 'issue', except that it issues
**	a query with the qualification inverted.  The inversion
**	(and subsequent tree normalization) is done on a duplicate
**	of the tree.
**
**	Parameters:
**		root -- the root of the tree to issue.
**
**	Returns:
**		none.
**
**	Side Effects:
**		'root' is issued.
**
**	Trace Flags:
**		none
*/

issueinvert(root)
QTREE	*root;
{
	register QTREE	*t;
	register QTREE	*uop;
	extern QTREE	*treedup();
	extern QTREE	*trimqlend(), *norml();

	/* make duplicate of tree */
	t = treedup(root);

	/* prepend NOT node to qualification */
	uop = (QTREE *) need(Qbuf, QT_HDR_SIZ + sizeof(short));
	uop->left = NULL;
	uop->right = t->right;
	uop->sym.type = UOP;
	uop->sym.len = sizeof(short);
	uop->sym.value.sym_op.opno = opNOT;
	t->right = uop;

	/* normalize and issue */
	t->right = norml(trimqlend(t->right));
	issue(mdQRY, t);
}
