# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)mklist.c	7.1	2/5/81)

/*
**  MKLIST
**
**	writes a list of query tree nodes in "OVQP order" --
**	that is, everything in postfix (endorder) except AND's and OR's
**	infixed (postorder) to OVQP.
**	called by call_ovqp().
*/



mklist(tree)
QTREE *tree;
{
	register int	typ;
	register QTREE 	*t;
	register int 	andor;

	t = tree;
	if (!t || (typ=t->sym.type)==TREE || typ==QLEND) 
		return;

	andor=0;
	mklist(t->left);
	if (typ==AND || typ==OR)
	{
		andor = 1;
		ovqpnod(t);
	}
	mklist(t->right);
	if (!andor)
		ovqpnod(t);
}
