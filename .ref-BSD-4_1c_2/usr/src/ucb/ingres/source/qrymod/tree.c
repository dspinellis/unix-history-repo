# include	<ingres.h>
# include	<tree.h>
# include	<symbol.h>
# include	"qrymod.h"
# include	<sccs.h>

SCCSID(@(#)tree.c	7.1	2/5/81)

/*
**  TREE -- create new tree node.
**
**	This is a stripped down version of the same thing in the
**	parser.
**
**	It only knows about lengths of zero and two.
**
**	Parameters:
**		lptr -- the left pointer.
**		rptr -- the right pointer.
**		typ -- the node type.
**		len -- the node length.
**		value -- the node value.
**
**	Returns:
**		A pointer to the created node.
**
**	Side Effects:
**		Space is taken from Qbuf.
*/


QTREE *
tree(lptr, rptr, typ, len, value)
QTREE	*lptr;
QTREE	*rptr;
char	typ;
int	len;
int	value;
{
	register QTREE	*tptr;
	extern char	*need();
	register int	l;

	l = len;

	tptr = (QTREE *) need(Qbuf, l + QT_HDR_SIZ);
	tptr->left = lptr;
	tptr->right = rptr;
	tptr->sym.type = typ;
	tptr->sym.len = l;

	if (l > 0)
		tptr->sym.value.sym_data.i2type = value;
	return (tptr);
}
