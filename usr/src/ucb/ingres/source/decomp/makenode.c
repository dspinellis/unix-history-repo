# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)makenode.c	7.1	2/5/81)




/*
**	Make a copy of a tree.
*/

QTREE *
copytree(r, buf)
register QTREE	*r;
char		*buf;
{
	register QTREE	*q;
	register int	length;
	extern char	*need();

	if (r == NULL)
		return (0);

	length = r->sym.len & I1MASK;
	q = (QTREE *) need(buf, length + QT_HDR_SIZ);
	bmove((char *)&r->sym, (char *)&q->sym, length + SYM_HDR_SIZ);

	q->left = copytree(r->left, buf);
	q->right = copytree(r->right, buf);

	return (q);
}
/*
**	Make a new copy of the root by making
**	new AND nodes and connecting them to the
**	same branches.
**
**	Trace Flags:
**		64
*/

QTREE *
copy_ands(root, buf)
QTREE	*root;
char 	*buf;
{
	register QTREE	*q;
	register QTREE	*x, *y;
	QTREE		*newroot;
	extern char	*need();
	register int	len;

#	ifdef xDTR1
	if (tTf(64, -1))
		printf("COPY_ANDS");
#	endif
	newroot = (QTREE *) need(buf, 0);
	y = 0;

	for (q=root; q->sym.type != QLEND; q=q->right)
	{
		len = q->sym.len & I1MASK;
		x = (QTREE *) need(buf, len + QT_HDR_SIZ);
		x->left = q->left;
		bmove((char *)&q->sym, (char *)&x->sym, len + SYM_HDR_SIZ);
		if (y)
			y->right = x;
		y = x;
	}
	y->right = q;

#	ifdef xDTR1
	if (tTf(64, 0))
	{
		printf("New tree\n");
		treepr(newroot);
	}
#	endif
	return(newroot);
}



QTREE *
makroot(buf)
char 	*buf;
{
	register QTREE *s;
	extern char	*need();

	s = (QTREE *) need(buf, QT_HDR_SIZ + sizeof (struct rootnode));
	s->right = De.de_qle;
	s->left = De.de_tr;
	s->sym.value.sym_root.rootuser = FALSE;
	s->sym.value.sym_root.lvarm = 0;
	s->sym.value.sym_root.rvarm = 0;
	s->sym.value.sym_root.tvarc = 0;
	s->sym.value.sym_root.lvarc = 0;
	s->sym.type = ROOT;
	s->sym.len = sizeof (struct rootnode);
	return (s);
}


QTREE *
makresdom(buf, node)
char	*buf;
QTREE	*node;
{
	register QTREE	*res, *n;
	extern char	*need();

	n = node;
	res = (QTREE *) need(buf, QT_HDR_SIZ + sizeof(struct resdomnode));
	res->sym.type = RESDOM;
	res->sym.len = sizeof(struct resdomnode);
	if (n->sym.type == AOP)
	{
		res->sym.value.sym_resdom.resfrmt = n->sym.value.sym_op.agfrmt;
		res->sym.value.sym_resdom.resfrml = n->sym.value.sym_op.agfrml;
	}
	else
	{
		res->sym.value.sym_resdom.resfrmt = n->sym.value.sym_var.varfrmt;
		res->sym.value.sym_resdom.resfrml = n->sym.value.sym_var.varfrml;
	}
	return (res);
}

QTREE *
makavar(node, varnum, attnum)
QTREE	*node;
int	varnum, attnum;
{
	register QTREE	*avar, *n;
	extern char	*need();

	n = node;

	avar = (QTREE *) need(De.de_qbuf, QT_HDR_SIZ + sizeof(struct varnode));
	avar->left = avar->right = NULL;
	avar->sym.value.sym_var.valptr = NULL;
	avar->sym.type = VAR;
	avar->sym.len = sizeof(struct varnode);
	avar->sym.value.sym_var.varfrmt = n->sym.value.sym_var.varfrmt;
	avar->sym.value.sym_var.varfrml = n->sym.value.sym_var.varfrml;
	avar->sym.value.sym_var.varno = varnum;
	avar->sym.value.sym_var.attno = attnum;
#	ifdef xDTR1
	if (tTf(64, 3))
	{
		printf("makavar: node=%x  ", n);
		nodepr(avar);
	}
#	endif
	return(avar);
}
