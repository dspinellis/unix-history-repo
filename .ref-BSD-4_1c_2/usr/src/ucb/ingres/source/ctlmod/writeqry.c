# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	<sccs.h>

SCCSID(@(#)writeqry.c	7.1	2/5/81)


/*
**  WRITEQRY -- write a whole query
**
**	An entire query is written, including range table, and so
**	forth.
**
**	Parameters:
**		root -- the root of the tree to write.
**		wrfn -- the function to do the physical write.
**		fnparam -- a parameter to pass to wrfn.
**
**	Returns:
**		none
**
**	Side Effects:
**		none.
*/

writeqry(root, wrfn, fnparam)
QTREE	*root;
int	(*wrfn)();
char	*fnparam;
{
	register int	i;
	struct srcid	sid;

	/* write the query mode */
	if (Qt.qt_qmode >= 0)
		writesym(QMODE, 2, (char *) &Qt.qt_qmode, wrfn, fnparam);

	/* write the range table */
	for (i = 0; i < MAXRANGE; i++)
	{
		if (Qt.qt_rangev[i].rngvmark)
		{
			sid.srcvar = i;
			bmove((char *) Qt.qt_rangev[i].rngvdesc, (char *) &sid.srcdesc, sizeof sid.srcdesc);
			writesym(SOURCEID, sizeof sid, (char *) &sid, wrfn, fnparam);
		}
	}

	/* write a possible result variable */
	if (Qt.qt_resvar >= 0)
		writesym(RESULTVAR, 2, (char *) &Qt.qt_resvar, wrfn, fnparam);

	/* write the tree */
	writetree(root, wrfn, fnparam);
}
/*
**  WRITETREE -- write query tree
**
**	A query tree is written to the down pipe.  The parameter is
**	the root of the tree to be written.
**
**	Parameters:
**		q1 -- the root of the tree to write.
**		wrfn -- the function to call to do physical
**			writes.
**		fnparam -- a parameter to pass to wrfn.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
*/

writetree(q1, wrfn, fnparam)
QTREE	*q1;
int	(*wrfn)();
char	*fnparam;
{
	register QTREE	*q;
	register int	l;
	register char	t;

	q = q1;

	/* write the subtrees */
	if (q->left != NULL)
		writetree(q->left, wrfn, fnparam);
	if (q->right != NULL)
		writetree(q->right, wrfn, fnparam);

	/* write this node */
	t = q->sym.type;
	if (t == AND || t == ROOT || t == AGHEAD)
		q->sym.len = 0;
	l = q->sym.len & I1MASK;
	writesym(q->sym.type, l, (char *) &q->sym.value, wrfn, fnparam);
#	ifdef	xQTR1
	if (tTf(11, 8))
		nodepr(q);
#	endif
}
/*
**  WRITESYM -- write a symbol block
**
**	A single symbol entry of the is written.
**	a 'value' of zero will not be written.
*/

writesym(typ, len, value, wrfn, fnparam)
int	typ;
int	len;
char	*value;
int	(*wrfn)();
char	*fnparam;
{
	struct symbol	sym;
	register char	*v;
	register int	l;

	sym.type = typ & I1MASK;
	sym.len = l = len & I1MASK;
	(*wrfn)(&sym, 2, fnparam);
	v = value;
	if (v != NULL && l > 0)
		(*wrfn)(v, l, fnparam);
}
