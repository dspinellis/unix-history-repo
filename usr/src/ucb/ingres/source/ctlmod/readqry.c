# include	"ctlmod.h"
# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	<sccs.h>

SCCSID(@(#)readqry.c	7.1	2/5/81)

# define	STACKFULL	3701	/* trbuild stack ovflo error */

/*
** READQRY
**
** 	Reads in query symbols from input pipe into core
**	locations and sets up information needed for later 
**	processing.
**
**	Returns ptr to root of querytree
**
**	Locbuf is a 'struct srcid' since that is the largest node of
**	a QMODE, SOURCEID, or RESULTVAR node.
*/

QTREE *
readqry(rdfn, fnparam, initialize)
int	(*rdfn)();	/* tree read function */
int	fnparam;	/* parameter to pass to rdfn */
bool	initialize;	/* if set, initialize Qbuf */
{
	register QTREE	*q;
	register QTREE	*rtval;
	register DESC	*d;
	extern QTREE	*readtree();
	extern char	*xalloc();
	extern QTREE	*trbuild();
	int		mark;
	extern QTREE	*readsym();
	int		i;
	int		j;

#	ifdef xCTR1
	if (tTf(10, 8))
		printf("READQRY:\n");
#	endif

	if (initialize)
	{
		/* initialize for new query block */
		clrrange();
		Qt.qt_resvar = -1;
		Qt.qt_qmode = -1;
	}
	for (i = 0; i < MAXRANGE; i++)
		Qt.qt_remap[i] = i;

	mark = markbuf(Qbuf);

	/* read symbols from input */
	for (;;)
	{
		freebuf(Qbuf, mark);
		q = readsym(rdfn, fnparam);
		switch (q->sym.type)
		{
		  case QMODE:
			if (Qt.qt_qmode != -1)
				syserr("readqry: two qmodes");
			Qt.qt_qmode = q->sym.value.sym_data.i2type;
			break;

		  case RESULTVAR:
			if (Qt.qt_resvar != -1)
				syserr("readqry: two resultvars");
			Qt.qt_resvar = q->sym.value.sym_data.i2type;
			break;

		  case SOURCEID:
			d = (DESC *) xalloc(sizeof *d);
			bmove((char *) &q->sym.value.sym_srcid.srcdesc, (char *) d, sizeof *d);
			i = q->sym.value.sym_srcid.srcvar;
			j = declare(i, d);
			if (j != i && initialize)
				syserr("readqry: declare(%d)=%d", i, j);
			Qt.qt_remap[i] = j;
			break;

		  case TREE:	/* beginning of tree, no more other stuff */
			q = readtree(q, rdfn, fnparam);
			rtval = trbuild(q);
			if (rtval == NULL)
				error(STACKFULL, 0);
			return (rtval);

		  default:
			syserr("readqry: bad symbol %d", q->sym.type);
		}
	}
}
/*
**  READSYM
**	reads in one symbol from pipe into symbol struct.
*/

QTREE *
readsym(rdfn, fnparam)
int	(*rdfn)();	/* tree read function */
char	*fnparam;
{
	register int	len;
	register int	t;
	register QTREE	*q;
	extern char	*need();
	int		rlen;

	q = (QTREE *) need(Qbuf, QT_HDR_SIZ);
	if ((*rdfn)(fnparam, &q->sym, TYP_LEN_SIZ) < TYP_LEN_SIZ) 
		syserr("readsym: read sym");
	rlen = len = q->sym.len & I1MASK;
	t = q->sym.type;

	switch (t)
	{
	  case AND:
		if (len < 6)
			len = 6;
		break;

	  case ROOT:
	  case AGHEAD:
		if (len < 8)
			len = 8;
		break;
	}

	q->sym.len = len;


	if (len != 0)
	{
		/* this will be contiguous with above need call */
		need(Qbuf, len);
	}
	if (rlen != 0)
	{
		if ((*rdfn)(fnparam, &q->sym.value, rlen) < rlen) 
			syserr("readsym: read val (sym=%d,%d)", t, rlen);
	}

	switch (t)
	{
	  case ROOT:
		q->sym.value.sym_root.rootuser = TRUE;
		break;
	  case AGHEAD:
		q->sym.value.sym_root.rootuser = FALSE;
		break;
	}
# ifdef xCTR1
	if (tTf(10, 9))
		nodepr(q);
# endif xCTR1

	return (q);
}
/*
**  READTREE
**
** 	reads in tree symbols into a buffer up to a root (end) symbol
**
*/

QTREE *
readtree(tresym, rdfn, fnparam)
QTREE	*tresym;
int	(*rdfn)();
char	*fnparam;
{
	register QTREE	*q;
	register QTREE	*rtval;
	extern char	*need();

	rtval = tresym;

	for(;;)
	{
		/* space for left & right pointers */
		q = readsym(rdfn, fnparam);
		if (q->sym.type == ROOT)
			return (rtval);
	}
}
/*
**  TRBUILD -- Rebuild a tree in memory
**
**	Trbuild is called with a pointer to the TREE node of
**	a query already in memory. It rebuilds the pointer
**	structure assuming the querytree is in postfix order.
**
**	Parameters:
**		bufptr - a pointer to the TREE node
**
**	Returns:
**		NULL - Internal stack overflow (STACKSIZ)
**		pointer to the ROOT node
**
**	Side Effects:
**		All left & right pointers are rebuilt.
**
**	Called By:
**		readqry
**
**	Syserrs:
**		syserr if there are too many leaf nodes or too
**		few child nodes
*/


QTREE *
trbuild(bufptr)
QTREE	*bufptr;
{
	register QTREE 	**stackptr;
	register char	*p;
	register SYMBOL	*s;
	QTREE		*treestack[STACKSIZ];
	extern bool	leaf();


	stackptr = treestack;

	for (p = (char *) bufptr;; p += QT_HDR_SIZ + ((s->len + 1) & 0376))
	{
		s = &((QTREE *)p)->sym;
		((QTREE *)p)->left = ((QTREE *)p)->right = 0;

		/* reunite p with left and right children on stack, if any*/
		if (!leaf((QTREE *) p))		/* this node has children */
		{
			if (s->type != UOP)
				if (stackptr <= treestack) 
				{
				err:
					syserr("trbuild:too few nodes");
				}
				else
					((QTREE *)p)->right = *(--stackptr);
			if (s->type != AOP)
				if (stackptr <= treestack) 
					goto err;
				else
					((QTREE *)p)->left = *(--stackptr);
		}

		/*
		** If this is a ROOT node then the tree is complete.
		** verify that there are no extra nodes in the
		** treestack.
		*/
		if (s->type == ROOT)	 	/* root node */
		{
			if (stackptr != treestack)
				syserr("trbuild:xtra nodes");
			return ((QTREE *)p);
		}

		/* stack p */
		if (stackptr-treestack >= STACKSIZ) 
			return (NULL);	/* error:stack full */
		*(stackptr++) = (QTREE *) p;

	}
}


bool
leaf(p)
QTREE *p;
{
	switch (p->sym.type)
	{
	  case VAR:
	  case TREE:
	  case QLEND:
	  case INT:
	  case FLOAT:
	  case CHAR:
	  case COP:
		return(TRUE);

	  default:
		return(FALSE);
	}
}
