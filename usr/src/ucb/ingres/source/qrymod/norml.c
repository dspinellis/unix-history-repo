# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	<sccs.h>

SCCSID(@(#)norml.c	7.1	2/5/81)

extern QTREE	*treedup();
extern char	*need();


/*
** NORML
**	this routine passes the qualification clause portion of the query
**	tree to the routines for depressing nots and for conjunctive 
**	normalization.  It also calls a routine to place an and with one
**	null branch at the rightmost end of the tree.
*/

QTREE *
norml(ptr)
QTREE	*ptr;
{
	extern QTREE	*nnorm();
	extern QTREE	*travers();
	extern QTREE	*tree();

#	ifdef xQTR2
	if (tTf(73, 0))
		treepr(ptr, "->NORML");
#	endif

	if (ptr == NULL)
		return (tree(NULL, NULL, QLEND, 0, 0));

	/* push through the 'nots' as far a possible */
	ptr = nnorm(ptr);

	/* make tree into conjunctive normal form */
	ptr = travers(ptr);

	/* terminate the list on the rightmost branch */
	adjust(&ptr);

#	ifdef xQTR3
	if (tTf(73, 2))
		treepr(ptr, "before mvands");
#	endif

	/* make 'ands' into a linear list */
	mvands(ptr);

#	ifdef xQTR2
	if (tTf(73, 1))
		treepr(ptr, "<-NORML");
#	endif

	return (ptr);
}


/*
** NORM
**	this routine takes a tree which has nots only at the lower ends, and
**	places it into conjunctive normal form by repeatedly applying the
**	replacement rule: A or (B and C) ==> (A or B) and (A or C)
*/
QTREE *
norm(p1)
QTREE	*p1;
{
	extern QTREE	*tree();
	register QTREE	*p;
	register QTREE	*lptr;
	register QTREE	*rptr;

	p = p1;
	switch (p->sym.type)
	{
	  case AND:
		p->left = norm(p->left);
		p->right = norm(p->right);
		break;

	  case OR:
		if (p->right->sym.type == AND)
		{
		andright:
			/*
			** combine left subtree with each subtree of the
			** right subtree
			*/
			/*
			** use copy first so that the copy is guaranteed to be
			** the same as the original
			*/
			lptr = tree(treedup(p->left), p->right->left, OR, 0, 0);
			rptr = tree(p->left, p->right->right, OR, 0, 0);
			lptr = norm(lptr);
			rptr = norm(rptr);
			/* change node type to AND from OR */
			p->left = lptr;
			p->right = rptr;
			p->sym.type = AND;	/* length is same */
			break;
		}
		if (p->left->sym.type == AND)
		{
		andleft:
			/*
			** combine right subtree with each subtree of the
			** left subtree
			*/
			/*
			** again, the copy should be used first
			*/
			lptr = tree(p->left->left, treedup(p->right), OR, 0, 0);
			rptr = tree(p->left->right, p->right, OR, 0, 0);
			lptr = norm(lptr);
			rptr = norm(rptr);
			/* change node type to AND from OR */
			p->left = lptr;
			p->right = rptr;
			p->sym.type = AND;
			break;
		}
		/*
		** when TRAVERS is being used to optomize the normalization
		** order there should never be (I think) an OR as a child
		** of the OR in the parent.  Since TRAVERS works bottom up
		** in the tree the second OR level should be gone.
		*/
		if (p->right->sym.type == OR)
		{
			/* skip this (p->sym.type) "or" and normalize the right subtree */
			p->right = norm(p->right);

			/* now re-try the current node */
			if (p->right->sym.type == AND)
				goto andright;
			break;
		}
		if (p->left->sym.type == OR)
		{
			/* skip this "or" and normalize the left subtree */
			p->left = norm(p->left);

			/* now re-try the current node */
			if (p->left->sym.type == AND)
				goto andleft;
			break;
		}
		break;
	}
	return (p);
}

/*
** TRAVERS
**	traverse the tree so that conversion of ORs of ANDs can
**	take place at the innermost clauses first.  IE, normalize
**	before replication rather than after replication.
**
**	This routine need not be used.  The NORM routine will completely
**	traverse the tree and normalize it but...    TRAVERS finds
**	the lowest subtree to normalize first so that sub-trees can
**	be normalized before replication, hence reducing the time required
**	to normalize large trees.  It will also make OR-less trees faster
**	to normalize (since nothing need be done to it).
*/
QTREE *
travers(p1)
QTREE	*p1;
{
	register QTREE	*p;
	extern QTREE	*norm();

	p = p1;
	if (p->right != NULL)
		p->right = travers(p->right);
	if (p->left != NULL)
		p->left = travers(p->left);
	if (p->sym.type == OR)
		p = norm(p);
	return (p);
}
/*
** NNORM
**	this routine depresses nots in the query tree to the lowest possible
**	nodes.  It may also affect arithmetic operators in this procedure
*/
QTREE *
nnorm(p1)
QTREE	*p1;
{
	extern QTREE	*notpush();
	register QTREE	*p;

	p = p1;
	if (p->sym.type == AGHEAD)
	{
		/*
		** don't need to continue past an AGHEAD
		** actually, it causes problems if you do
		** because the qualification on the agg
		** has already been normalized and the
		** QLEND needs to stay put
		*/
		return (p);
	}
	if ((p->sym.type == UOP) && (p->sym.value.sym_op.opno == opNOT))
	{
		/* skip not node */
		p = p->right;
		p = notpush(p);
	}
	else
	{
		if (p->left != NULL)
			p->left = nnorm(p->left);
		if (p->right != NULL)
			p->right = nnorm(p->right);
	}
	return (p);
}

/*
** NOTPUSH
**	this routine decides what to do once a not has been found in the
**	query tree
*/
QTREE *
notpush(p1)
QTREE	*p1;
{
	extern QTREE	*nnorm();
	register QTREE	*p;

	p = p1;
	switch (p->sym.type)
	{
	  case AND:
		p->sym.type = OR;
		p->left = notpush(p->left);
		p->right = notpush(p->right);
		break;

	  case OR:
		p->sym.type = AND;
		p->left = notpush(p->left);
		p->right = notpush(p->right);
		break;

	  case BOP:
		switch (p->sym.value.sym_op.opno)
		{
		  case opEQ:
			p->sym.value.sym_op.opno = opNE;
			break;

		  case opNE:
			p->sym.value.sym_op.opno = opEQ;
			break;

		  case opLT:
			p->sym.value.sym_op.opno = opGE;
			break;

		  case opGE:
			p->sym.value.sym_op.opno = opLT;
			break;

		  case opLE:
			p->sym.value.sym_op.opno = opGT;
			break;

		  case opGT:
			p->sym.value.sym_op.opno = opLE;
			break;

		  default:
			syserr("strange BOP in notpush '%d'", p->sym.value.sym_op.opno);
		}
		break;

	  case UOP:
		if (p->sym.value.sym_op.opno == opNOT)
		{
			/* skip not node */
			p = p->right;
			p = nnorm(p);
		}
		else
			syserr("strange UOP in notpush '%d'", p->sym.value.sym_op.opno);
		break;

	  default:
		syserr("unrecognizable node type in notpush '%d'", p->sym.type);
	}
	return (p);
}

/*
** ADJUST
**	terminate qual with an AND and a QLEND at the rightmost branch
*/
adjust(pp)
QTREE	**pp;
{
	extern QTREE	*tree();
	register QTREE	*p;

	p = *pp;
	switch (p->sym.type)
	{
	  case AND: 
		adjust(&(p->right));
		break;

	  case OR:
	  default:
		*pp = tree(p, tree(NULL, NULL, QLEND, 0, NULL), AND, 0, 0);
		break;
	}
}

QTREE *
treedup(p)
register QTREE	*p;
{
	register QTREE	*np;
	extern char	Qbuf[];

	if (p == NULL)
		return (p);
	np = (QTREE *) need(Qbuf, (p->sym.len & I1MASK) + QT_HDR_SIZ);
	bmove(p, np, (p->sym.len & I1MASK) + QT_HDR_SIZ);
	np->left = treedup(p->left);
	np->right = treedup(p->right);
	return (np);
}

/*
**	MVANDS -- pushes all AND's in Qual into linear list
*/
mvands(andp)
QTREE	*andp;
{
	register QTREE	*ap, *lp, *rp;

	ap = andp;
	if (ap->sym.type == QLEND)
		return;
	rp = ap->right;
	lp = ap->left;
	if (lp->sym.type == AND)
	{
		ap->left = lp->right;
		ap->right = lp;
		lp->right = rp;
		mvands(ap);
	}
	else
		mvands(rp);
}
