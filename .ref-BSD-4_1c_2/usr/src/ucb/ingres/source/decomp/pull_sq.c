# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)pull_sq.c	7.1	2/5/81)

/*
** DECOMP1.C
**
**	contains routines associated with setting up 
**	detachable 1-variable sub-queries.
**	ptrs to these sq's are kept in the 
**	array 'sqlist' declared in the main decomp routine.
**
**	Trace Flags:
**		34
*/


pull_sq(tree1, sqlist, locrang, sqrange, buf)
QTREE	*tree1;
QTREE	*sqlist[];
int	locrang[];
int	sqrange[];
char	*buf;
{
	register QTREE 	*q, *tree, *r;
	QTREE 		*s;
	int 		anysq, j, badvar;
	extern QTREE	*makroot();

	tree = tree1;

#	ifdef xDTR1
	if (tTf(34, 0))
		printf("PULL_SQ:tree=%x\n", tree);
#	endif

	anysq = 0;
	for (j = 0; j < MAXRANGE; j++)
		sqlist[j] = 0;

	if (tree->sym.value.sym_root.tvarc == 1)
		return;

	/* detach all one variable clauses except:
	** if the target list is one variable and
	** that variable is disjoint from the other
	** variables, then don't pull it.
	**
	** It will be more efficient to process it
	** all at once in decompy
	*/

	badvar = 0;
	if (tree->sym.value.sym_root.lvarc == 1)
	{
		badvar = tree->sym.value.sym_root.lvarm;	/* get bit position of var */

		/* look for a two variable clause involving badvar */
		for (r = tree->right; r->sym.type != QLEND; r = r->right)
		{
			if (r->sym.value.sym_root.lvarc > 1 && (r->sym.value.sym_root.lvarm & badvar))
			{
				badvar = 0;
				break;
			}
		}
	}
#	ifdef xDTR1
	if (tTf(34, 2))
		printf("Detachable clauses: (badvar=%o)\n", badvar);
#	endif
	for (r=tree; r->right->sym.type!=QLEND; )
	{
#		ifdef xDTR1
		if (tTf(34, 3))
			nodepr(r);
#		endif
		q = r;
		r = r->right;
		if (r->sym.value.sym_root.lvarc == 1)
		{
			j = bitpos(r->sym.value.sym_root.lvarm);
#			ifdef xDTR1
			if (tTf(34, 4))
			{
				printf("\nvar=%d, clause\n", j);
				treepr(r->left);
			}
#			endif
			if (r->sym.value.sym_root.lvarm == badvar)
			{
#				ifdef xDTR1
				if (tTf(34, 5))
					printf("not detaching \n");
#				endif
				continue;
			}
			anysq++;

			if (!sqlist[j])		/* MAKE ROOT NODE FOR SUBQUERY */
				sqlist[j] = makroot(buf);
			s = sqlist[j];

			/* MODIFY MAIN QUERY */

			q->right = r->right;

			/* MODIFY `AND` NODE OF DETACHED CLAUSE */

			r->right = s->right;
			r->sym.value.sym_root.rvarm = s->sym.value.sym_root.rvarm;
			r->sym.value.sym_root.tvarc = 1;

			/* ADD CLAUSE TO SUB-QUERY */

			s->right = r;
			s->sym.value.sym_root.rvarm = r->sym.value.sym_root.lvarm;
			s->sym.value.sym_root.tvarc = 1;

#			ifdef xDTR1
			if (tTf(34, 6))
			{
				printf("SQ\n");
				treepr(s);
			}
#			endif

			r = q;
		}
	}

	/* NOW SET UP TARGET LIST FOR EACH SUBQUERY IN SQLIST */

#	ifdef xDTR1
	if (tTf(34, 7))
		printf("# sq clauses=%d\n", anysq);
#	endif
	if (anysq)
	{
#		ifdef xDTR1
		if (tTf(34, 8))
			printf("Dfind--\n");
#		endif
		dfind(tree, buf, sqlist);
		mapvar(tree, 1);

		/* create the result relations */
		for (j = 0; j < MAXRANGE; j++)
		{
			if (q = sqlist[j])
			{
				if (q->left->sym.type != TREE)
				{
					savrang(locrang, j);
					sqrange[j] = mak_t_rel(q, "d", -1);
				}
				else
					sqrange[j] = NORESULT;
			}
		}
	}
}
/*
**  DFIND
*/
dfind(tree, buf, sqlist)
register QTREE	*tree;
char		*buf;
QTREE		*sqlist[];
{
	register char	varno;
	register QTREE	*sq;
	extern QTREE	*ckvar();

	if (tree == NULL) 
		return;
#	ifdef xDTR1
	if (tTf(34, 9))
		nodepr(tree);
#	endif
	if (tree->sym.type == VAR)
	{
		tree = ckvar(tree);
		varno = tree->sym.value.sym_var.varno;
		if (sq = sqlist[varno])
			maktl(tree, buf, sq, varno);
		return;
	}

	/* IF CURRENT NODE NOT A `VAR` WITH SQ, RECURSE THRU REST OF TREE */

	dfind(tree->left, buf, sqlist);
	dfind(tree->right, buf, sqlist);
	return;
}
/*
**  MAKTL
*/

maktl(node, buf, sq1, varno)
QTREE	*node;
char	*buf;
QTREE	*sq1;
int	varno;
{
	register QTREE 	*resdom, *tree, *sq;
	int		domno, map;
	extern QTREE	*makresdom();
	extern QTREE	*copytree();

	sq = sq1;
	domno = node->sym.value.sym_var.attno;

#	ifdef xDTR1
	if (tTf(34, 12))
		printf("\tVar=%d,Dom=%d ", varno, domno);
#	endif
	/* CHECK IF NODE ALREADY CREATED FOR THIS DOMAIN */

	for (tree = sq->left; tree->sym.type != TREE; tree = tree->left)
		if (tree->right->sym.value.sym_var.attno == domno)
		{
#			ifdef xDTR1
			if (tTf(34, 13))
				printf("Domain found\n");
#			endif
			return;
		}

	/* create a new resdom for domain */

	resdom = makresdom(buf, node);
	resdom->sym.value.sym_resdom.resno = sq->left->sym.type == TREE? 1:
					sq->left->sym.value.sym_resdom.resno + 1;
	/* resdom->right is a copy of the var node in order to
	** protect against tempvar() changing the var node.
	*/
	resdom->left = sq->left;
	resdom->right = copytree(node, buf);


	/* update ROOT node if necessary */

	sq->left = resdom;
	map = 1 << varno;
	if (!(sq->sym.value.sym_root.lvarm & map))
	{
		/* var not currently in tl */
		sq->sym.value.sym_root.lvarm |= map;
		sq->sym.value.sym_root.lvarc++;

		/* if var is not in qualification then update total count */
		if (!(sq->sym.value.sym_root.rvarm & map))
			sq->sym.value.sym_root.tvarc++;
#		ifdef xDTR1
		if (tTf(34, 15))
		{
			printf("new root ");
			nodepr(sq);
		}
#		endif
	}

#	ifdef xDTR1
	if (tTf(34, 14))
	{
		printf("new dom ");
		nodepr(resdom);
	}
#	endif
	return;
}
