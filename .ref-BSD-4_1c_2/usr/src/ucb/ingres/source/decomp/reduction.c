# include	<ingres.h>
# include	<symbol.h>
# include	<aux.h>
# include	<tree.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)reduction.c	7.1	2/5/81)

/*
**	Reduction -- This file contains routines related to the
**	reduction algorithm. The routines are all called by
**	decision().
**
**	Included are:
**
**		algorithm -- groups clauses according to common variables
**
**		buildlist -- build linked list of clauses from a query tree
**
**		construct -- build query trees from link lists of clauses
**
**		order     -- order a linked list of clauses into the prefered
**				execution sequence
*/
/*
**	Algorithm - determine whether query is connected or not
**
**	Algorithm takes a linked list of query components
**	and groups them according to the variables involved
**	in each component. If the query is fully interconnected
**	then algorithm returns TRUE else it returns FALSE.
**
**	By definition a constant clause (one involving zero variables)
**	is considered to use all variables. Without this rule,
**	a query with a null target list would always break into
**	two pieces.
**
**	Whether a query is fully connected is independent
**	of whether there are any one variable components
**	or not. This includes the target list. After applying
**	the algorithm, a scan is made to see how many multi-var
**	components are present. If there are two or more multi-var
**	components then the query is disconnected.
**
**	Trace Flags:
**		38
*/

algorithm(clist, varmap)
struct complist	*clist;
int		varmap;
{
	register struct complist	*chead, *cnext;
	register int			var;
	int				vmap;
	struct complist			*cprev, *cl;

	vmap = varmap;
	for (var = 1; vmap; var <<= 1)
	{
		if ((vmap & var) == 0)
			continue;
		vmap &= ~var;	/* remove var */

		/* done if query is already a single piece */
		if (clist->nextcomp == 0)
			break;

		/* find first component using variable var */
		for (chead = clist; chead; chead = chead->nextcomp)
		{
			if (chead->bitmap == 0 || (chead->bitmap & var))
			{
				/* this component uses variable var */

				cprev = chead;
		
				/* look for other components using variable var */
				for (cnext = chead->nextcomp; cnext; cnext = cnext->nextcomp)
				{
					if (cnext->bitmap == 0 || (cnext->bitmap & var))
					{
		
						/*
						** Found a component. Remove it from "next"
						** link and put it with head piece
						*/
		
						/* remove piece */
						cprev->nextcomp = cnext->nextcomp;
		
						/* fix up bit map */
						chead->bitmap |= cnext->bitmap;
		
						/* find end of head component */
						for (cl = chead; cl->linkcomp; cl = cl->linkcomp);
		
						/* connect with head piece */
						cl->linkcomp = cnext;
					}
					else
						cprev = cnext;
				}
			}
		}
	}

	/* return whether connected or not connected */
	for (var =0, chead = clist; chead; chead = chead->nextcomp)
		if (bitcnt(chead->bitmap) > 1)
			var++; /* this component involves 2 or more vars */

	return (var < 2); /* return TRUE if zero or one component multi-var */
}
/*
**	Buildlist -- Build a component list from a query tree.
**
**	Each ROOT and AND node is treated as a separate component
**	and a linked list is built connecting them. The ROOT node
**	MUST be the first element. This list will later be manipulated
**	by algorithm() to determine the structure of the query.
**
**	Returns:
**		Head of the list
*/

struct complist *
buildlist(root1, buf)
QTREE	*root1;
char	*buf;
{
	register struct complist	*head, *next;
	register QTREE			*root;
	struct complist			*ret;
	extern char			*need();

	ret = (struct complist *) need(buf, 0);
	head = 0;

	for (root = root1; root->sym.type != QLEND; root = root->right)
	{
		next = (struct complist *) need(buf, sizeof (*next));
		next->clause = root;
		next->nextcomp = next->linkcomp = 0;
		next->bitmap = root->sym.value.sym_root.lvarm;

		if (head)
			head->nextcomp = next;
		head = next;
	}
	return (ret);
}
/*
**  Construct -- construct a tree from a list component
**
**	Construct takes a list head and builds a querytree
**	from the components in the list. If the head component
**	is the ROOT of the original query, then
**	the original ROOT node is reused.
**
*/

QTREE *
construct(root, listhead, buf)
QTREE		*root;
struct complist	*listhead;
char		*buf;
{
	register QTREE			*ret, *q;
	register struct complist	*clist;
	extern QTREE			*makroot();

	clist = listhead;

	/* determine ROOT of tree */
	if (root == clist->clause)
	{
		q = root;
		clist = clist->linkcomp;
	}
	else
	{
		q = makroot(buf);
	}
	ret = q;
	for (; clist; clist = clist->linkcomp)
	{
		q->right = clist->clause;
		q = q->right;
	}

	q->right = De.de_qle;

	mapvar(ret, 1);
#	ifdef xDTR1
	if (tTf(38, 0))
	{
		printf("Construct\n");
		treepr(ret);
	}
#	endif
	return (ret);
}
/*
**  Order -- order a link list set of query components.
**
**	Takes a list of components and orders them:
**		first - disjoint components
**		second - reduction pieces
**		last - the original target list piece
**
**	Return:
**		new head of list
*/

struct complist *
order(clist, ovlapvar)
struct complist	*clist;
int		ovlapvar;
{
	register struct complist	*cl, *joint, *disjoint;
	struct complist			*xd, *xj, *tlpiece, *ret;
	QTREE				*tmp;
	int				map;

	tlpiece = clist;	/* target list piece always first */
	disjoint = joint = 0;
	map = ovlapvar >= 0 ? 1 << ovlapvar : 0;

	/* examine each irreducible component for disjointness */
	for (cl = tlpiece->nextcomp; cl; cl = cl->nextcomp)
	{
		if (cl->bitmap & map)
		{
			/* joint piece */
			if (joint == 0)
			{
				joint = cl;
				xj = cl;
			}
			else
			{
				joint->nextcomp = cl;
				joint = cl;
			}
		}
		else
		{
			/* disjoint piece */
			if (disjoint == 0)
			{
				disjoint = cl;
				xd = cl;
			}
			else
			{
				disjoint->nextcomp = cl;
				disjoint = cl;
			}
		}

	}
	/* we now have all disjoint, joint and tl pieces */
	/* order them in order (1) xd, (2) xj, (3) tlpiece */

	ret = tlpiece;
	tlpiece->nextcomp = 0;

	if (joint)
	{
		ret = xj;
		joint->nextcomp = tlpiece;
		if ((tlpiece->bitmap & (~map)) == 0)
		{
			/*
			** This is the special case of the target list
			** being one (or zero) variable and that variable
			** is the overlap variable. If left as is, the
			** reduction will take one step more than is
			** necessary. The target list piece is combined
			** with the last joint piece and processed together.
			**
			** An example of when this will happen is:
			** ret(p.a) : p.b = s.b ^ p.c = y.c
			**
			** Reduction would split this up into
			** (1) ret (p.a,p.b) : p.c = y.c
			** (2) ret (p.a) : p.b = s.b
			** (3) ret (p.a)
			**
			** Here we are allowing pieces (2) & (3) to be done together
			*/

			for (cl = joint; cl->linkcomp; cl = cl->linkcomp);

			cl->linkcomp = tlpiece;
			joint->nextcomp = 0;

			/* switch tl clause to top of complist */
			tmp = joint->clause;
			joint->clause = tlpiece->clause;
			tlpiece->clause = tmp;
		}
	}

	if (disjoint)
	{
		ret = xd;
		disjoint->nextcomp = joint ? xj : tlpiece;
	}

	return (ret);
}
