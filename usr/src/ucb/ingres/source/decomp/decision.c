# include	<ingres.h>
# include	<symbol.h>
# include	<aux.h>
# include	<tree.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)decision.c	7.1	2/5/81)

/*
** DECISION -- This file contains code related to deciding how to
**	process the remaining query. The main routine is decision()
**	and the other routines are called only by decision. The routine
**	in this file include:
**
**	Decision -- Decides how to process a query.
**
**	Fixtl     -- Determines the target list for each component.
**
**	Fixresult -- Determines the result relation for the next query
**
**	Fixrange  -- Adjust the range table after a query
**
**	Fixovlap  -- Fixes trees in cases where reduction is used
**
**	Rmovlap   -- Restores trees in cases where reduction was used.
**
**	Listcomp  -- Debugging routine.
*/
/*
**	Decision is given a subquery and decides how to process it.
**	The choices are:
**		Disjoint pieces -- The original query had disjoint components.
**					Do each component separately.
**		Reduction       -- The query is joined by a single variable.
**					Reduce the query on that joining variable
**					and do each component separately.
**		Substitution    -- The query is neither disjoint nor reducible.
**					Process by tuple substitution.
**
**	Notice that decision() is recursive and will call itself on each
**	subcomponent it decides to do. Decision calls various support
**	routines in the file "reduction.c".
*/

decision(root, qmode, result_num, buf)
QTREE	*root;
int	qmode;
int	result_num;
char	*buf;
{
	register QTREE		**qp;
	register struct complist *clist;
	register int		ovlapvar;
	struct complist		*cp;
	int			i, onepiece, qtrue, map;
	int			mark, mark1, cand_map;
	QTREE			*tree, *newtree;
	QTREE			*qlist[MAXRANGE];
	int			newqmode;
	int			ovlaprelnum, newr;
	int			locrange[MAXRANGE];
	extern QTREE		*copy_ands();
	extern struct complist	*buildlist(), *order();
	extern QTREE		*construct();

#	ifdef xDTR1
	if (tTf(37, -1))
	{
		printf("DECISION root=%x,vc=%d,res=%d\n", root, root->sym.value.sym_root.tvarc, result_num);
	}
#	endif
	mark = markbuf(buf);
	if (root->sym.value.sym_root.tvarc < 3)
	{
		/* setup to do as one single piece */
		onepiece = TRUE;
	}
	else
	{
		/* break the query apart if possible */

		/* build component list */
		clist = buildlist(root, buf);
#		ifdef xDTR1
		if (tTf(37, 2))
			listcomp(clist, "Original Comp");
#		endif

		/* is query completely connected or disjoint */
		map = root->sym.value.sym_root.lvarm | root->sym.value.sym_root.rvarm;
		onepiece = algorithm(clist, map);
#		ifdef xDTR1
		if (tTf(37, 1))
			printf("Orig query %s\n", onepiece ? "connected" : "disjoint");
#		endif
		/* assume there is no joining variable */
		ovlapvar = -1;

		if (onepiece)
		{
			/*
			** Try to reduce a single connected piece.
			** In turn each variable will be logically
			** removed from the query and a test will
			** be made to see if the query is still
			** connected.
			*/

			cand_map = map;
			for (i = 1; cand_map; i <<= 1)
			{
				if ((cand_map & i) == 0)
					continue;
				cand_map &= ~i;
				freebuf(buf, mark);
				clist = buildlist(root, buf);
				if (algorithm(clist, map & ~i) == 0)
				{
					ovlapvar = bitpos(i);
					ovlaprelnum = De.de_rangev[ovlapvar].relnum;
					onepiece = FALSE;
					break;
				}
			}
#			ifdef xDTR1
			if (tTf(37, 1))
			{
				if (ovlapvar < 0)
					printf("Query not reducible\n");
				else
				{
					printf("Query Reducible on %d\n", ovlapvar);
					if (tTf(37, 3))
						listcomp(clist, "After reduct");
				}
			}
#			endif
		}

		/*
		** If query is more than one piece, build trees
		** for each piece.
		*/

		if (!onepiece)
		{
			/* order pieces */
			clist = order(clist, ovlapvar);
#			ifdef xDTR1
			if (tTf(37, 4))
				listcomp(clist, "After ordering");
#			endif
			cp = clist;
			qp = qlist;
			do
			{
				*qp++ = construct(root, cp, buf);
			} while (cp = cp->nextcomp);
			*qp++ = 0;
		}
	}

	/*
	** The query is now either the one original piece or
	** is in multiple pieces. The information in clist
	** has been thrown away and now the ordered pieces
	** will be processed.
	*/

	if (onepiece)
	{
		freebuf(buf, mark);
		qtrue = decompy(root, qmode, result_num, buf);
	}
	else
	{
		/* the query is in pieces. prepare to process */

		newquery(locrange);	/* save state of range table */

		/* determine the target list for each piece of the query */
		for (qp = qlist; tree = *qp++; )
			fixtl(tree, qp, ovlapvar, buf);

		/* adjust refs to ovlapvar since domain #'s could have changed */
		fixovlap(qlist, ovlapvar, buf);


		/* now process each component */
		mark1 = markbuf(buf);
		for (qp = qlist; tree = *qp++; )
		{

#			ifdef xDTR1
			if (tTf(37, 6))
			{
				printf("next piece\n");
				treepr(tree);
			}
#			endif

			/* determine result relation name */
			newr = fixresult(root, tree, &newqmode, qmode, result_num);
	
			/*
			** Run the query. If reduction is being
			** performed, the actual tree given to
			** the decision routine must be a copy
			** to protect from the recursive call changing
			** the tree. Any work done at this level,
			** must be to the original tree
			*/
			newtree = tree;
			if (ovlapvar != -1)
			{
				freebuf(buf, mark1);
				newtree = copy_ands(tree, buf);
			}
			qtrue = decision(newtree, newqmode, newr, buf);
	
			/* fix up the range table */
			fixrange(root, tree, ovlapvar, ovlaprelnum, newr);
	
			/* if last piece was false then done */
			if (!qtrue)
				break;
	
		}

		/* restore the trees */
		rmovlap(qlist, ovlapvar);

		/* restore range table back to original */
		endquery(locrange, TRUE);	/* reopen previous range */

		/* return any buffer space used */
		freebuf(buf, mark);
	}

	/* all done with query */
	return (qtrue);
}
/*
** Fixresult -- Determine result relation for "tree" query.
**	If "tree" is the original target list piece then use the
**		original relation and mode.
**	If "tree" is a reduction piece then create a temporary relation
**		for it.
**	If "tree" is a disjoint piece then there is no target list nor
**		result relation.
**
**	Return:
**		result relation number
**	Side Effects:
**		*newmode is set to the query mode of the next piece
*/

fixresult(root, tree1, newmode, origmode, resnum)
QTREE	*root;
QTREE	*tree1;
int	*newmode;
int	origmode;
int	resnum;
{
	register QTREE	*tree;
	register int	newresult;

	tree = tree1;
	*newmode = mdRETR;
	if (tree->left->sym.type != TREE)
	{
		if (tree != root)
		{
			/* make new result for reduction step */
			newresult = mak_t_rel(tree, "r", -1);
		}
		else
		{
			/* final piece with original result and mode */
			newresult = resnum;
			*newmode = origmode;
		}
	}
	else
		newresult = NORESULT;
	return (newresult);
}
/*
**	Update range table after a reduction has been processed.
**	Only the intermediate reductions will affect the range
**	table. The last piece does not.
*/

fixrange(root, tree, ovlapvar, reductnum, newrelnum)
QTREE	*root;
QTREE	*tree;
int	ovlapvar;
int	reductnum;
int	newrelnum;
{
	register int	old;
	register int	i;
	extern DESC	*openr1();

	if (root != tree)
	{
		/* this is an intermediate piece */
		i = ovlapvar;

		if (newrelnum >= 0)
		{
			old = new_range(i, newrelnum);
			if (old != reductnum)
				dstr_rel(old);
			openr1(i);
		}
	}
}
/*
** Fixovlap -- Adjust subsequent trees which reference the reduction var
**
**	If the first query in list redefines the reduction variable
**	(if any) then each subsequent query which references the
**	reduction variable is adjusted.
**	Since there may be multiple pieces,
**	subsequence redefinitions are done without
**	reallocating buffer space.
**
*/

fixovlap(qlist, ovlapvar, buf)
QTREE	*qlist[];
int	ovlapvar;
char	*buf;
{
	register QTREE	**qp, *piece, **qp1;
	QTREE		*next;
	int		ovmap;
	extern QTREE	**mksqlist();

	ovmap = 1 << ovlapvar;

	/* for each piece, if it redefines ovlapvar, then fix up rest */
	for (qp = qlist; piece = *qp++; )
	{
		if (piece->sym.value.sym_root.lvarm & ovmap)
		{
			for (qp1 = qp; next = *qp1++; )
			{
				if ((next->sym.value.sym_root.lvarm | next->sym.value.sym_root.rvarm) & ovmap)
				{
					tempvar(next, mksqlist(piece, ovlapvar), buf);
					buf = NULL;	/* do not allocate on subsequent refs */
				}
			}
		}
	}
}
/*
** Rmovlap -- Restore query trees back to their original state.
**
**	Rmovlap undoes the effect of fixovlap(). Any references
**	to the reduction variable which were adjusted are now
**	reverted back to the original reference. Note that the
**	first piece is not effected by fixovlap.
*/

rmovlap(qlist, ovlapvar)
QTREE	*qlist[];
int	ovlapvar;
{
	register QTREE	**qp, *tree;
	int		ovmap;

	if (ovmap = (1 << ovlapvar))
	{
		/* for each piece, remove any tempvars */
		for (qp = &qlist[1]; tree = *qp++; )
		{
			origvar(tree, mksqlist(tree, ovlapvar));
		}
	}
}
/*
** Fixtl -- Determine what the target list of each tree should be.
**
**	Fixtl takes each query which references the reduction variable
**	and looks for references to that variable in subsequent queries.
**	Dfind will build a target list which includes every domain
**	which will later be referenced.
*/

fixtl(tree1, qp1, ovlapvar, buf)
QTREE	*tree1;
QTREE	*qp1[];
int	ovlapvar;
char	*buf;
{
	register QTREE	*tree, **qp, *next;
	int		var, map;
	extern QTREE	**mksqlist();

	tree = tree1;
	var = ovlapvar;
	map = 1 << var;

	/*
	** if the last tree referenced the overlap variable then
	** try to fix next tree
	*/
	if (tree->sym.value.sym_root.rvarm & map)
	{
		qp = qp1;
		while (next = *qp++)
		{
			if ((next->sym.value.sym_root.lvarm | next->sym.value.sym_root.rvarm) & map)
				dfind(next, buf, mksqlist(tree, var));
		}
	}
}



# ifdef	xDTR1

/*
**	This is strictly a debuggin routine used for
**	printing component lists.
*/

listcomp(list, mesg)
struct complist	*list;
char		*mesg;
{
	register struct complist	*c, *cl;
	register int			i;

	if (tTf(37, 14))
	{
		printf("%s\n", mesg);
	    i = -1;

	    for (cl = list; cl; cl = cl->nextcomp)
	    {
		    i++;
		    printf("Component %d:map=%o\n", i, cl->bitmap);
		    for (c = cl; c; c = c->linkcomp)
		    {
			    printf("%x, ", c->clause);
			    if (tTf(37, 15))
			    {
				    treepr(c->clause->left);
				    nodepr(c->clause);
			    }
		    }
		    printf("\n");
	    }
    }
}
# endif
