# include	<ingres.h>
# include	<tree.h>
# include	<symbol.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)aggregate.c	7.2	3/5/81)




/*
**	AGGREGATE - replace aggregates with their values
**
**	Aggregate attempts to optimize aggregate processing
**	wherever possible. It replaces aggregates with their
**	values, and links aggregate functions which have
**	identical by-lists together.
**
**	Note that for the sake of this code, A "prime"
**	aggregate is one in which duplicates are removed.
**	These are COUNTU, SUMU, and AVGU.
**
**	Aggregate first forms a list of all aggregates in
**	the order they should be processed.
**
**	For each aggregate, it looks at all other aggregates
**	to see if there are two simple aggregates
**	or if there is another aggregate funtion with the
**	same by-list.
**
**	An attempt is made to run
**	as many aggregates as possible at once. This can be
**	done only if two or more aggregates have the same
**	qualifications and in the case of aggregate functions,
**	they must have identical by-lists.
**	Even then, certain combinations
**	of aggregates cannot occure together. The list is
**	itemized later in the code.
**
**	Aggregate calls BYEVAL or AGEVAL to actually process
**	aggregate functions or aggregates respectively.
**
**	Trace Flags:
**		40
*/

aggregate(root)
QTREE	*root;
{
	struct agglist	alist[MAXAGG + 1];
	QTREE		*rlist[MAXAGG + 1];
	struct agglist	*al, *al1;
	register QTREE	*agg, *aop1, *r;
	QTREE		*aop, *agg1;
	int		i, simple_agg, varmap;
	int		attcnt, anyagg, attoff, twidth;
	QTREE		*makavar(), *agspace();
	extern char	*rangename();
	extern QTREE	*ageval();
	extern QTREE	*byeval();

	al = alist;
	De.de_aggnext = al;
	De.de_agglim = &al[MAXAGG];

	findagg(&root, root);	/* generate list of all aggregates */
	De.de_aggnext->agpoint = 0;	/* mark end of the list */
	anyagg = 0;

	varmap = root->sym.value.sym_root.lvarm | root->sym.value.sym_root.rvarm;

	/* process each aggregate */
	for (;agg = al->agpoint; al++)
	{
		/* is this still an aggregate? */
		if (agg->sym.type != AGHEAD)
			continue;
		mapvar(agg, 0);	/* map the aggregate tree */
		anyagg++;

		De.de_sourcevar = bitpos(agg->sym.value.sym_root.lvarm | agg->sym.value.sym_root.rvarm);
#		ifdef xDTR1
		if (tTf(40, 4))
			printf("De.de_sourcevar=%d,rel=%s\n", De.de_sourcevar, rangename(De.de_sourcevar));
#		endif

		simple_agg = (agg->left->sym.type == AOP);	/* TRUE if no bylist */
		aop = agg->left;	/* assume it was TRUE */
#		ifdef xDTR1
		if (tTf(40, 0))
			printf("%s\n", simple_agg ? "agg" : "agg-func");
#		endif
		if (simple_agg)
		{
			/* simple aggregate */
			rlist[0] = agspace(aop);
			twidth = aop->sym.value.sym_op.opfrml & I1MASK;	/* init width to the width of the aggregate */
		}
		else
		{
			attoff = agg->left->left->sym.value.sym_resdom.resno + 2;
			aop = aop->right;	/* find the AOP node */
			/* assign  new source variable for aggregate */
			al->agvarno = getrange(&varmap);
			/* compute width of bylist + count field */
			twidth = findwid(agg->left) + 4;

			/* make sure the aggregate does not exceed max dimensions */
			if (chkwidth(aop, &twidth, attoff))
				derror(AGFTOBIG);
		}
		attcnt = 1;	/* one aggregate so far */

		/* look for another identical aggregate */
		for (al1 = al + 1; agg1 = al1->agpoint; al1++)
		{

			/* if agg is nested inside agg1 then ignore it */
			if (al->agfather == agg1 || agg1->sym.type != AGHEAD)
			{
				continue;
			}

			/* split aggs and agg-func apart */
			/* check for identical aggregate */
			if (simple_agg)
			{
				aop1 = agg1->left;	/* find AOP node */

				if (aop1->sym.type != AOP)
					continue;	/* not a simple agg */

				/* make sure they can be run together */
				if (checkagg(agg, aop, agg1, aop1) == 0) 
					continue;

				if ((i = sameagg(agg, aop1, attcnt)) >= 0)
				{
					/* found identical aggregate to rlist[i] */
					r = rlist[i];
				}
				else
				{
					/* put this agg in with the others */

					/* first make sure it won't exceed tuple length */
					if (chkwidth(aop1, &twidth, 0))
						continue;	/* can't be included */
					r = rlist[attcnt++] = agspace(aop1);

					/* connect into tree */
					aop1->left = agg->left;
					agg->left = aop1;
				}
			}
			else
			{
				/* aggregate function */
				if (!sameafcn(agg->left->left, agg1->left->left))
					continue;

				aop1 = agg1->left->right;	/* find AOP */


				if (checkagg(agg, aop, agg1, aop1) == 0)
				{
					/* same by-lists but they can't be run together */
					continue;
				}

				if ((i = sameagg(agg, aop1, attcnt)) < 0)
				{
					/* make sure there is room */
					if (chkwidth(aop1, &twidth, attcnt + attoff))
						continue;

					/* add aggregate into tree */
					i = attcnt++;

					aop1->left = agg->left->right;
					agg->left->right = aop1;
				}

				r = makavar(aop1, al->agvarno, i + attoff);
			}
			/* replace aggregate in original tree with its value */
			*(al1->father) = r;

			/* remove aggregate from local list */
			agg1->sym.type = -1;
#			ifdef xDTR1
			if (tTf(40, 3))
				printf("including aghead %x\n", agg1);
#			endif
		}

		/* process aggregate */
		if (simple_agg)
		{
			rlist[attcnt] = 0;
			ageval(agg, rlist);	/* pass tree and result list */
			r = rlist[0];
		}
		else
		{
			opt_bylist(alist, agg);
			byeval(al->agfather, agg, al->agvarno);
			r = makavar(aop, al->agvarno, attoff);
		}
		/*
		** Link result into tree. al->father hold the address
		** &(tree->left) or &(tree->right).
		*/
		*(al->father) = r;
#		ifdef xDTR1
		if (tTf(40, 4))
		{
			printf("agg value\n");
			treepr(*(al->father));
		}
#		endif
	}
	if (anyagg)
	{
		opt_bylist(alist, root);
		mapvar(root, 0);	/* remap main tree */
	}
}
/*
**	findagg builds a list of all aggregates
**	in the tree. It finds them by leftmost
**	innermost first.
**
**	The parameters represent:
**		nodep:	the address of the node pointing to you
**				eg. &(tree->left) or &(tree->right)
**		agf:	the root node. or if we are inside
**			a nested aggregate, the AGHEAD node
*/

findagg(nodep,  agf)
QTREE	**nodep;
QTREE	*agf;
{
	register QTREE		*q, *f;
	register struct agglist	*list;
	int			agg;

	if ((q = *nodep) == NULL)
		return;

	f = agf;
	if (agg = (q->sym.type == AGHEAD))
		f = q;	/* this aggregate is now the father root */

	/* find all aggregates below */
	findagg(&(q->left), f);
	findagg(&(q->right), f);

	/* if this is an aggregate, put it on the list */
	if (agg)
	{
		if (De.de_aggnext >= De.de_agglim)
			derror(TOOMANYAGGS);
		list = De.de_aggnext;
		list->father = nodep;
		list->agpoint = q;
		list->agfather = agf;
		De.de_aggnext++;
	}
}
/*
**	Agspace allocates space for the result of
**	a simple aggregate.
*/

QTREE *
agspace(aop)
QTREE	*aop;
{
	register QTREE	*a, *r;
	register int	length;
	extern char	*need();

	a = aop;
	length = a->sym.value.sym_op.opfrml & I1MASK;
	r = (QTREE *) need(De.de_qbuf, length + QT_HDR_SIZ);
	r->left = r->right = 0;
	r->sym.type = a->sym.value.sym_op.opfrmt;
	r->sym.len = length;

	return (r);
}
/*
** Chkwidth -- make sure that the inclusion of another aggregate will
**	not exceed the system limit. This means that the total width
**	cannot exceed MAXTUP and the number of domains cannot exceed MAXDOM-1
*/

chkwidth(aop, widthp, domno)
QTREE	*aop;
int	*widthp;
int	domno;
{
	register int	width;

	width = *widthp;

#	ifdef xDTR1
	if (tTf(40, 10))
		printf("agg width %d,dom %d\n", width, domno);
#	endif

	width += (aop->sym.value.sym_op.opfrml & I1MASK);

	if (width > MAXTUP || domno > MAXDOM - 1)
		return (1);

	*widthp = width;
	return (0);
}
/*
**	Determine whether an aggregate is prime
**	or a don't care aggregate. Returns TRUE
**	if COUNTU,SUMU,AVGU,MIN,MAX,ANY.
**	Returns false if COUNT,SUM,AVG.
*/

cprime(aop)
QTREE	*aop;
{
	register int	i;

	i = TRUE;
	switch (aop->sym.value.sym_op.opno)
	{
	  case opCOUNT:
	  case opSUM:
	  case opAVG:
		i = FALSE;
	}
	return (i);
}
/*
**	Getrange find a free slot in the range table
**	for an aggregate function.
**
**	If there are no free slots,derror is called
*/

getrange(varmap)
int	*varmap;
{
	register int	i, map, bit;

	map = *varmap;

	for (i = 0; i < MAXRANGE; i++)
	{
		/* if slot is used, continue */
		if ((bit = 1 << i) & map)
			continue;

		map |= bit;	/* "or" bit into the map */
		*varmap = map;

#		ifdef xDTR1
		if (tTf(40, 10))
			printf("Assn var %d, map %o\n", i, map);
#		endif

		return (i);
	}
	derror(NODESCAG);
	return  (-1);
}


checkagg(aghead1, aop1, aghead2, aop2)
QTREE	*aghead1;
QTREE	*aop1;
QTREE	*aghead2;
QTREE	*aop2;
{
	register QTREE	*aop_1, *aop_2, *agg1;
	int		ok;

	/* two aggregate functions can be run together
	** according to the following table:
	**
	**		prime	!prime	don't care
	**
	** prime	afcn?	never	afcn?
	** !prime	never	always	always
	** don't care	afcn?	always	always
	**
	** don't care includes: ANY, MIN, MAX
	** afcn? means only if a-fcn's are identical
	*/

	aop_1 = aop1;
	aop_2 = aop2;
	agg1 = aghead1;

	if (!prime(aop_1) && !prime(aop_2))
		ok = TRUE;
	else
		if (sameafcn(aop_1->right, aop_2->right))
			ok = cprime(aop_1) && cprime(aop_2);
		else
			ok = FALSE;
	/* The two aggregates must range over the same variables */
	if ((agg1->sym.value.sym_root.lvarm | agg1->sym.value.sym_root.rvarm) != (aghead2->sym.value.sym_root.lvarm | aghead2->sym.value.sym_root.rvarm))
		ok = FALSE;


	/* check the qualifications */
	if (ok)
		ok = sameafcn(agg1->right, aghead2->right);
	return (ok);
}


sameagg(aghead, newaop, agg_depth)
QTREE	*aghead;
QTREE	*newaop;
int	agg_depth;
{
	register QTREE	*agg, *newa;
	register int	i;

	agg = aghead;
	newa = newaop;
	agg = agg->left;
	if (agg->sym.type == BYHEAD)
		agg = agg->right;

	/* agg now points to first aggregate */
	for (i = 1; agg; agg = agg->left, i++)
		if (sameafcn(agg->right, newa->right) && agg->sym.value.sym_resdom.resno == newa->sym.value.sym_op.opno)
		{
#			ifdef xDTR1
			if (tTf(40, 6))
				printf("found identical aop %x\n", newa);
#			endif
			return (agg_depth - i);
		}

	/* no match */
	return (-1);
}




opt_bylist(alist, root)
struct agglist	*alist;
QTREE		*root;
{
	register struct agglist	*al;
	register QTREE		*agg;
	register struct hitlist	*hl;
	QTREE			**tpr, *tree, *lnodv[MAXDOM+2];
	struct hitlist		hlist[30];
	int			anyop, i, usedmap, vars, treemap;

	/* compute bitmap of all possible vars in tree (can include xtra vars) */
	treemap = root->sym.value.sym_root.lvarm | root->sym.value.sym_root.rvarm;
	anyop = FALSE;

	/* scan the list of aggregates looking for one nested in root */
	for (al = alist; (agg = al->agpoint) && agg != root; al++)
	{
		if (agg->sym.type == AGHEAD && agg->left->sym.type == BYHEAD &&
				al->agfather == root)
		{

			/* this aggregate function is nested in root */

			/* make sure it has some vars of interest */
			if ((treemap & varfind(agg->left->left, (QTREE *)NULL)) == 0)
				continue;

#			ifdef xDTR1
			if (tTf(40, 11))
			{
				printf("nested agg\n");
				treepr(agg);
			}
#			endif

			/* form list of bydomains */
			lnodv[lnode(agg->left->left, lnodv, 0)] = 0;
			usedmap = 0;

			De.de_hnext = &hlist[0];
			De.de_hlimit = &hlist[30];

			/* find all possible replacements */
			vars = modtree(&root, lnodv, &usedmap);

			/*
			** All references to a variable must be replaced
			** in order to use this aggregates by-domains.
			*/
			if (usedmap && ((vars & usedmap) == 0))
			{
#				ifdef xDTR1
				if (tTf(40, 7))
					printf("Committed\n");
#				endif
				/* success. Committ the tree changes */
				De.de_hnext->trepr = NULL;

				for (hl = &hlist[0]; tpr = hl->trepr; hl++)
				{
					/* get bydomain number */
					i = hl->byno;

					/* get node being replaced */
					tree = *tpr;

					/* if it is already a var, just change it */
					if (tree->sym.type == VAR)
					{
						tree->sym.value.sym_var.varno = al->agvarno;
						tree->sym.value.sym_var.attno = i + 2;
					}
					else
						*tpr = makavar(lnodv[i], al->agvarno, i + 2);

					anyop = TRUE;
#					ifdef xDTR1
					if (tTf(40, 7))
					{
						printf("modified tree\n");
						treepr(*tpr);
					}
#					endif
				}
			}
			/* vars is now a map of the variables in the root */
			treemap = vars;
		}
	}

	/* if any changes were made, get rid of the unnecessary links */
	if (anyop)
		chklink(root);
}




modtree(pnode, lnodv, replmap)
QTREE	**pnode;
QTREE	*lnodv[];
int	*replmap;
{
	register QTREE	*tree;
	register int	vars, i;
	QTREE		*afcn;

	/* point up current node */
	if ((tree = *pnode) == NULL)
		return (0);

	/* examine each by-list for match on this subtree */
	for (i = 0; afcn = lnodv[i]; i++)
	{
		if (sameafcn(tree, afcn->right))
		{
#			ifdef xDTR1
			if (tTf(40, 9))
			{
				printf("potential Jackpot");
				treepr(tree);
			}
#			endif
			vars = varfind(tree, (QTREE *)NULL);
			if (De.de_hnext == De.de_hlimit)
				return (vars);	/* no more room */

			/* record what needs to be done */
			De.de_hnext->trepr = pnode;
			De.de_hnext->byno = i;
			De.de_hnext++;
			*replmap |= vars;
			return (0);
		}
	}
	if (tree->sym.type == VAR)
		return (01 << tree->sym.value.sym_var.varno);

	/* try the subtrees */
	vars = modtree(&(tree->left), lnodv, replmap);
	if ((vars & *replmap) == 0)
		vars |= modtree(&(tree->right), lnodv, replmap);

	return (vars);
}


chklink(root)
QTREE	*root;
{
	register QTREE	*r, *b, *last;

	last = root;

	for (r = last->right; r->sym.type != QLEND; r = r->right)
	{
		/* if this is an EQ node then check for an unnecessary compare */
		if ((b = r->left)->sym.type == BOP && b->sym.value.sym_op.opno == opEQ)
		{
			if (sameafcn(b->left, b->right))
			{
#				ifdef xDTR1
				if (tTf(40, 5))
				{
					printf("unnec clause\n");
					treepr(b);
				}
#				endif
				last->right = r->right;
				continue;
			}
		}
		last = r;
	}
}



sameafcn(q1, q2)
QTREE *q1, *q2;
{

	register QTREE	*t1, *t2;
	register int	len;
	int		type;

	t1 = q1;
	t2 = q2;

	if (!(t1 && t2)) 
		return(!(t1 || t2));
	len = (t1->sym.len & 0377) + SYM_HDR_SIZ;
	type = t1->sym.type;
	if (type == VAR)
		len = sizeof(struct varnode);
	if (type == AND)
		len = 2;
	if (!bequal(&t1->sym.type, &t2->sym.type, len)) 
		return(0);
	return(sameafcn(t1->left,t2->left) && sameafcn(t1->right,t2->right));
}
/*
**	varfind -- find all variables in the tree pointed to by "root".
**		Examine all parts of the tree except aggregates. For
**		aggregates, ignore simple aggregate and look only
**		at the by-lists of aggregate functions. If the aggregate
**		is "aghead" then ignore it. There is no reason to look
**		at yourself!!!!
**		This routine is called by byeval() to determine
**		whether to link the aggregate to the root tree.
**
**	Curiosity note: since the tree being examined has not been
**	processed by decomp yet, ckvar does not need to be called
**	since the var could not have been altered.
*/

varfind(root, aghead)
QTREE	*root;
QTREE	*aghead;
{
	register QTREE	*tree;
	register int	type;

	if ((tree = root) == NULL)
		return (0);

	if ((type = tree->sym.type) == AGHEAD)
	{
		/* ignore if it matches aghead */
		if (tree == aghead)
			return (0);
		/* if this is an aggregate function, look at bylist */
		tree = tree->left;
		if ((type = tree->sym.type) != BYHEAD)
			return (0);
	}

	if (type == VAR)
		return (1 << tree->sym.value.sym_var.varno);

	return (varfind(tree->left, aghead) | varfind(tree->right, aghead));
}
