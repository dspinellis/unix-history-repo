# include	<ingres.h>
# include	<tree.h>
# include	<symbol.h>
# include	<pv.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)byeval.c	7.1	2/5/81)

/*
**	BYEVAL - process aggregate function
**
**	Byeval is passed the root of the original query
**	tree and the root of the aggregate function to
**	be processed.
**
**	It first creates a temporary relation which will
**	hold the aggregate result. The format of the relation
**	is:
**	_SYSxxxxxaa(count, by-dom1, ... , by-domn, ag1, ... , agm)
**
**	The relation is moved into the range table and will become
**	a part of the query.
**
**	If there are any occurences of the variables
**	from the by-domains, anywhere in the original query tree,
**	the aggregate relation is linked on all by-domains in the
**	original query tree.
**
**	If the aggregate is unique, multivariable, or has a
**	qualification, then special processing is done.
**
**	If the aggregate is qualified then the by-domains are
**	projected into the result relation. This guarantees that
**	every value of the by-domains will be represented in the
**	aggregate result.
**
**	If the aggregate is unique or multivariable, then another
**	temporary relation is created and the values which will be
**	aggregated; along with the by-domains, are retrieved into
**	the temporary relation.
**
**	If unique, then duplicates are removed from the temporary relation.
**
**	Next the result relation for the aggregate is modified
**	to hash in order to speed up the processing of the aggregate
**	and guarantee that there are no duplicates in the bylist.
**
**	The aggregate is then run, and if a temporary relation was
**	created (eg. unique or multivar aggregate) then it is destroyed.
**
**	Trace Flags:
**		42
*/


QTREE *
byeval(root, aghead, agvar)
QTREE	*root;		/* root of orig query */
QTREE	*aghead;	/* root of ag fcn sub-tree */
int	agvar;		/* variable number assigned to this aggregate */
{

	register QTREE 	*q, *ag, *resdom;
	QTREE		*r;
	int		temp_relnum, i, filled;
	QTREE		*lnodv[MAXDOM+2], *save_node[MAXDOM+2];
	char		agbuf[AGBUFSIZ];
	char		nums[2];
	int		relnum;
	QTREE 		*byhead, **alnp;
	int		bydoms, bymap, primeag, srcmap;
	extern int	derror();
	extern QTREE	*makroot(), *makavar(), *makresdom(), *copytree();
	extern char	*rnum_convert();

#	ifdef xDTR1
	if (tTf(42, -1))
		printf("BYEVAL\n");
#	endif

	ag = aghead;
	byhead = ag->left;

	/* first create the aggregate result relation */
	/* params for create */

	initp();	/* init globals for setp */
	setp(PV_STR,"0");	/* initial relstat field */
	relnum = rnum_alloc();
	setp(PV_STR,rnum_convert(relnum));
	setp(PV_STR,"count");	/* domain 1 - count field per BY value */
	setp(PV_STR,"i4");	/* format of count field */

	i = bydoms = lnode(byhead->left, lnodv, 0);
	lnodv[i] = 0;
	alnp = &lnodv[++i];
	i = lnode(byhead->right, lnodv, i);
	lnodv[i] = 0;

	domnam(lnodv, "by");	/* BY list domains */
	domnam(alnp, "ag");	/* aggregate value domains */

	call_dbu(mdCREATE, FALSE);

	De.de_rangev[agvar].relnum = relnum;
#	ifdef xDTR1
	if (tTf(42, 7))
		printf("agvar=%d,rel=%s\n", agvar, rnum_convert(relnum));
#	endif

	bymap = varfind(byhead->left, (QTREE *)NULL);

	/*
	** Find all variables in the tree in which you are nested.
	** Do not look at any other aggregates in the tree. Just in
	** case the root is an aggregate, explicitly look at its
	** two descendents.
	*/
	srcmap = varfind(root->left, ag) | varfind(root->right, ag);
#	ifdef xDTR1
	if (tTf(42, 8))
		printf("bymap=%o,srcmap=%o\n", bymap, srcmap);
#	endif

	if (bymap & srcmap)
		modqual(root, lnodv, srcmap, agvar);

	/* if aggregate is unique or there is a qualification
	** or aggregate is multi-var, then special processing is done */

	temp_relnum = NORESULT;
	filled = FALSE;
	primeag = prime(byhead->right);
	if (ag->right->sym.type != QLEND || ag->sym.value.sym_root.tvarc > 1 || primeag)
	{
		/* init a buffer for new tree components */
		initbuf(agbuf, AGBUFSIZ, AGBUFFULL, derror);

		/* make a root for a new tree */
		q = makroot(agbuf);

		/*
		** Create a RESDOM for each by-domain in the original
		** aggregate. Rather than using the existing by-domain
		** function, a copy is used instead. This is necessary
		** since that subtree might be needed later (if modqual())
		** decided to use it. Decomp does not restore the trees
		** it uses and thus the by-domains might be altered.
		*/
		for (i = 0; r = lnodv[i]; i++)
		{
			resdom = makresdom(agbuf, r);
			resdom->sym.value.sym_resdom.resno = i + 2;
			resdom->right = copytree(r->right, agbuf);
			resdom->left = q->left;
			q->left = resdom;
		}
		mapvar(q, 0);	/* make maps on root */
#		ifdef xDTR1
		if (tTf(42, 2))
		{
			printf("byedomains\n");
			treepr(q);
		}
#		endif

		/* if agg is qualified, project by-domains into result */
		if (ag->right->sym.type != QLEND)
		{
			filled = TRUE;
			i = De.de_sourcevar;	/* save value */
			decomp(q, mdRETR, relnum);
			De.de_sourcevar = i;	/* restore value */
		}

		/* if agg is prime or multivar, compute into temp rel */
		if (ag->sym.value.sym_root.tvarc > 1 || primeag)
		{
			q->right = ag->right;	/* give q the qualification */
			ag->right = De.de_qle;	/* remove qualification from ag */

			/* put aop resdoms on tree */
			for (i = bydoms + 1; r = lnodv[i]; i++)
			{
				resdom = makresdom(agbuf, r);
				resdom->right = r->right;
				resdom->left = q->left;
				q->left = resdom;

				/* make aop refer to temp relation */
				r->right = makavar(resdom, FREEVAR, i);
			}

			/* assign result domain numbers */
			for (resdom = q->left; resdom->sym.type != TREE; resdom = resdom->left)
				resdom->sym.value.sym_resdom.resno = --i;

			/*
			** change by-list in agg to reference new source rel.
			** Save the old bylist to be restored at the end of
			** this aggregate.
			*/
			for (i = 0; resdom = lnodv[i]; i++)
			{
				save_node[i] = resdom->right;
				resdom->right = makavar(resdom, FREEVAR, i + 1);
			}

			mapvar(q, 0);
#			ifdef xDTR1
			if (tTf(42, 3))
			{
				printf("new ag src\n");
				treepr(q);
			}
#			endif

			/* create temp relation */
			temp_relnum = mak_t_rel(q, "a", -1);
			decomp(q, mdRETR, temp_relnum);
			De.de_rangev[FREEVAR].relnum = temp_relnum;
			De.de_sourcevar = FREEVAR;
			if (primeag)
				removedups(FREEVAR);
#			ifdef xDTR1
			if (tTf(42, 4))
			{
				printf("new agg\n");
				treepr(ag);
			}
#			endif
		}
	}

	/* set up parameters for modify to hash */
	initp();
	setp(PV_STR, rnum_convert(relnum));
	setp(PV_STR, "hash");	/* modify the empty rel to hash */
	setp(PV_STR, "num");	/* code to indicate numeric domain names */
	nums[1] = '\0';
	for (i = 0; i < bydoms; i++)
	{
		nums[0] = i + 2;
		setp(PV_STR, nums);
	}
	setp(PV_STR, "");

	/* set up fill factor information */
	setp(PV_STR,"minpages");
	if (filled)
	{
		setp(PV_STR,"1");
		setp(PV_STR,"fillfactor");
		setp(PV_STR,"100");
	}
	else
	{
		setp(PV_STR,"10");
	}
	specclose(relnum);
	call_dbu(mdMODIFY, FALSE);


	De.de_newq = 1;
	De.de_newr = TRUE;
	call_ovqp(ag, mdRETR, relnum);

	De.de_newq = 0;
	/* if temp relation was used, destroy it */
	if (temp_relnum != NORESULT)
	{
		for (i = 0; resdom = lnodv[i]; i++)
			resdom->right = save_node[i];
		dstr_rel(temp_relnum);
	}
}




modqual(root, lnodv, srcmap, agvar)
QTREE	*root;
QTREE	*lnodv[];
int	srcmap;
int	agvar;
{
	register QTREE	*and_eq, *afcn;
	register int	i;
	extern QTREE	*copytree();
	extern char	*need();
	register int	len;

#	ifdef xDTR1
	if (tTf(42, 12))
		printf("modqual %o\n", srcmap);
#	endif

	for (i = 0; afcn = lnodv[i]; i++)
	{
		/*  `AND' node  */
		len = sizeof (struct rootnode) - sizeof (short);
		and_eq = (QTREE *) need(De.de_qbuf, QT_HDR_SIZ + len);
		and_eq->sym.type = AND;
		and_eq->sym.len = len;
		and_eq->sym.value.sym_root.tvarc = 0;
		and_eq->sym.value.sym_root.lvarc = 0;
		and_eq->sym.value.sym_root.lvarm = 0;
		and_eq->sym.value.sym_root.rvarm = 0;
		and_eq->right = root->right;
		root->right = and_eq;

		/* `EQ' node  */
		len = sizeof (struct opnode);
		and_eq->left = (QTREE *) need(De.de_qbuf, QT_HDR_SIZ + len);
		and_eq = and_eq->left;
		and_eq->sym.type = BOP;
		and_eq->sym.len = len;
		and_eq->sym.value.sym_op.opno = opEQ;

		/* bydomain opEQ var */
		and_eq->right = copytree(afcn->right, De.de_qbuf);	/* a-fcn (in Source) specifying BY domain */
		and_eq->left = makavar(afcn, agvar, i+2);	/* VAR ref BY domain */
	}
}
