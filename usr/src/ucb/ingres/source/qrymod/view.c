# include	<ingres.h>
# include	<aux.h>
# include	<symbol.h>
# include	<tree.h>
# include	"qrymod.h"
# include	<sccs.h>

SCCSID(@(#)view.c	7.1	2/5/81)

/*
**  VIEW.C -- view processing
**
**	This module does the view processing.  Basically, it operates
**	by detecting all references to views and replacing them by
**	references to real relations.  There are a number of cases
**	when it cannot do this, to whit:
**
**	Syntactic problems:  the view may have a domain defined as
**	a non-simple value (that is, not a simple attribute), which
**	is then required to take on a value.  For example, if the
**	view is defined as
**		range of x is baserel
**		define v (d = x.a / 3)
**	and then referenced as
**		append to v (d = 7)
**	would result after query modification as
**		range of x is baserel
**		append to baserel (a / 3 = 7)
**	which is not acceptable.  Of course, there is a range of cases
**	where this can be fixed, but (for the time being) we will just
**	throw them all out.
**
**	Disappearing tuple anomaly:  the implicit qualification on
**	a view allows tuples to disappear even when not a duplicate.
**	For example, take a view defined as:
**		range of x is baserel
**		define v (d = x.a) where x.a = 4
**	and issue the query
**		append to v (d = 5)
**	The tuple will be inserted into the base relation, but will not
**	be included in the view.  To solve that problem, we disallow
**	updates to domains included in the qualification of the query.
**	Note that this includes implicit updates, that is, an append
**	with the domain missing (which implicitly appends a zero or
**	blank domain).
**
**	Cross product problem:  a view which is defined as a cross
**	product of two relations has several update anomalies.  For
**	example, take R1 and R2 as:
**              R1 | a | b      R2 | b | c
**              ---|---|---     ---|---|---
**                 | 7 | 0         | 0 | 3
**                 | 8 | 0         | 0 | 4
**	and issue the view definition
**		range of m is R1
**		range of n is R2
**		define v (m.a, m.b, n.c) where m.b = n.b
**	which will define a view which looks like
**              view | a | b | c
**              -----|---|---|---
**                   | 7 | 0 | 3
**                   | 7 | 0 | 4
**                   | 8 | 0 | 3
**                   | 8 | 0 | 4
**	Now try issuing
**		range of v is v
**		delete v where v.a = 8 and v.c = 4
**	which will try to give a view which looks like:
**              view | a | b | c
**              -----|---|---|---
**                   | 7 | 0 | 3
**                   | 7 | 0 | 4
**                   | 8 | 0 | 3
**	which is of course unexpressible in R1 and R2.
**
**	Multiple query problem:  certain updates will require gener-
**	ating multiple queries to satisfy the update on the view.
**	Although this can be made to work, it won't now.  Cases are
**	replaces where the target list contains more than one
**	relation, and appends to a view over more than one relation.
**
**	To solve these problems, we dissallow the following cases:
**
**	I.  In a REPLACE or APPEND statement, if a 'v.d' appears
**		on the LHS in the target list of the query and
**		the a-fcn for 'v.d' is not a simple attribute.
**	II.  In REPLACE or APPEND statements, if a 'v.d' appears
**		on the LHS in a target list of the query and in
**		the qualification of the view.
**	III.  In a DELETE or APPEND statement, if the view ranges
**		over more than one relation.
**	IV.  In a REPLACE statement, if the query resulting after
**		modification of the tree, but before appending the
**		view qualification Qv, has more than one variable.
**	V.  In any update, if an aggregate or aggregate function
**		appears anywhere in the target list of the view.
**
**	Note the assumption that the definition of a consistant update
**	is:
**		"An update is consistant if the result of
**		 performing the update on the view and then
**		 materializing that view is the same as the
**		 result of materializing the view and then
**		 performing the update."
**
**	Trace Flags:
**		30 -> 39
*/
/*
**  VIEW -- driver for view processing
**
**	This routine does the view processing portion of qrymod.
**	Since the 'tree' catalog can contain relations which are
**	themselves views, it iterates over itself until no views
**	are found.  Presumably this cannot result in an infinite
**	loop, although in fact it probably can; this should be
**	dealt with at some time.
**
**	For each range variable declared, it is checked whether
**	that variable is a view.  If not, it is ignored.
**	Then the tree which defines
**	this view is fetched from the "tree" catalog by 'gettree',
**	which also defines any variables required by this tree
**	and adjusts the tree so that the varno's contained in the
**	tree correspond to the varno's in the range table.
**
**	'Subsvars' and 'vrscan' really do it.  Given the root of the tree
**	to be modified, the variable number to be eliminated, and the
**	target list for a replacement tree, they actually do the
**	tacking of 'new tree' onto 'old tree'.  After it is done,
**	there should be no references to the old variable at all
**	in the tree.  'Subsvars' scans for VAR nodes (which are
**	retrieve-only, and hence are always alright); 'vrscan' scans
**	the left hand branch of the tree (the RESDOM nodes) and
**	substitutes them.
**
**	'Appqual' appends the qualification for the view (if any)
**	onto the tree.  Finally, the variable for the view (which
**	had all references to it eliminated by 'subsvars') is un-
**	defined, so that that slot in the range table can be re-
**	used by later scans.
**
**	Parameters:
**		root -- root of the tree to be modified.
**
**	Returns:
**		Root of modified tree.
**
**	Side Effects:
**		The range table is updated to delete any views and
**			add any base relations needed to support them.
**		Activity occurs in the 'tree' catalog to get the trees
**			needed to define the views.
**		The tree pointed to by 'root' is modified.
**
**	Trace Flags:
**		30
*/

QTREE *
view(root)
QTREE	*root;
{
	register int	i;
	DESC		desc;
	register int	vn;
	register QTREE	*vtree;
	int		viewfound;
	extern QTREE	*gettree();
	extern QTREE	*norml();
	auto QTREE	*r;

#	ifdef xQTR1
	tTfp(30, -1, "\n->VIEW\n\n");
#	endif

	r = root;

	/* scan range table until no views */
	viewfound = TRUE;
	while (viewfound)
	{
#		ifdef xQTR2
		tTfp(30, 1, "scanning Qt.qt_rangev\n");
#		endif

		/* scan range table for views */
		viewfound = FALSE;

		/* make new resultvar old resultvar for non-update */
		Qm.qm_newresvar = Qt.qt_resvar;

		/* scan all variables in range table */
		for (vn = 0; vn < MAXVAR + 1; vn++)
		{
			/* check for empty entry in range table */
			if (Qt.qt_rangev[vn].rngvdesc == NULL)
				continue;

			/* see if it is a view or base relation */
			if (!bitset(S_VIEW, Qt.qt_rangev[vn].rngvdesc->reldum.relstat))
				continue;
#			ifdef xQTR1
			if (tTf(30, 3))
				printf("view vn %d: %.12s\n", vn,
				    Qt.qt_rangev[vn].rngvdesc->reldum.relid);
#			endif

			vtree = gettree(Qt.qt_rangev[vn].rngvdesc->reldum.relid,
					Qt.qt_rangev[vn].rngvdesc->reldum.relowner,
					mdVIEW, 0, FALSE);
#			ifdef xQTR3
			if (tTf(30, 5))
				treepr(vtree, "Viewdef");
#			endif

			/* check for updating with aggregates */
			if (Qt.qt_qmode != mdRETR && aggcheck(vtree))
				qmerror(3350, Qt.qt_qmode, Qt.qt_resvar, 0);	/* cannot update views with aggregates */

			/* scan view replacing RESDOM nodes */
			if (Qt.qt_qmode != mdRETR && vn == Qt.qt_resvar)
				vrscan(&r->left, vtree);

			/* scan view replacing VAR nodes */
			subsvars(&r, vn, vtree->left, mdVIEW);

			/* test for non-functional replace */
			if (Qt.qt_qmode == mdREPL && bitcnt(varset(r) | (1 << Qm.qm_newresvar)) > 1)
				qmerror(3360, Qt.qt_qmode, Qt.qt_resvar, 0);	/* non-functional update */

			/* append new qualification */
			appqual(vtree->right, r);

			/* delete view range variable */
			declare(vn, NULL);

			/* mark the view as having been processed */
			viewfound = TRUE;

			/* change 'Qt.qt_resvar' to be the base rel var */
			Qt.qt_resvar = Qm.qm_newresvar;
		}
	}

	/* renormalize the tree (just in case) */
	r->right = norml(trimqlend(r->right));

#	ifdef xQTR1
	if (tTf(30, 15))
		treepr(r, "VIEW->");
#	endif

	return (r);
}
/*
**  VRSCAN -- scan query tree and replace RESDOM nodes
**
**	The query tree issued is scanned and RESDOM nodes are
**	converted to conform to the underlying base relations.
**	There are many checks in here, and things can fail
**	easily.
**
**	The first check is for more than one relation in a
**	DELETE or APPEND command.  This would require expanding
**	the query into at least two queries.  For DELETE commands,
**	this is the only check.  (Note that by this time 'aggcheck'
**	has aborted anything which would cause problems with
**	aggregates.)
**
**	For append commands, we abort immediately if there is
**	a qualification on the view, since the inserted tuple(s)
**	might not (all) appear in the view.
**
**	For all other queries, the target list of the query submitted
**	is scanned down the left hand side (the RESDOM list).
**	For each RESDOM, that variable is looked up in the view
**	definition.  If the definition of it is not a simple
**	attribute, the query is aborted.
**
**	Then, if the variable appears anywhere in the qualification
**	of the view, the query is aborted.
**
**	Finally, we keep track of the varno which should become the
**	new number two (that is, the Qt.qt_resvar).  If there are two
**	candidates for this position, we promptly abort.
**
**	And as the last step, we actually change the 'resno' for
**	this RESDOM.
**
**	When we exit the loop which scans RESDOM's, we change the
**	'Qt.qt_resvar' to be the new variable which we have selected.
**
**	Notice that there are a number of overly restrictive
**	conditions on runability.  Notably, there are large classes
**	of queries which can run consistantly but which violate
**	either the not-in-qualification condition or the aggregate-
**	free condition.
**
**	Parameters:
**		root -- the root of the tree to be updated.
**		vtree -- the tree which defines the view.
**
**	Returns:
**		none (maybe non-local on error)
**
**	Side Effects:
**		The tree pointed to by 'root' is modified.
**
**	Trace Flags:
**		33
*/

vrscan(root, vtree)
QTREE	*root;
QTREE	*vtree;
{
	register QTREE	*t;
	register QTREE	*v;
	int		i;
	extern QTREE	*qscan();
	extern QTREE	*vfind();
	register QTREE	*p;

	t = root;
	v = vtree;

	/* check DELETE and APPEND cases of > 1 relation */
	if (Qt.qt_qmode == mdDEL || Qt.qt_qmode == mdAPP)
	{
		/* scan target list of view for > 1 relation */
		if (bitcnt(i = varset(v->left)) != 1)
			qmerror(3330, Qt.qt_qmode, Qt.qt_resvar, 0);	/* query would result in > 1 query */

		/* this is the only check in this module for DELETES */
		if (Qt.qt_qmode == mdDEL)
		{
			/* set Qt.qt_resvar to underlying (single) relation */
			Qm.qm_newresvar = bitpos(i);
			return;
		}

		/* check for a qualification on an append */
		if (v->right->sym.type != QLEND)
			qmerror(3320, Qt.qt_qmode, Qt.qt_resvar, 0);	/* attribute in qualification of view */
	}

	/* scan target list of query */
	i = -1;
	while ((t = t->left)->sym.type != TREE)
	{
		if (t->sym.type != RESDOM)
			syserr("vrscan: bad TL node %d", t->sym.type);

		/* check for 'tid' attribute (stuck in by DEL and REPL) */
		if (t->sym.value.sym_resdom.resno == 0)
			continue;

		/* find definition for this domain in the view */
		p = vfind(t->sym.value.sym_resdom.resno, v->left);

		/* check for simple attribute */
		if (p->sym.type != VAR)
			qmerror(3310, Qt.qt_qmode, Qt.qt_resvar, 0);	/* non-simple attribute */

		/* scan qualification of view for this attribute */
		if (qscan(v->right, p->sym.value.sym_var.varno, p->sym.value.sym_var.attno) != NULL)
			qmerror(3320, Qt.qt_qmode, Qt.qt_resvar, 0);	/* attribute in qualification of view */

		/* check for trying to do update on two relations again */
		/* this test should only be true for REPLACE commands */
		if (i < 0)
			i = p->sym.value.sym_var.varno;
		else if (i != p->sym.value.sym_var.varno)
			qmerror(3330, Qt.qt_qmode, Qt.qt_resvar, 0);	/* query on two relations */

		/* finally, do the substitution of resno's */
		t->sym.value.sym_resdom.resno = p->sym.value.sym_var.attno;
	}

	/* change the result variable for the query to the underlying */
	Qm.qm_newresvar = i;
}
