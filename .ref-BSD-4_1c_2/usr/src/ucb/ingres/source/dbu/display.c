# include	<ingres.h>
# include	<aux.h>
# include	<catalog.h>
# include	<tree.h>
# include	<symbol.h>
# include	<access.h>
# include 	<func.h>
# include	<pv.h>
# include	<sccs.h>

SCCSID(@(#)display.c	7.1	2/5/81)

/*
**  DISPLAY -- display query corresponding to view, permission, 
**		or intgerity declaration
*/




extern short	tTdbu[];
extern int	display();
extern int	null_fn();

struct fn_def DsplayFn =
{
	"DISPLAY",
	display,
	null_fn,
	null_fn,
	NULL,
	0,
	tTdbu,
	100,
	'Z',
	0
};
/*
**  DISPLAY -- display query
**
**	Parameters:
**		pc -- number of args in pv
**		pv -- pv[i] == 4 for VIEW, 5 for PERMIT, 6 for INTEGRITY
**				where i % 2 == 0
**		pv[i] -- relation names for which pv[i-1] is mode
**				where i%2==1
**
**	Returns:
**		0 -- total success
**		err -- error number of last error
**
**	Side Effects:
**		prints out definition of appropriate characteristic of relation
**
**	Trace Flags:
**		33, -1
*/


display(pc, pv)
int	pc;
PARM	pv[];
{
	register int	ac;
	register PARM	*av;
	extern DESC	Treedes;
	register int	i;
	int		err;
	char		err_array[PV_MAXPC];
	auto int	mode;

#	ifdef xZTR1
	if (tTf(50, -1))
	{
		printf("display: ");
		prvect(pc, pv);
	}
#	endif

	err = 0;
	if (pc % 2 != 0)
		syserr("display: bad param count %d", pc);
	opencatalog("tree", 0);

	for (ac = 0, av = pv; ac < pc; av++, ac++)
	{
		if (atoi(av->pv_val.pv_str, &mode) != 0)
			syserr("display: mode `%s'", av->pv_val.pv_str);
		av++;
		err_array[ac++] = 0;
		err_array[ac] = disp(av->pv_val.pv_str, mode);
	}
	for (ac = 0, av = pv; ac < pc; av++, ac++)
	{
		if (err_array[ac])
			err = error(5400 + err_array[ac], (av->pv_val).pv_str, 0);
	}
	return (err);
}
/* 
** DISP -- display integrity, permit, or define query on a relation
**
**	Finds a relation owned by the user or the DBA and passes
**	the name and owner to the appropritae routine depending on
**	mode.
**
**	Parameters:
**		relation -- relation on which query is to be printed
**		mode -- the print mode:
**			4 -- view
**			5 -- permit
**			6 -- integrity
**
**	Returns:
**		0 -- success
**		1 -- no such relation, or none seeable by the user.
**		3 -- VIEW mode and relation not a view
**		4 -- PERMIT and no permissions on relation
**		5 -- INTEGRITY mode and no integrity constraints
**
**	Trace Flags:
**		33, 8
*/

disp(relation, mode)
char	*relation;
int	mode;
{
	DESC		d;
	register int	i;
	extern char	*Resrel;

#	ifdef xZTR1
	if (tTf(50, 8))
		printf("disp: relation %s\n", relation);
#	endif

	Resrel = relation;
	i = openr(&d, -1, relation);
	if (i > 0)
		return (1);
	else if (i < 0)
		syserr("disp: openr(%s) ret %d", relation, i);
	switch (mode)
	{
	  case 4:		/* View query */
		if (d.reldum.relstat & S_VIEW)
			pr_def(relation, d.reldum.relowner);
		else 
			return (3);
		break;

	  case 5:
		if (pr_prot(relation, &d))
			return (4);
		break;

	  case 6:
		if (d.reldum.relstat & S_INTEG)
			pr_integrity(relation, d.reldum.relowner);
		else
			return (5);
		break;

	  default:
		syserr("disp: mode == %d", mode);
	}
	return (0);
}
/*
**  PR_DEF -- Print "define view" query of a view
**
**	Parameters:
**		relation -- relation in question
**		owner -- relowner
**
**	Returns:
**		none
**
**	Side Effects:
**		reads a tree, clears range table
**
**	Trace Flags:
**		33, 9
*/

pr_def(relation, owner)
char	*relation;
char	*owner;
{
	register QTREE	*t;
	QTREE		*gettree();

#	ifdef xZTR1
	if (tTf(50, 9))
		printf("pr_def(relation=\"%s\", owner=%s)\n", relation, owner);
#	endif

	printf("View %s defined:\n\n", relation);
	clrrange();

	/* Treeid == 0 because views have only one definition */
	t = gettree(relation, owner, mdVIEW, 0);
	pr_range();
	printf("define view ");
	pr_tree(t);
}
/*
**  PR_INTEGRITY -- print out integrity constraints on a relation
**
**	Finds all integrity tuples for this unique relation, and
**	calls pr_int() to print a query from them.
**
**	Parameters:
**		relid -- rel name
**		relowner -- 2 byte owner id
**
**	Returns:
**		none
**
**	Side Effects:
**		file activity, query printing
**
**	Trace Flags:
**		33, 9
*/

pr_integrity(relid, relowner)
char	*relid;
char	*relowner;
{
	extern DESC		Intdes;
	TID			hitid, lotid;
	struct integrity	key, tuple;
	register int		i;


#	ifdef xZTR1
	if (tTf(50, 9))
		printf("pr_integrity(relid =%s, relowner=%s)\n", 
		relid, relowner);
#	endif

	printf("Integrity constraints on %s are:\n\n", relid);
	opencatalog("integrities", 0);

	/* get integrities tuples for relid, relowner */
	clearkeys(&Intdes);
	setkey(&Intdes, &key, relid, INTRELID);
	setkey(&Intdes, &key, relowner, INTRELOWNER);
	if (i = find(&Intdes, EXACTKEY, &lotid, &hitid, &key))
		syserr("pr_integrity: find %d", i);
	for ( ; ; )
	{
		if (i = get(&Intdes, &lotid, &hitid, &tuple, TRUE))
			break;
		if (kcompare(&Intdes, &tuple, &key) == 0)
			pr_int(&tuple, relid);
	}
	if (i != 1)
		syserr("pr_integrity: get %d", i);
}
/*
**  PR_INT -- print an integrity definition given a integrities tuple
**
**	Parameters:
**		i -- integrity tuple
**
**	Returns:
**		none
**
**	Side Effects:
**		prints a query
**		reads a tree
*/

pr_int(i, relid)
register struct integrity	*i;
char				*relid;
{
	register QTREE	*t;
	QTREE		*gettree();

	clrrange();
	t = gettree(i->intrelid, i->intrelowner, mdINTEG, i->inttree);
	printf("Integrity constraint %d -\n\n", i->inttree);
	pr_range();

	printf("define integrity on ");
	pr_rv(Qt.qt_resvar = i->intresvar);
	printf(" is "); 
	pr_qual(t->right);
	printf("\n\n\n");

}
