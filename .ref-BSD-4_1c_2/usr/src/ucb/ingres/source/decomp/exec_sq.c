# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	<pv.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)exec_sq.c	7.1	2/5/81)

/*
** DECOMP2 -- Routines for executing detached sub-queries appearing
**	in sqlist. These routines include:
**
**	exec_sq -- execute sub-queries and update range table.
**
**	undo_sq -- restore range table and destroy temp rels.
**
**	reset_sq - restore range table and reset temp rels.
**
**	execsq1 -- call ovqp with subquery.
*/
/*
**  EXEC_SQ
**
**	Execute the subqueries in sqlist. Associated with
**	each sub-query is a relation number stored in sqrange.
**	If the sub-query has a non-null target list, the range
**	table is updated to reflect the new range of the relation.
**
**	If any sub-query is false, all subsequent ones are ignored
**	by ovqp and exec_sq returns the var number of the false subquery.
**
**	As a side effect, "disj" is incremented for each disjoint sub-query
**
**	Trace Flags:
**		35
*/

exec_sq(sqlist, sqrange, disj)
QTREE	*sqlist[];
int	sqrange[];
int	*disj;
{
	register QTREE	*sq;
	register int	i, qualfound;
	extern DESC	*openr1();

#	ifdef xDTR1
	if (tTf(35, 0))
		printf("EXEC_SQ--\n");
#	endif

	*disj = 0;

	for (i = 0; i < MAXRANGE; i++)
	{
		if (sq = sqlist[i])
		{
#			ifdef xDTR1
			if (tTf(35, 1))
				printf("sq[%d]=%x\n", i, sq);
#			endif
			qualfound = execsq1(sq, i, sqrange[i]);

#			ifdef xDTR1
			if (tTf(35, 2))
				printf("qualfound=%d\n", qualfound);
#			endif
			if (!qualfound)
			{
				return(i);
			}
			if (sq->left->sym.type != TREE)
			{
				/*
				** Update the range table and open
				** the relation's restricted replacement.
				*/
				new_range(i, sqrange[i]);
				openr1(i);
			}
			else
			{
				(*disj)++;
			}
		}
	}
	return (-1);
}
/*
**  UNDO_SQ
**
**	Undo the effects of one variable detachment on
**	the range table. The two parameters "limit" and
**	"maxlimit" describe how far down the list of
**	subqueries were processed.  Maxlimit represents
**	the furthest every attained and limit represents
**	the last variable processed the last time.
**
**	Trace Flags:
**		36
*/

undo_sq(sqlist, locrang, sqrange, limit, maxlimit, reopen)
QTREE	*sqlist[];
int	locrang[];
int	sqrange[];
int	limit;
int	maxlimit;
int	reopen;
{
	register QTREE	*sq;
	register int	i, lim;
	bool		dstr_flag;

#	ifdef xDTR1
	if (tTf(36, 0))
		printf("UNDO_SQ--\n");
#	endif

	initp();	/* setup parm vector for destroys */
	lim = limit == -1 ? MAXRANGE : limit;
	if (maxlimit == -1)
		maxlimit = MAXRANGE;

	for (i = 0; i < MAXRANGE; i++)
		if (sq = sqlist[i])
		{
			if (sq->left->sym.type != TREE)
			{
				if (i < lim)
				{
					/* The query was run. Close the temp rel */
					closer1(i);
				}

				/* mark the temporary to be destroyed */
				dstr_mark(sqrange[i]);
				dstr_flag = TRUE;

				/* reopen the original relation. If maxlimit
				** never reached the variable "i" then the
				** original relation was never closed and thus
				** doesn't need to be reopened.
				*/
				rstrang(locrang, i);
				if (reopen && i < maxlimit)
					openr1(i);
			}
		}
	/* Only call destroy if there's something to destroy */
	if (dstr_flag)
		call_dbu(mdDESTROY, FALSE);
	else
		resetp();

}
/*
** Execsq1 -- call ovqp with mdRETR on temp relation
*/

execsq1(sq, var, relnum)
QTREE 	*sq;
int	var;
int	relnum;
{
	register int	qualfound;

	De.de_sourcevar = var;
	De.de_newq = 1;
	qualfound = call_ovqp(sq, mdRETR, relnum);
	return (qualfound);
}
/*
**	Reset each relation until limit.
**	Reset will remove all tuples from the
**	relation but not destroy the relation.
**	The descriptor for the relation will be removed
**	from the cache.
**
**	The original relation is returned to
**	the range table.
**
**	If limit is -1 then all relations are done.
*/

reset_sq(sqlist, locrang, limit)
QTREE	*sqlist[];
int	locrang[];
int	limit;
{
	register QTREE	*sq;
	register int	i, lim;
	int		old, reset;
	extern char	*rnum_convert();

	lim = limit == -1 ? MAXRANGE : limit;
	reset = FALSE;
	initp();

	for (i = 0; i < lim; i++)
		if ((sq = sqlist[i]) && sq->left->sym.type != TREE)
		{
			old = new_range(i, locrang[i]);
			setp(PV_STR, rnum_convert(old));
			specclose(old);
			reset = TRUE;
		}

	if (reset)
	{
		/*
		** Guarantee that OVQP will not reuse old
		** page of relation being reset
		*/
		De.de_newr = TRUE;
		call_dbu(mdRESETREL, FALSE);
	}
	else
		resetp();
}
