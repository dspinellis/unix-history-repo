# include	<pv.h>
# include	<ingres.h>
# include	<aux.h>
# include	<catalog.h>
# include 	<func.h>
# include	<sccs.h>

SCCSID(@(#)destroy.c	7.1	2/5/81)

extern	short	tTdbu[];
extern	int	destroy();
extern 	int	null_fn();

struct fn_def DstroyFn =
{
	"DESTROY",
	destroy,
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
**  DESTROY RELATION
**
**	The relations in pv are destroyed.  This involves three steps:
**	1 - remove tuple from relation relation
**	2 - remove tuples from attribute relation
**	3 - unlink physical file
**
**	If the relation is a secondary index, the entry is removed
**	from the index relation, and the primary relation is set to
**	be "not indexed" (unless there is another index on it).
**
**	If the relation has an index, all its indexes are also
**	destroyed.
**
**	If any errors occured while destroying the relations,
**	then the last error number is returned, otherwise
**	0 is returned.
**
**	If any query modification was defined on the relation,
**	the qrymod catalogues are updated.
**
**	Trace Flags:
**		32
*/

destroy(pc, pv)
int	pc;
PARM	*pv;
{
	register int	i, ret;
	register char	*name;

	opencatalog("relation", 2);
	opencatalog("attribute", 2);

	for (ret = 0; pc-- > 0; )
	{
		name = ((pv++)->pv_val).pv_str;
		if (i = des(name))
			ret = i;
	}
	return (ret);
}


des(name)
char	*name;
{
	register int	i;
	register char	*relname;
	struct tup_id	tid;
	char		newrelname[MAXNAME + 3];
	extern DESC	Reldes, Attdes, Inddes, Treedes;
	struct relation	relt, relk;

	relname = name;
#	ifdef xZTR1
	tTfp(32, -1, "destroy: %s\n", relname);
#	endif

	newrelname[MAXNAME + 2] = 0;

	/* get the tuple from relation relation */
	setkey(&Reldes, &relk, relname, RELID);
	setkey(&Reldes, &relk, Usercode, RELOWNER);
	if ((i = getequal(&Reldes, &relk, &relt, &tid)) != 0)
	{
		if (i < 0)
			syserr("DESTROY: geteq(rel/%s) %d", relname, i);
		return (nferror(5202, relname, 0));	/* nonexistant relation */
	}

	/* don't allow a system relation to be destroyed */
	if (relt.relstat & S_CATALOG)
		return (nferror(5201, relname, 0));	/* attempt to destroy system catalog */

	if ((i = delete(&Reldes, &tid)) != 0)
		syserr("DESTROY: delete(rel) %d", i);

	/*
	** for concurrency reasons, flush the relation-relation page
	** where the tuple was just deleted. This will prevent anyone
	** from being able to "openr" the relation while it is being
	** destroyed. It also allows recovery to finish the destroy
	** it the system crashes during this destroy.
	*/
	if (i = flush_rel(&Reldes, FALSE))
		syserr("destroy:flush rel %d", i);

	purgetup(&Attdes, relt.relid, ATTRELID, relt.relowner, ATTOWNER, 0);

	/*
	**	If this is a user relation, then additional processing
	**	might be needed to clean up indicies, protection constraints
	**	etc.
	*/
	if ((relt.relstat & S_CATALOG) == 0)
		userdestroy(&relt);

	if ((relt.relstat & S_VIEW) == 0)
	{
		ingresname(relname, Usercode, newrelname);
		if (unlink(newrelname) < 0)
			syserr("destroy: unlink(%.14s)", newrelname);
	}
	return (0);

}
