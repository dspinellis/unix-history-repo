# include	<ingres.h>
# include	<catalog.h>
# include	<tree.h>
# include	<symbol.h>
# include	<sccs.h>

SCCSID(@(#)gettree.c	7.1	2/5/81)


/*
**  GETTREE -- get tree from 'tree' catalog
**
**	This function, given an internal treeid, fetches and builds
**	that tree from the 'tree' catalog.  There is nothing exciting
**	except the mapping of variables, done by mapvars().
**
**	Parameters:
**		treeid -- internal id of tree to fetch and build.
**		init -- passed to 'readqry' to tell whether or not
**			to initialize the query buffer.
**
**	Returns:
**		Pointer to root of tree.
**
**	Side Effects:
**		file activity.  Space in Qbuf is used up.
**
**	Trace Flags:
**		13
*/

QTREE *
gettree(treerelid, treeowner, treetype, treeid, init)
char	*treerelid;
char	*treeowner;
char	treetype;
int	treeid;
int	init;
{
	register QTREE	*t;
	extern int	relntrrd();
	register int	i;
	extern QTREE	*readqry();

	/* initialize relntrrd() for this treeid */
	relntrrd(0, NULL, 0, treerelid, treeowner, treetype, treeid);

	/* read and build query tree */
	t = readqry(relntrrd, 0, init);

	/* remap varno's to be unique */
	if (!init)
		mapvars(t);

	return (t);
}
/*
**  RELNTRRD -- read tree from 'tree' relation
**
**	This looks exactly like the 'pipetrrd' call, except that info
**	comes from the 'tree' talolog instead of from the pipe.  It
**	must be initialized by calling it with a NULL pointer and
**	the segment name wanted as 'treeid'.
**
**	Parameters:
**		dummyx -- a placeholder parameter to make this
**			routine compatible with pb_get.
**		ptr -- NULL -- "initialize".
**			else -- pointer to read area.
**		cnt -- count of number of bytes to read.
**		treerelid -- if ptr == NULL, the relation name
**			associated with the tree; ignored otherwise.
**		treeowner -- if ptr == NULL, the owner of the relation
**			associated with the tree; ignored otherwise.
**		treetype -- if ptr == NULL, the type of the tree
**			(view, prot, etc.); ignored otherwise.
**		treeid -- if ptr == NULL, this is the tree id,
**			otherwise this parameter is not supplied.
**
**	Returns:
**		count of actual number of bytes read.
**
**	Side Effects:
**		activity in database.
**		static variables are adjusted correctly.  Note that
**			this routine can be used on only one tree
**			at one time.
*/

relntrrd(dummyx, ptr, cnt, treerelid, treeowner, treetype, treeid)
char	*ptr;
int	cnt;
char	*treerelid;
char	*treeowner;
char	treetype;
int	treeid;
{
	static struct tree	trseg;
	static char		*trp;
	static int		seqno;
	register char		*p;
	register int		n;
	register int		i;
	struct tree		trkey;
	TID			tid;
	extern DESC		Treedes;

	p = ptr;
	n = cnt;

	if (p == NULL)
	{
		/* initialize -- make buffer appear empty */
		trp = &trseg.treetree[sizeof trseg.treetree];
		bmove(treerelid, trseg.treerelid, MAXNAME);
		bmove(treeowner, trseg.treeowner, 2);
		trseg.treetype = treetype;
		trseg.treeid = treeid;
		seqno = 0;
		opencatalog("tree", 0);

#		ifdef xQTR2
		if (tTf(13, 6))
			printf("relntrrd: n=%.12s o=%.2s t=%d i=%d\n",
			    treerelid, treeowner, treetype, treeid);
#		endif

		return (0);
	}

	/* fetch characters */
	while (n-- > 0)
	{
		/* check for segment empty */
		if (trp >= &trseg.treetree[sizeof trseg.treetree])
		{
			/* then read new segment */
			clearkeys(&Treedes);
			setkey(&Treedes, &trkey, trseg.treerelid, TREERELID);
			setkey(&Treedes, &trkey, trseg.treeowner, TREEOWNER);
			setkey(&Treedes, &trkey, &trseg.treetype, TREETYPE);
			setkey(&Treedes, &trkey, &trseg.treeid, TREEID);
			setkey(&Treedes, &trkey, &seqno, TREESEQ);
			seqno++;
			if ((i = getequal(&Treedes, &trkey, &trseg, &tid)) != 0)
				syserr("relnrdtr: getequal %d", i);
			trp = &trseg.treetree[0];
		}

		/* do actual character fetch */
		*p++ = *trp++;
	}

	return (cnt);
}
