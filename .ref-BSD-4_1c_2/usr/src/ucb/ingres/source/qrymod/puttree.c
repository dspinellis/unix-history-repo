# include	<ingres.h>
# include	<catalog.h>
# include	<symbol.h>
# include	<lock.h>
# include	<tree.h>
# include	"qrymod.h"
# include	<sccs.h>

SCCSID(@(#)puttree.c	7.1	2/5/81)

/*
**  PUTTREE -- put tree into 'tree' catalog
**
**	The named tree is inserted into the 'tree' catalog.
**
**	The algorithm is to lock up the entire catalog and try to
**	find the smallest unique id possible for the named relation.
**
**	Parameters:
**		root -- the root of the tree to insert.
**		treerelid -- the relid of the relation for which
**			this tree applies.
**		treeowner -- the owner of the above relation.
**		treetype -- the type of this tree; uses the mdXXX
**			type (as mdPROT, mdINTEG, mdDISTR, etc.).
**
**	Returns:
**		The treeid that was assigned to this tree.
**
**	Side Effects:
**		The tree catalog gets locked, and information is
**		inserted.
**
**	Trace Flags:
**		10
*/

puttree(root, trelid, towner, ttype)
QTREE	*root;
char	*trelid;
char	*towner;
int	ttype;
{
	struct tree	treekey;
	struct tree	treetup;
	struct tup_id	treetid;
	register int	i;
	auto int	treeid;

	opencatalog("tree", 2);

	/*
	**  Find a unique tree identifier.
	**	Lock the tree catalog, and scan until we find a
	**	tuple which does not match.
	*/

	setrll(A_SLP, Treedes.reltid.ltid, M_EXCL);

	setkey(&Treedes, &treekey, trelid, TREERELID);
	setkey(&Treedes, &treekey, towner, TREEOWNER);
	setkey(&Treedes, &treekey, &ttype, TREETYPE);
	for (treeid = 0;; treeid++)
	{
		setkey(&Treedes, &treekey, &treeid, TREEID);
		i = getequal(&Treedes, &treekey, &treetup, &treetid);
		if (i < 0)
			syserr("d_tree: getequal");
		else if (i > 0)
			break;
	}

	/*
	**  We have a unique tree id.
	**	Insert the new tuple and the tree into the
	**	tree catalog.
	*/

	relntrwr(NULL, 0, trelid, towner, ttype, treeid);
	writeqry(root, relntrwr, 0);
	relntrwr(NULL, 1);

	/* all inserted -- flush pages and unlock */
	if (noclose(&Treedes) != 0)
		syserr("d_tree: noclose");
	unlrl(Treedes.reltid.ltid);

	return(treeid);
}
/*
**  RELNTRWR -- physical tree write to relation
**
**	This is the routine called from writeqry to write trees
**	to the 'tree' relation (rather than the W_down pipe).
**
**	It is assumed that the (treerelid, treeowner, treetype,
**	treeid) combination is unique in the tree catalog, and that
**	the tree catalog is locked.
**
**	Parameters:
**		ptr -- a pointer to the data.  If NULL, this is
**			a control call.
**		len -- the length of the data.  If ptr == NULL, this
**			field is a control code:  zero means
**			initialize (thus taking the next two param-
**			eters); one means flush.
**		treerelid -- the name of the relation for which this
**			tree applies (init only).
**		treeowner -- the owner of this relation (init only).
**		treetype -- on initialization, this tells what the
**			tree is used for.
**		treeid -- on initialization, this is the tree id we
**			want to use.
**
**	Returns:
**		The number of bytes written ('len').
**
**	Side Effects:
**		Well, yes.  Activity occurs in the tree catalog.
**
**	Trace Flags:
**		none
*/

relntrwr(ptr, len, treerelid, treeowner, treetype, treeid)
char	*ptr;
int	len;
char	*treerelid;
char	*treeowner;
int	treetype;
int	treeid;
{
	static struct tree	treetup;
	struct tup_id		treetid;
	register char		*p;
	register int		l;
	static char		*tptr;

	p = ptr;
	l = len;

	/* check for special function */
	if (p == NULL)
	{
		switch (l)
		{
		  case 0:
			clr_tuple(&Treedes, &treetup);
			bmove(treerelid, treetup.treerelid, MAXNAME);
			bmove(treeowner, treetup.treeowner, 2);
			treetup.treetype = treetype;
			treetup.treeid = treeid;
			tptr = treetup.treetree;
			break;

		  case 1:
			if (tptr != treetup.treetree)
			{
				if (insert(&Treedes, &treetid, &treetup, FALSE) < 0)
					syserr("relntrwr: insert 1");
			}
			break;
		
		  default:
			syserr("relntrwr: ctl %d", l);
		}
		return;
	}

	/* output bytes */
	while (l-- > 0)
	{
		*tptr++ = *p++;

		/* check for buffer overflow */
		if (tptr < &treetup.treetree[sizeof treetup.treetree])
			continue;
		
		/* yep, flush buffer to relation */
		if (insert(&Treedes, &treetid, &treetup, FALSE) < 0)
			syserr("relntrwr: insert 2");
		treetup.treeseq++;
		tptr = treetup.treetree;

		/* clear out the rest of the tuple for aesthetic reasons */
		*tptr = ' ';
		bmove(tptr, tptr + 1, sizeof treetup.treetree - 1);
	}

	return (len);
}
