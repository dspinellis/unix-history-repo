# include	<ingres.h>
# include	<symbol.h>
# include	<tree.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)selectv.c	7.1	2/5/81)

/*
**  SELECTV -- Select a variable for substitution
**
**	Selectv is called to determinimume the "best" variable
**	to substitute for. The algorithm used is based on
**	the Wong/Youseffi paper ERL M78/17. Selectv returns
**	the variable to be substituted or it returns -1 which
**	indicates that there is a relation with no tuples.
**	If there is only one variable left, it is of course
**	the one choosen.
**
**	For cases of more than one variable, a variable with
**	0 or 1 tuples will always be choosen. Otherwise the
**	variable with the smallest "costfunction" will be selected.
**	In case of a tie, the variable with the smallest tuple count
**	is selected.
**
**	The costfunction is:
**		|Ri|
**	       ______
**		Peffective i
**
**	where |Ri| = cardinality of variable i.
**	and   Peff = The number of pages needed to be accessed to
**			safisfy a query on Ri assuminimumg all other
**			variables have been substituted for.
**
**
**	Peff is only an estimate. The estimate is based on the
**	storage structure of Ri and on whether Ri is used in
**	the target list. Considering only equality clauses,
**	the relation's structure is examinimumed to see if any
**	can be used effectively. The following assumptions are
**	made:
**		useful hash: Peff = 1
**		useful isam: Peff = 2
**		useful hash sec index: Peff = 2
**		useful isam sec index: Peff = 3
**
**	If there is no useful structure or if the relation is
**	a heap then:
**		Peff = number of pages Ri occupies
**	If the relation is not in the target list then its
**	function is only an existence test. Scanning can and
**	is stopped the first time a tuple is found which satisfies.
**	We assume that on the average only 1/4 of the pages
**	will have to be examinimumed.
**
**	Parameters:
**		root - root of the query
**
**	Returns:
**		The variable to substitute for
**			or
**		-1 if the query is false
**
**	Side Effects:
**		can cause a relation to be opened since
**		ckpkey needs to know the key structure.
**
**	Called By:
**		decompy, decompz
**
**	Trace Flags:
**		44
*/

selectv(root)
register QTREE	*root;
{
	int				minimum, var, map, i;
	register struct rang_tab	*rt, *rx;
	long				costfunc(), lx, lt;

	map = root->sym.value.sym_root.lvarm | root->sym.value.sym_root.rvarm;

	minimum = -1;
	lx = 0;
	rx = NULL;

	for (i = 1, var = 0; map; i <<= 1, var++)
	{
		if ((map & i) == 0)
			continue;

		map &= ~i;
		rt = &De.de_rangev[var];
		if (rx == NULL)
		{
			rx = rt;
			minimum = var;
			if (map == 0 || rt->rtcnt < 2)
				break;
			lx = costfunc(root, var, rt);
		}
		else
		{
			if (rt->rtcnt < 2)
			{
				rx = rt;
				minimum = var;
				break;
			}

			lt = costfunc(root, var, rt);
			if (lt < lx)
			{
				rx = rt;
				minimum = var;
				lx = lt;
			}
		}
	}

	if (minimum == -1)
		syserr("selectv:no source var");

#	ifdef xDTR1
	if (tTf(44, 1))
	{
		printf("selectv:%d,tups=%ld,", minimum, rx->rtcnt);
		printf("cf=%ld,", lx);
		nodepr(root);
	}
#	endif

	if (rx->rtcnt == 0)
		minimum = -1;

	return (minimum);
}



long
costfunc(root, var, rx)
QTREE		*root;
int		var;
struct rang_tab	*rx;
{
	register struct rang_tab	*r;
	register int			i;
	register char			*lp;
	char				linkmap[MAXDOM];
	long				rel_page(), l;
	int				c_bug;

	r = rx;

	for (lp = linkmap, i = MAXDOM; i--; )
		*lp++ = 0;
	i = var;

	/*
	** The c-compiler does not know how to generate code
	** for the assignment of an short to a long inside
	** an "if" expression. Therefore the variable "c_bug"
	** is assigned the value of ckpkey inside the "if".
	** The value is assigned to "l" in the "else".
	*/
	if (!findlinks(root, -1, i, linkmap) || (c_bug = ckpkey(linkmap, i)) == 0)
	{
		l = rel_pages(r->rtcnt, r->rtwid);

		/* if not in target list, assume 1/4 */
		if ((root->sym.value.sym_root.lvarm & (01 << i)) == 0)
		{
			l >>= 2;
			if (l == 0)
				l = 1;
		}
	}
	else
		l = c_bug;	/* l could not be assigned directly above */

	l = r->rtcnt / l;

#	ifdef xDTR1
	if (tTf(44, 3))
		printf("costfunc %d is %ld\n", i, l);
#	endif
	return (l);
}
