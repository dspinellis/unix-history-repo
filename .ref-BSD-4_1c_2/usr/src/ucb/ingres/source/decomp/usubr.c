# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	<access.h>
# include	<pv.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)usubr.c	7.1	2/5/81)


/*
**	usubr.c
**
**	utility routines to handle setting up params, etc for DBU calls
*/



/*
 *	generate domain names, formats
 */
domnam(lnp, pre)
QTREE	**lnp;
char	*pre;
{

	register char 	suf, *n;
	char		name[MAXNAME];
	char		*getformat();
	register QTREE	**p;

	suf = '1';
	for (n=name; *n++= *pre++;);
	*n = '\0';
	n--;
	for (p = lnp; *p != NULL; p++)
	{
		*n = suf++;
		setp(PV_STR, name);
		setp(PV_STR, getformat(*p));
	}
}
/*
**	gets format in ascii from RESDOM or AOP node
*/
static char *
getformat(p)
QTREE	*p;
{

	static char	buf[10];
	register char	*b;

	b = buf;

	*b++ = p->sym.value.sym_op.opfrmt;
	itoa(p->sym.value.sym_op.opfrml & I1MASK, b);
	return(buf);
}
/*
**	makes list of nodes (depth first)
*/

lnode(nod, lnodv, count)
QTREE 	*nod, *lnodv[];
int	count;
{
	register QTREE	*q;
	register int	i;

	i = count;
	q = nod;

	if (q && q->sym.type != TREE)
	{
		i = lnode(q->left, lnodv, i);
		lnodv[i++] = q;
	}
	return(i);
}
/*
**	Immediately destroys the relation if it is an _SYS
*/

dstr_rel(relnum)
int	relnum;
{
	initp();
	if (dstr_mark(relnum))
		call_dbu(mdDESTROY, FALSE);
	else
		resetp();
}
/*
**	Put relation on list of relations to be
**	destroyed. A call to initp() must be
**	made before any calls to dstr_mark().
**
**	A call to call_dbu will actually have
**	the relations exterminated
**
**	Trace Flags:
**		65
*/

dstr_mark(relnum)
int	relnum;
{
	register char	*p;
	char		*rnum_convert();
	bool		dstr_flag;

	dstr_flag = FALSE;
	if (rnum_temp(relnum))
	{
		p = rnum_convert(relnum);
#		ifdef xDTR1
		if (tTf(65, 4))
			printf("destroying %s\n", p);
#		endif
		setp(PV_STR, p);
		specclose(relnum);	/* guarantee that relation is closed and descriptor destroyed */
		rnum_remove(relnum);
		dstr_flag = TRUE;	/* indicate that there are relations to be destroyed */
	}
	return(dstr_flag);
}
/*
**	Make a temporary relation to match
**	the target list of tree.
**
**	If rnum is positive, use it as the relation number,
**	Otherwise allocate a new one.
*/

mak_t_rel(tree, prefix, rnum)
QTREE	*tree;
char	*prefix;
int	rnum;
{
	QTREE		*lnodv[MAXDOM + 1];
	register int	relnum;

	initp();
	setp(PV_STR, "0");	/* initial relstat field */
	relnum = rnum < 0 ? rnum_alloc() : rnum;
	setp(PV_STR, rnum_convert(relnum));
	lnodv[lnode(tree->left, lnodv, 0)] = NULL;
	domnam(lnodv, prefix);

	call_dbu(mdCREATE, FALSE);
	return (relnum);
}


QTREE **
mksqlist(tree, var)
QTREE	*tree;
int	var;
{
	register QTREE	**sq;
	register int	i;
	static QTREE	*sqlist[MAXRANGE];

	sq = sqlist;
	for (i = 0; i < MAXRANGE; i++)
		*sq++ = 0;

	sqlist[var] = tree;
	return (sqlist);
}




long
rel_pages(tupcnt, width)
long	tupcnt;
int	width;
{
	register int	tups_p_page;

	tups_p_page = (PGSIZE - 12) / (width + 2);
	return ((tupcnt + tups_p_page - 1) / tups_p_page);
}
