# include	<pv.h>
# include	<ingres.h>
# include	<access.h>
# include	<aux.h>
# include	<catalog.h>
# include	<symbol.h>
# include	<func.h>
# include	<sccs.h>

SCCSID(@(#)rmqm.c	7.1	2/5/81)

/*
**  RMQM -- DBU to delete protection and integrity constraints
**
**	Trace Flags:
**		43
*/


extern	short	tTdbu[];
extern	int	dest_const();
extern	int	null_fn();

struct fn_def RmqmFn =
{
	"RMQM",
	dest_const,
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
**  DEST_CONST -- destroy constraints
**
**	Parameters:
**		pc -- number of parameters in pv
**		pv -- pv [0] == 5 destroy permission
**			         == 6 destroy integrity constraint
**		        pv [1]    relation from which to destroy constrain
**		        pv [2] == if (pc != 2) relation from which to delete
**				     	constraints
**		        pv[3] ... pv[pc - 1] == id of constraint
**
**	Returns:
**		0
**
**	Side Effects:
**		destroys constraints. Involves activity on catalogs 'relation',
**		protect, integrities, and tree.
**
**	Trace Flags:
**		43, 0
*/

dest_const(pc, pv)
int	pc;
PARM	pv[];
{
	DESC			d;
	register int		i;
	int			mode;
	extern struct admin	Admin;

#	ifdef xZTR1
	if (tTf(43, 0))
	{
		printf("dest_const: ");
		prvect(pc, pv);
	}
#	endif

	if (!(Admin.adhdr.adflags & A_QRYMOD))
		return (0);
	i = openr(&d, -1, pv[1].pv_val.pv_str);
	if (i < 0)
		syserr("dest_const: openr(%s) %d", pv[1].pv_val.pv_str, i);

	if (i == 1 || !bequal(Usercode, d.reldum.relowner, 2))
	{
		error(5202, pv[1].pv_val.pv_str, 0);
		return (0);
	}

	if (atoi(pv[0].pv_val.pv_str, &mode) < 0)
		syserr("rmqm: bad mode: %s", pv[0].pv_val.pv_str);
	if (mode == 5)
		dest_prot(&d, &pv[2]);
	else if (mode == 6)
		dest_integ(&d, &pv[2]);
	else
		syserr("dest_const: bad mode %d", mode);
	return (0);
}
/*
**  DEST_INTEG -- directs destruction of integrity constraints
**
**	Parameters:
**		desc -- descriptor for relation
**		intv -- PV_EOF terminated list of id strings, if first element
**		        is PV_EOF means "all"
**
**	Returns:
**		none
**
**	Side Effects:
**		deletes integrity constraint. Activity on 'relation', integrities,
**		and tree.
*/

dest_integ(d, intv)
register DESC	*d;
PARM		intv[];
{
	extern DESC		Intdes;
	struct integrity	tuple, key;
	struct tree		tkey;
	register		i, j;
	int			tree_const();
	int			int_inttree();

#	ifdef xZTR1
	if (tTf(43, 1))
		printf("dest_integ((%s, %s)...)\n", d->reldum.relid, d->reldum.relowner);
#	endif

	i_cat("integrities", &Intdes, &key, d->reldum.relid, INTRELID,
	d->reldum.relowner, INTRELOWNER, mdINTEG, &tkey);

	if (intv[0].pv_type == PV_EOF)
	{
		/* destroy integrity 'relation' ALL */
		if (!(d->reldum.relstat & S_INTEG))
			return (0);
		del_all(d, &Intdes, &key, &tuple, &tkey, S_INTEG,
		tree_const, int_inttree);
		return (0);
	}
	/* destroy integrity 'relation' int {, int} */
	for (i = 0; intv[i].pv_type != PV_EOF; i++)
		del_int(&Intdes, &key, &tuple, &tkey, intv[i].pv_val.pv_str, INTTREE, 
		tree_const, int_inttree);

	/* rescan to output error messages */
	for (j = 0; j < i; j++)
		if (*(intv[j].pv_val.pv_str))
			error(5203, intv[j].pv_val.pv_str, 0);

	/* finally, check that there are still integrity constraints
	** on the relation, if not must reset the S_INTEG bit in the relation
	** relation tuple for that relation.
	*/
	chk_const(d, &Intdes, &key, &tuple, d->reldum.relid, INTRELID, d->reldum.relowner,
	INTRELOWNER, S_INTEG);
}
/*
**  DEST_PROT -- directs destruction of protection constraints
**
**	Parameters:
**		desc -- descriptor for relation
**		intv -- PV_EOF terminated list of id strings, if first element
**		        is PV_EOF means "all"
**
**	Returns:
**		none
**
**	Side Effects:
**		deletes protection constraint. Activity on 'relation', 
**		protect, and tree.
**
**	Trace Flags:
**		43, 2
*/


dest_prot(d, intv)
register DESC	*d;
PARM		intv[];
{
	extern DESC	Prodes;
	struct protect	tuple, key;
	struct tree	tkey;
	register	i, j;
	int		propermid;
	int		prot_protree();
	int		tree_prot();

#	ifdef xZTR1
	if (tTf(43, 2))
		printf("dest_prot((%s, %s)...)\n", d->reldum.relid, d->reldum.relowner);
#	endif

	i_cat("protect", &Prodes, &key, d->reldum.relid, PRORELID, d->reldum.relowner,
	PRORELOWN, mdPROT, &tkey);

	if (intv[0].pv_type == PV_EOF)
	{
		/* destroy permit 'relation' ALL */
		if (!(d->reldum.relstat & S_PROTRET) || !(d->reldum.relstat & S_PROTALL))
			r_relstat(d, S_PROTRET | S_PROTALL, 1);
		if (!(d->reldum.relstat & S_PROTUPS))
			return (0);
		del_all(d, &Prodes, &key, &tuple, &tkey, S_PROTUPS,
		tree_prot, prot_protree);
		return (0);
	}
	/* destroy permit 'relation' int {, int} */
	for (i = 0; intv[i].pv_type != PV_EOF; i++)
	{
		if (j = atoi(intv[i].pv_val.pv_str, &propermid))
			syserr("dest_prot: bad atoi \"%s\" %d", intv[i].pv_val.pv_str, j);
		if (propermid == 0)
		{
			if (!(d->reldum.relstat & S_PROTALL))
			{
				r_relstat(d, S_PROTALL, 1);
				intv[i].pv_val.pv_str = 0;
			}
			continue;
		}
		else if (propermid == 1)
		{
			if (!(d->reldum.relstat & S_PROTRET))
			{
				r_relstat(d, S_PROTRET, 1);
				intv[i].pv_val.pv_str = 0;
			}
			continue;
		}
		del_int(&Prodes, &key, &tuple, &tkey, intv[i].pv_val.pv_str, PROPERMID, 
		tree_prot, prot_protree);
	}
	/* rescan to output error messages */
	for (j = 0; j < i; j++)
		if (intv[j].pv_val.pv_str)
			error(5204, intv[j].pv_val.pv_str, 0);

	/* finally, check that there are still permissions
	** on the relation, if not must reset the S_PROTUPS bit in the relation
	** relation tuple for that relation's relstat.
	*/
	chk_const(d, &Prodes, &key, &tuple, d->reldum.relid, PRORELID,
	d->reldum.relowner, PRORELOWN, S_PROTUPS);
}
/*
**  I_CAT -- prepare catalogs for deletin of constraint
**
**	Initializes treerelid, treeowner, and treetype fields
**	of tree key. Also relation id and owner fields of
**	appropriate catalog c_desc, with key 'key'.
**
**	Parameters:
**		c_name -- name of catalog for opencatalog
**		c_desc -- descriptor of catalog
**		key -- key for catalog
**		relid -- relation.relid for relation to be de-constrained
**		id_attno -- attno of relid in constraint catalog c_desc
**		relowner -- relation.relowner for rel to be de-constrained
**		own_attno -- attno of owner in constrain catalog
**		type -- treetype for tree tuple (depends on catalog)
**		tkey -- key for tree catalog
**
**	Returns:
**		none
**
**	Side Effects:
**		opencatalogs the constraint catalog c_desc, and the "tree" rel
**		for READ/WRITE. Sets keys.
**
**	Trace Flags:
**		43, 3
*/

i_cat(c_name, c_desc, key, relid, id_attno, relowner, own_attno, type, tkey)
char		*c_name;
DESC		*c_desc;
char		*key;
char		*relid;
int		id_attno;
char		*relowner;
int		own_attno;
int		type;
struct tree	*tkey;
{
	extern DESC	Treedes;

#	ifdef xZTR1
	if (tTf(43, 3))
		printf("i_cat(c_name \"%s\", relid %s id_attno %d relowner %s own_attno %d type %d)\n",
		c_name, relid, id_attno, relowner, own_attno, type);
#	endif

	opencatalog("tree", 2);
	setkey(&Treedes, tkey, relid, TREERELID);
	setkey(&Treedes, tkey, relowner, TREEOWNER);
	setkey(&Treedes, tkey, &type, TREETYPE);
	opencatalog(c_name, 2);
	clearkeys(c_desc);
	setkey(c_desc, key, relid, id_attno);
	setkey(c_desc, key, relowner, own_attno);
}
/*
**  DEL_ALL -- delete all constraints for a given relation
**
**	Deletes all constraints of a given type given by a constraint
**	catalog 'c_desc'. Note that Protection constraints 0 & 1, given
**	by relation.relstat field are not deleted here.
**
**	Parameters:
**		r_desc -- descriptor for relation to de-constrain (for
**			r_relstat)
**		c_desc -- constraint catalog descriptor
**		key -- c_desc's key
**		tuple -- c_desc's tuple (needed because sizeof tuple is not
**			known here, so must be allocated beforehand)
**		tkey -- tree key with TREERELID and TREERELOWNER setkeyed
**		bit -- bits in relstat to reset after deleting all constraints
**		tree_pred -- called with constraint tuple to determine
**			wether a tree tuple is present or not (as can happen
**			for protect catalog)
**		tree_field -- should return the treeid from tuple
**
**	Returns:
**		none
**
**	Side Effects:
**		tree and constraint catalog activity
**
**	Requires:
**		del_tree()
**		r_relstat()
**
**	Called By:
**		dest_????
**
**	Trace Flags:
**		43, 4
**
**	Syserrs:
**		bad find, get, delete, flush_rel
**
**	History:
**		1/10/79 -- (marc) written
*/

del_all(r_desc, c_desc, key, tuple, tkey, bit, tree_pred, tree_field)
DESC		*r_desc;
DESC		*c_desc;
char		*key;
char		*tuple;
struct tree	*tkey;
int		bit;
int		(*tree_pred)();
int		(*tree_field)();
{
	TID		lotid, hitid;
	register int	i;

#	ifdef xZTR1
	if (tTf(43, 4))
		printf("del_all(bit=0%o)\n", bit);
#	endif

	if (i = find(c_desc, EXACTKEY, &lotid, &hitid, key))
		syserr("del_all: find %d", i);
	while (!(i = get(c_desc, &lotid, &hitid, tuple, TRUE)))
	{
		if (!kcompare(c_desc, tuple, key))
		{
			/* for each constraint of for a relation */
			if (i = delete(c_desc, &lotid))
				syserr("del_all: delete %d", i);
			/* for crash recovery */
			if (i = flush_rel(c_desc, FALSE))
				syserr("del_all: flush_rel %d", i);
			/* if there is a tree tuple, destroy it */
			if ((*tree_pred)(tuple))
				del_tree(tkey, (*tree_field)(tuple));
		}
	}
	if (i != 1)
		syserr("del_all: get %d", i);
	/* turn off bit in relstat field */
	r_relstat(r_desc, bit, 0);
}
/*
**  DEL_INT -- delete from a constraint catalog a constraint
**
**	Parameters:
**		c_desc -- catalog descriptor
**		key -- catalog key
**		tuple -- catalog tuple (needed because tuple size unknown here)
**		tkey -- tree key with TREERELID and TREERELOWNER setkeyed
**		constid -- integer constraint id in string form
**		constattno -- attno of comstraint number in c_desc
**		tree_pred -- predicate on existence of tree tuple 
**		tree_field -- returns treeid from constrain tuple
**
**	Returns:
**		none
**
**	Side Effects:
**		constraint and tree catalog activity.
**		*constid set to 0 if constraint id exists.
**
**	Requires:
**		del_tree()
**
**	Called By:
**		dest_????
**
**	Trace Flags:
**		43, 5
**
**	Syserrs:
**		bad atoi (parser error), getequal, delete, flush_rel
**
**	History:
**		1/10/79 -- (marc) written
*/

del_int(c_desc, key, tuple, tkey, constid, constattno, tree_pred, tree_field)
DESC		*c_desc;
char		*key;
char		*tuple;
struct tree	*tkey;
char		*constid;
int		constattno;
int		(*tree_pred)();
int		(*tree_field)();
{
	TID		tid;
	register int	i;
	int		constnum;

#	ifdef xZTR1
	if (tTf(43, 5))
		printf("del_int(constid=%s, constattno=%d)\n", 
	 	constid, constattno);
#	endif

	if (i = atoi(constid, &constnum))
		syserr("del_int: bad atoi \"%s\" %d", constid, i);
	setkey(c_desc, key, &constnum, constattno);
	if (!(i = getequal(c_desc, key, tuple, &tid)))
	{
		if (i = delete(c_desc, &tid))
			syserr("del_int(%d) %d", constid, i);
		if ((*tree_pred)(tuple))
			del_tree(tkey, (*tree_field)(tuple));
		*constid = '\0';
		return;
	}
	else if (i != 1)
		syserr("dest_int: getequal %d", i);
	/* bad constnum */
}
/*
**  DEST_TREE -- destroy a tree tuple with for a given treeid
**
**	Deletes all tuples from tree with 'treeid' and previously set
**	keys.
**
**	Parameters:
**		key -- tre key
**		treeid -- integer treeid
**
**	Returns:
**		none
**
**	Side Effects:
**		tree activity
**
**	Trace Flags:
**		43, 6
*/

del_tree(key, treeid)
struct tree	*key;
int		treeid;
{
	struct tree	tuple;
	TID		lotid, hitid;
	register int	i;
	register int	flag;
	extern DESC	Treedes;

#	ifdef xZTR1
	if (tTf(43, 6))
		printf("del_tree(treeid=%d)\n", treeid);
#	endif

	setkey(&Treedes, key, &treeid, TREEID);
	if (i = find(&Treedes, EXACTKEY, &lotid, &hitid, key))
		syserr("del_tree: bad find %d treeid %d", i, treeid);
	flag = 0;
	while (!(i = get(&Treedes, &lotid, &hitid, &tuple, TRUE)))
	{
		if (!kcompare(&Treedes, &tuple, key))
		{
			if (i = delete(&Treedes, &lotid))
				syserr("del_tree: delete treeid %d %d", treeid, i);
			if (!flag)
				flag++;
		}
	}
	if (i != 1)
		syserr("del_tree: bad get %d", i);
	if (!flag)
		syserr("del_tree: no tuples qualified treeid %d", treeid);
	if (i = flush_rel(&Treedes, FALSE))
		syserr("del_tree: flush_rel(&Treedes) %d", i);
}
/*
**  CHK_CONST -- check constraint catlg for tuples for a rel, and reset relatin.relstat
**
**	Parameters:
**		r_desc -- reon desc for de-constrained relation
**		c_desc -- catalog desc
**		key -- catalog key (here unknown size)
**		tuple -- " tuple space " " " " "
**		relid -- relation name
**		id_attno -- attno of relid
**		relowner -- relation owner
**		own_attno -- relowner attno
**		bit -- bits to reset in relstat if there are no constraints left
**
**	Returns:
**		none
**
**	Side Effects:
**		reads catalog, maybe changes relstat field of relation
**		relations's r_desc tuple
**
**	Trace Flags:
**		43, 7
*/

chk_const(r_desc, c_desc, key, tuple, relid, id_attno, relowner, own_attno, bit)
DESC	*r_desc;
DESC	*c_desc;
char	*key;
char	*tuple;
char	*relid;
int	id_attno;
char	*relowner;
int	own_attno;
int	bit;
{
	TID		tid;
	register int	i;


#	ifdef xZTR1
	if (tTf(43, 7))
		printf("chk_const: relid %s id_attno %d relowner %s own_attno %d bit 0%o)\n",
		relid, id_attno, relowner, own_attno, bit);
#	endif

	clearkeys(c_desc);
	setkey(c_desc, key, relid, id_attno);
	setkey(c_desc, key, relowner, own_attno);
	if ((i = getequal(c_desc, key, tuple, &tid)) == 1)
		r_relstat(r_desc, bit, 0);
	else if (i < 0)
		syserr("chk_const: getequal %d", i);
}
/*
**  R_RELSTAT -- set or reset bits in the relation.relstat field
**
**	Does the above for relation described by desc.
**
**	Parameters:
**		d -- relation to have relation.relstat field changed
**		bit -- bits to set or reset
**		action -- 0 reset, 1 set
**
**	Returns:
**		none
**
**	Side Effects:
**		relation is opened for READ/WRITE, relstat changed
**
**	Trace Flags:
**		43, 8
*/


r_relstat(d, bit, action)
register DESC	*d;
int		bit;
int		action;
{
	struct relation	tuple, key;
	TID		tid;
	register int	i;
	extern DESC	Reldes;

#	ifdef xZTR1
	if (tTf(43, 8))
		printf("r_relstat(bit=0%o, action %d)\n",
		bit, action);
#	endif

	opencatalog("relation", 2);
	clearkeys(&Reldes);
	setkey(&Reldes, &key, d->reldum.relid, RELID);
	setkey(&Reldes, &key, d->reldum.relowner, RELOWNER);
	if (i = getequal(&Reldes, &key, &tuple, &tid))
		syserr("r_relstat: getequal %s, %s, %d", d->reldum.relid,
		d->reldum.relowner, i);
	if (action)
	{
		if (tuple.relstat == (i = tuple.relstat | bit))
			return;
		tuple.relstat = i;
	}
	else
	{
		if (tuple.relstat == (i = tuple.relstat & ~bit))
			return;
		tuple.relstat = i;
	}
	if ((i = replace(&Reldes, &tid, &tuple, 0)) < 0 || i == 2)
		syserr("r_relstat: replace %d", i);
	if (i = flush_rel(&Reldes, FALSE))
		syserr("r_relstat: flush_rel(&Reldes) %d", i);
}
/*
**  TREE_CONST -- True predicate
**
**	Called indirectly by routines wishing to know if
**	a integrity constraint has an associated tree tuple.
**	As this is always the case, returns TRUE always.
**
**	Parameters:
**		i -- integrity tuple
**
**	Returns:
**		TRUE
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		43, 9
*/

tree_const(i)
struct integrity	*i;
{
#	ifdef xZTR1
	if (tTf(43, 9))
		printf("tree_const()\n");
#	endif

	return (TRUE);
}
/*
**  TREE_PROT -- Protection tuple tree predicate
**
**	Called indirectly by routines wishing to know if
**	a protection constraint has an associated tree tuple.
**
**	Parameters:
**		p -- protect tuple
**
**	Returns:
**		TRUE -- if p->protree != -1
**		FLASE -- otherwise
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		43, 9
*/

tree_prot(p)
struct protect	*p;
{
#	ifdef xZTR1
	if (tTf(43, 9))
		printf("tree_prot(p->protree=%d)\n", p->protree);
#	endif

	if (p->protree == -1)
		return (FALSE);
	else
		return (TRUE);
}
/*
**  PROT_PROTREE -- get protree field of a protection tuple
**
**	Parameters:
**		p -- protect tuple
**
**	Returns:
**		p->protree
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		43, 9
*/

prot_protree(p)
struct protect	*p;
{
#	ifdef xZTR1
	if (tTf(43, 9))
		printf("prot_protree(protree=%d)\n", p->protree);
#	endif

	return (p->protree);
}
/*
**  INT_INTTREE -- get inttree field of a integrity tuple
**
**	Parameters:
**		i -- integrity tuple
**
**	Returns:
**		i->inttree
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		43, 9
*/

int_inttree(i)
struct integrity	*i;
{
#	ifdef xZTR1
	if (tTf(43, 9))
		printf("int_inttree(inttree=%d)\n", i->inttree);
#	endif

	return (i->inttree);
}
