# include	<pv.h>
# include	<ingres.h>
# include	<access.h>
# include	<aux.h>
# include	<catalog.h>
# include	<symbol.h>
# include	<lock.h>
# include	<func.h>
# include	<sccs.h>

SCCSID(@(#)create.c	7.1	2/5/81)

extern	short	tTdbu[];
extern	int	create();
extern	int	null_fn();

struct fn_def CreateFn =
{
	"CREATE",
	create,
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
**  CREATE -- create new relation
**
**	This module creates a brand new relation in the current
**	directory (database).  The relation is always created as
**	a paged heap.  It may not redefine an existing relation,
**	or rename a system catalog.
**
**	Trace Flags:
**		31
*/


struct domain
{
	char	*name;
	char	frmt;
	char	frml;
};

/*
**  CREATE -- create new relation
**
**	This routine is the driver for the create module.
**
**	Parameters:
**		pc -- parameter count
**		pv -- parameter vector:
**			0 -- relation status (relstat) -- stored into
**				the 'relstat' field in the relation
**				relation, and used to determine the
**				caller.  Interesting bits are:
**
**				S_INDEX -- means called by the index
**					processor.  If set, the 'relindxd'
**					field will also be set to -1
**					(SECINDEX) to indicate that this
**					relation is a secondary index.
**				S_CATALOG -- this is a system catalog.
**					If set, this create was called
**					from creatdb, and the physical
**					file is not created.  Also, the
**					expiration date is set infinite.
**				S_VIEW -- this is a view.  Create has
**					been called by the 'define'
**					statement, rather than the
**					'create' statement.  The physical
**					file is not created.
**
**			1 -- relation name.
**			2 -- attname1
**			3 -- format1
**			4, etc -- attname, format pairs.
**
**	Returns:
**		zero -- successful create.
**		else -- failure somewhere.
**
**	Side Effects:
**		A relation is created (this is a side effect?).  This
**		means entries in the 'relation' and 'attribute' cata-
**		logs, and (probably) a physical file somewhere, with
**		one page already in it.
**
**	Trace Flags:
**		31
*/

create(pc, pv)
int	pc;
PARM	pv[];
{
	register PARM		*pp;
	register int		i;
	int			bad;
	struct domain		domain[MAXDOM];
	struct domain		*dom;
	char			*relname, tempname[MAXNAME+3];
	struct tup_id		tid;
	struct relation		rel, key;
	struct attribute	att;
	DESC			desr;
	extern char		*Usercode;
	extern DESC		Reldes, Attdes;
	extern int		errno;
	register int		relstat;
	long			temptid;
	long			npages;
	int			fdes;
	bool			internal;

#	ifdef xZTR1
	if (tTf(31, -1))
	{
		printf("creating %s\n", pv[1].pv_val.pv_str);
	}
#	endif
	pp = pv;
	relstat = oatoi(pp[0].pv_val.pv_str);
	/*
	**	If this database has query modification, then default
	**	to denial on all user relations.
	**	(Since views cannot be protected, this doesn't apply to them)
	*/
	if ((Admin.adhdr.adflags & A_QRYMOD) && ((relstat & (S_VIEW || S_CATALOG)) == 0))
		relstat |= (S_PROTALL | S_PROTRET);
	relname = (++pp)->pv_val.pv_str;
	internal = bequal(relname, "_SYS", 4);
	ingresname(relname, Usercode, rel.relid);
	bmove(rel.relid, att.attrelid, MAXNAME + 2);
	opencatalog("relation", 2);

	/* check for duplicate relation name */
	if ((relstat & S_CATALOG) == 0)
	{
		if (openr(&desr, -1, relname) == 0)
		{
			if (bequal(desr.reldum.relowner, rel.relowner, 2))
			{
				return (error(5102, relname, 0));	/* bad relname */
			}
			if (desr.reldum.relstat & S_CATALOG)
			{
				return (error(5103, relname, 0));	/* attempt to rename system catalog */
			}
		}
	}
	opencatalog("attribute", 2);

	/* initialize structures for system catalogs */
	initstructs(&att, &rel);
	rel.relstat = relstat;
	if ((relstat & S_CATALOG) != 0)
		rel.relsave = 0;
	else if ((relstat & S_INDEX) != 0)
		rel.relindxd = SECINDEX;
	
#	ifdef xZTR3
	if (tTf(31, 2))
	{
		printf("\nrel->relprim = %D\n", rel.relprim);
		printup(&Reldes, &rel);
	}
#	endif

	/* check attributes */
	pp++;
	for (i = pc - 2; i > 0; i -= 2)
	{
		bad = chk_att(&rel, pp[0].pv_val.pv_str, pp[1].pv_val.pv_str, domain, internal);
		if (bad != 0)
		{
			return (error(bad, relname, pp[0].pv_val.pv_str, pp[1].pv_val.pv_str, 0));
		}
		pp += 2;
	}

	/*
	** Create files if appropriate. Concurrency control for
	** the create depends on the actual file. To prevent
	** to users with the same usercode from creating the
	** same relation at the same time, their is check
	** on the existence of the file. The important events are
	** (1) if a tuple exists in the relation relation then
	** the relation really exists. (2) if the file exists then
	** the relation is being created but will not exist for
	** use until the relation relation tuple is present.
	** For VIEWS, the file is used for concurrency control
	** during the create but is removed afterwards.
	*/
	if ((relstat & S_CATALOG) == 0)
	{
		/* for non system named temporary relations
		** set a critical section lock while checking the
		** existence of a file.  If it exists, error return(5102)
		** else create file.
		*/
		temptid = 0;
		if (Lockrel && (!bequal(rel.relid,"_SYS",4)))
		{
			temptid = -1;
			setcsl(temptid);	/* set critical section lock */
			if ((fdes = open(rel.relid,0)) >= 0)
			{
						/* file already exists */
				close(fdes);
				unlcs(temptid);	/* release critical section lock */
				return (error(5102, relname, 0));
			}
			errno = 0;	/* file doesn't exist */
		}
		ingresname(rel.relid, rel.relowner, tempname);
		desr.relfp = creat(tempname, FILEMODE);
		if (temptid != 0)
			unlcs(temptid);	/* release critical section lock */
		if (desr.relfp < 0)
			syserr("create: creat %s", rel.relid);
		desr.reltid.ltid = -1L;	/* init reltid to unused */
		if ((relstat & S_VIEW) == 0)
		{
			npages = 1;
			if (i = formatpg(&desr, npages))
				syserr("create: formatpg %d", i);
		}

		close(desr.relfp);
	}

	/* insert attributes into attribute relation */
	pp = pv + 2;
	dom = domain;
	for (i = pc - 2; i > 0; i -= 2)
	{
		ins_att(&Attdes, &att, dom++);
		pp += 2;
	}

	/*
	** Flush the attributes. This is necessary for recovery reasons.
	** If for some reason the relation relation is flushed and the
	** machine crashes before the attributes are flushed, then recovery
	** will not detect the error.
	** The call below cannot be a "noclose" without major changes to
	** creatdb.
	*/
	if (i = pageflush(0))
		syserr("create:flush att %d", i);

	if (i = insert(&Reldes, &tid, &rel, FALSE))
		syserr("create: insert(rel, %.14s) %d", rel.relid, i);

	if (relstat & S_VIEW)
		unlink(tempname);
	return (0);
}



/*
**  CHK_ATT -- check attribute for validity
**
**	The attribute is checked to see if
**	* it's name is ok (within MAXNAME bytes)
**	* it is not a duplicate name
**	* the format specified is legal
**	* there are not a ridiculous number of attributes
**	  (ridiculous being defined as anything over MAXDOM - 1)
**	* the tuple is not too wide to fit on one page
**
**	Parameters:
**		rel -- relation relation tuple for this relation.
**		attname -- tentative name of attribute.
**		format -- tentative format for attribute.
**		domain -- a 'struct domain' used to determine dupli-
**			cation, and to store the resulting name and
**			format in.
**
**	Returns:
**		zero -- OK
**		5104 -- bad attribute name.
**		5105 -- duplicate attribute name.
**		5106 -- bad attribute format.
**		5107 -- too many attributes.
**		5108 -- tuple too wide.
**
**	Side Effects:
**		'rel' has the relatts and relwid fields updated to
**		reflect the new attribute.
**
**	Trace Flags:
**		31
*/

chk_att(rel, attname, format, domain, internal)
struct relation	*rel;
char		*attname, *format;
struct domain	domain[];
bool		internal;
{
	register int			i;
	register struct relation	*r;

	r = rel;

#	ifdef xZTR3
	if (tTf(31, 1))
		printf("chk_att %s %s\n", attname, format);
#	endif

	if (sequal(attname, "tid"))
		return (5104);		/* bad attribute name */
	if ((i = dup_att(attname, r->relatts, domain)) < 0)
		return (5105);		/* duplicate attribute */
	if (formck(format, &domain[i], internal))
		return (5106);		/* bad attribute format */
	r->relatts++;
	r->relwid += domain[i].frml & 0377;
	if (r->relatts >= MAXDOM)
		return (5107);		/* too many attributes */
	if (r->relwid > MAXTUP && (r->relstat & S_VIEW) == 0)
		return (5108);		/* tuple too wide */
	return (0);
}




/*
**  INS_ATT -- insert attribute into attribute relation
**
**	Parameters:
**		des -- relation descriptor for the attribute catalog.
**		att -- attribute tuple, preinitialized with all sorts
**			of good stuff (everything except 'attname',
**			'attfrmt', and 'attfrml'; 'attid' and 'attoff'
**			must be initialized to zero before this routine
**			is called the first time.
**		dom -- 'struct domain' -- the information needed about
**			each domain.
**
**	Returns:
**		none
**
**	Side Effects:
**		The 'att' tuple is updated in the obvious ways.
**		A tuple is added to the 'attribute' catalog.
**
**	Trace Flags:
**		none currently
*/

ins_att(des, att, dom)
DESC			*des;
struct attribute	*att;
struct domain		*dom;
{
	register int		i;
	struct tup_id		tid;
	register struct domain	*d;

	d = dom;

	pmove(d->name, att->attname, MAXNAME, ' ');
	att->attfrmt = d->frmt;
	att->attfrml = d->frml;
	att->attid++;
	if (insert(des, &tid, att, FALSE))
		syserr("ins_att: insert(att, %s)", d->name);
	att->attoff += att->attfrml & 0377;
}




/*
**  DUP_ATT -- check for duplicate attribute
**
**	The attribute named 'name' is inserted into the 'attalias'
**	vector at position 'count'.  'Count' should be the count
**	of existing entries in 'attalias'.  'Attalias' is checked
**	to see that 'name' is not already present.
**
**	Parameters:
**		name -- the name of the attribute.
**		count -- the count of attributes so far.
**		domain -- 'struct domain' -- the list of domains
**			so far, names and types.
**
**	Returns:
**		-1 -- attribute name is a duplicate.
**		else -- index in 'domain' for this attribute (also
**			the attid).
**
**	Side Effects:
**		The 'domain' vector is extended.
**
**	Trace Flags:
**		none
*/

dup_att(name, count, domain)
char		*name;
int		count;
struct domain	domain[];
{
	register struct domain	*d;
	register int		lim;
	register int		i;

	lim = count;
	d = domain;

	for (i = 0; i < lim; i++)
		if (sequal(name, d++->name))
			return (-1);
	if (count < MAXDOM)
		d->name = name;
	return (i);
}




/*
**  INITSTRUCTS -- initialize relation and attribute tuples
**
**	Structures containing images of 'relation' relation and
**	'attribute' relation tuples are initialized with all the
**	information initially needed to do the create.  Frankly,
**	the only interesting part is the the expiration date
**	computation; longconst(9, 14976) is exactly the number
**	of seconds in one week.
**
**	Parameters:
**		att -- attribute relation tuple.
**		rel -- relation relation tuple.
**
**	Returns:
**		none
**
**	Side Effects:
**		'att' and 'rel' are initialized.
**
**	Requires:
**		time -- to get the current date.
**
**	Called By:
**		create
**
**	Trace Flags:
**		none
**
**	Diagnostics:
**		none
**
**	Syserrs:
**		none
**
**	History:
**		2/27/78 (eric) -- documented.
*/

initstructs(att, rel)
register struct attribute	*att;
register struct relation	*rel;
{
	/* setup expiration date (today + one week) */
	time(&rel->relstamp);
	rel->relsave = rel->relstamp + 604800L;
	rel->relfree = 0;
	rel->reltups = 0;
	rel->relatts = 0;
	rel->relwid = 0;
	rel->relprim = 1;
	rel->relspec = M_HEAP;
	rel->relindxd = 0;
	att->attxtra = 0;
	att->attid = 0;
	att->attoff = 0;
}



/*
**  CHECK ATTRIBUTE FORMAT AND CONVERT
**
**	The string 'a' is checked for a valid attribute format
**	and is converted to internal form.
**
**	zero is returned if the format is good; one is returned
**	if it is bad.  If it is bad, the conversion into a is not
**	made.
**
**	A format of CHAR can be length zero only if this
**	create was generated internally.
*/

formck(a, dom, internal)
char		*a;
struct domain	*dom;
bool		internal;
{
	int			len;
	register int		i;
	char			c;
	register char		*p;
	register struct domain	*d;

	p = a;
	c = *p++;
	d = dom;

	if (atoi(p, &len) != 0)
		return (1);
	i = len;

	switch (c)
	{

	  case INT:
		if (i == 1 || i == 2 || i == 4)
		{
			d->frmt = INT;
			d->frml = i;
			return (0);
		}
		return (1);

	  case FLOAT:
		if (i == 4 || i == 8)
		{
			d->frmt = FLOAT;
			d->frml = i;
			return (0);
		}
		return (1);

	  /* note: should disallow c0 from user (but needed internally) */
	  case CHAR:
		if (i > 255 || i < 0 || (i == 0 && !internal))
			return (1);
		d->frmt = CHAR;
		d->frml = i;
		return (0);
	}
	return (1);

}
