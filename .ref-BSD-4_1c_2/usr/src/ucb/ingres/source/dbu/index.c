# include	<pv.h>
# include	<func.h>
# include	<symbol.h>
# include	<ingres.h>
# include	<aux.h>
# include	<catalog.h>
# include	<access.h>
# include	<lock.h>
# include	<sccs.h>

SCCSID(@(#)index.c	7.1	2/5/81)

extern short	tTdbu[];
extern int	indexx();
extern int	null_fn();

struct fn_def	IndexFn =
{
	"INDEX",
	indexx,
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
**	This is the DBU routine INDEX
**
**  pc = # of parameters
**  pv[0] points to primary relation name
**  pv[1] points to index relation name
**  pv[2] points to domain1
**  pv[3] points to domain2
**  .
**  .
**  .
**  pv[pc] = NULL
**
*/

struct dom
{
	int	id;
	int	off;
	int	frml;
	char	frm[5];
};

indexx(pc, pv)
int	pc;
PARM	pv[];
{
	register int		i;
	int			j;
	register struct dom	*dom;
	register PARM		*p;
	char			*primary, *indx;
	int			ndoms, newpc;
	struct tup_id		tid, hitid;
	struct tup_id		xtid;
	PARM			newpv[MAXKEYS * 2 + 4];
	char			primtup[MAXTUP], systup[MAXTUP];
	DESC			desc, pridesc;
	extern DESC		Reldes;
	extern DESC		Attdes;
	extern DESC		Inddes;
	struct relation		relkey, reltup;
	struct attribute	attkey, atttup;
	struct index		indtup;
	struct dom		domain[MAXKEYS];

	primary = pv[0].pv_val.pv_str;
	indx = pv[1].pv_val.pv_str;
#	ifdef xZTR1
	if (tTf(33, -1))
		printf("index: (pri %s ind %s)\n", primary, indx);
#	endif
	i = openr(&pridesc, 0, primary);
	if (i == AMOPNVIEW_ERR)
		return (error(5306, primary, 0));
	if (i > 0)
		return (error(5300, primary, 0));
	if (i < 0)
		syserr("INDEX : openr (%.14s) %d", primary, i);

	if (!bequal(pridesc.reldum.relowner, Usercode, 2))
	{
		i = 5303;
	}
	else if (pridesc.reldum.relstat & S_CATALOG)
	{
		i = 5305;
	}
	else if (pridesc.reldum.relindxd < 0)
	{
		i = 5304;
	}

	if (i)
	{
		closer(&pridesc);
		return (error(i, primary, 0));
	}
	/*
	**  GATHER INFO. ON DOMAINS
	*/
	opencatalog("attribute", 2);
	setkey(&Attdes, &attkey, primary, ATTRELID);
	setkey(&Attdes, &attkey, pridesc.reldum.relowner, ATTOWNER);
	pc -= 2;
	p = &pv[2];
	dom = domain;
	for (i = 0; i < pc; i++)
	{
		if (i >= MAXKEYS)
		{
			closer(&pridesc);
			return (error(5301, (p->pv_val).pv_str, primary, 0));	/* too many keys */
		}
		setkey(&Attdes, &attkey, (p->pv_val).pv_str, ATTNAME);
		j = getequal(&Attdes, &attkey, &atttup, &tid);
		if (j < 0)
			syserr("INDEX: geteq att %d", j);
		if (j)
		{
			closer(&pridesc);
			return (error(5302, (p->pv_val).pv_str, 0));	/* key not in relation */
		}
		dom->id = atttup.attid;
		dom->off = atttup.attoff;
		dom->frml = atttup.attfrml & 0377;
		dom->frm[0] = atttup.attfrmt;
		p++;
		dom++;
	}
	ndoms = i;
	noclose(&Attdes);

	/*
	** The "order" of the steps have been altered to improve
	** recovery possibilities
	*/
	/*
	**  STEP 1 & 2: CREATE INDEX RELATION.
	*/
	newpv[0].pv_val.pv_str = "0202";
	newpv[1].pv_val.pv_str = indx;
	newpc = 2;
	p = &pv[2];
	dom = domain;
	for (i = 0; i < pc; i++)
	{
		newpv[newpc++].pv_val.pv_str = (p->pv_val).pv_str;
		itoa(dom->frml, &dom->frm[1]);
		newpv[newpc++].pv_val.pv_str = dom->frm;
		dom++;
		p++;
	}
	newpv[newpc++].pv_val.pv_str = "tidp";
	newpv[newpc++].pv_val.pv_str = "i4";
	newpv[newpc].pv_type = PV_EOF;

	if (create(newpc, newpv))
		return (-1);

	/* This is done for concurrency reasons */
	if (noclose(&Reldes))
		syserr("index: noclose");

	/*
	**  STEP 5: FILL UP THE SECONDARY INDEX FILE ITSELF
	*/
	if (Lockrel)
		/* set a shared relation lock */
		setrll(A_SLP, *(long *) &pridesc.reltid, M_SHARE);	/* pardon the kludge  */
	if (i = openr(&desc, 2, indx))
		syserr("INDEX: openr %.14s %d", indx, i);
	find(&pridesc, NOKEY, &tid, &hitid);
	while ((i = get(&pridesc, &tid, &hitid, primtup, TRUE)) == 0)
	{
		dom = domain;
		for (i = j = 0; j < ndoms; j++)
		{
			bmove(&primtup[dom->off], &systup[i], dom->frml);
			i += dom->frml;
			dom++;
		}
		bmove(&tid, &systup[i], sizeof tid);		/* move in pointer */
		if ((j = insert(&desc, &xtid, systup, TRUE)) < 0)
			syserr("INDEX: insert %.14s %d", indx, j);
	}
	if (i < 0)
		syserr("INDEX: get %.14s %d", primary, i);
	closer(&pridesc);
	closer(&desc);


	/*
	**  STEP 3: ENTRIES TO INDEX-REL
	*/
	pmove(primary, indtup.irelidp, MAXNAME, ' ');		/* mv in primary name  */
	bmove(pridesc.reldum.relowner, indtup.iownerp, 2);	/* primary owner */
	pmove(indx, indtup.irelidi, MAXNAME, ' ');		/* index name */
	indtup.irelspeci = M_HEAP;
	for (i = 0; i < MAXKEYS; i++)
		indtup.idom[i] = (i < ndoms) ? domain[i].id : 0;
	opencatalog("indexes", 2);
	if ((i = insert(&Inddes, &tid, &indtup, TRUE)) < 0)
		syserr("INDEX: insert ix %d", i);

	/*
	**  STEP 4: TURN BIT ON IN PRIMARY RELATION TO SHOW IT IS BEING INDEXED
	*/
	opencatalog("relation", 2);
	setkey(&Reldes, &relkey, primary, RELID);
	setkey(&Reldes, &relkey, pridesc.reldum.relowner, RELOWNER);
	if (i = getequal(&Reldes, &relkey, &reltup, &tid))
		syserr("INDEX: geteq rel %d", i);
	reltup.relindxd = SECBASE;
	if ((i = replace(&Reldes, &tid, &reltup, TRUE)) < 0)
		syserr("INDEX: replace rel %d", i);

	if (Lockrel)
		unlrl(*(long *) &pridesc.reltid);	/* release relation lock */

	return (0);
}
