# include	<ingres.h>
# include	<aux.h>
# include	<access.h>
# include	<symbol.h>
# include	<sccs.h>

SCCSID(@(#)openr.c	7.1	2/5/81)

/*
**  OPENR -- Open a relation into a descriptor
**
**	Openr will open the named relation into the given descriptor
**	according to the mode specified. When searching for a name,
**	a relation owner by the current user will be searched for first.
**	If none is found then one owned by the DBA will be search for.
**
**	There are several available modes for opening a relation. The
**	most common are
**		mode 0 -- open for reading
**		mode 2 -- open for writing (mode 1 works identically).
**	Other modes which can be used to optimize performance:
**		mode -1 -- get relation-relation tuple and tid only.
**      			Does not open the relation.
**		mode -2 -- open relation for reading after a previous
**      			call of mode -1.
**		mode -3 -- open relation for writing after a previous
**      			call of mode -1.
**		mode -4 -- open relation for reading. Assumes that relation
**      			was previously open (eg relation & attributed
**      			have been filled) and file was closed by closer.
**		mode -5 -- open relation for writing. Same assumptions as
**      			mode -4.
**
**	Parameters:
**		dx - a pointer to a struct descriptor (defined in ingres.h)
**		mode - can be 2,0,-1,-2,-3,-4,-5
**		name - a null terminated name (only first 12 chars looked at)
**
**	Returns:
**		1 - relation does not exist
**		0 - ok
**		<0 - error. Refer to the error codes in access.h
**
**	Side Effects:
**		Opens the physical file if required. Fill the
**		descriptor structure. Initializes the access methods
**		if necessary.
**
**	Trace Flags:
**		90
*/


openr(d, mode, name)
register DESC	*d;
int		mode;
char		*name;
{
	register int	retval, filemode;
	char		filename[MAXNAME+3];

#	ifdef xATR1
	if (tTf(21, 0))
		printf("openr:%.12s,%d\n", name, mode);
#	endif
#	ifdef xATM
	if (tTf(76, 2))
		timtrace(21, 0);
#	endif

	/* init admin */
	acc_init();

	/* process according to mode */

	filemode = 0;
	switch (mode)
	{

	  case -1:
		retval = get_reltup(d, name);
		break;

	  case 1:
	  case 2:
		filemode = 2;

	  case 0:
		if (retval = get_reltup(d, name))
			break;

	  case -2:
	  case -3:
		if (retval = get_attuples(d))
			break;

	  case -5:
		if (mode == -3 || mode == -5)
			filemode = 2;

	  case -4:
		clearkeys(d);
		/* descriptor is filled. open file */
		ingresname(d->reldum.relid, d->reldum.relowner, filename);
		/* can't open a view */
		if (d->reldum.relstat & S_VIEW)
		{
			retval = acc_err(AMOPNVIEW_ERR);	/* view */
			break;
		}
		if ((d->relfp = open(filename, filemode)) < 0)
		{
			retval = acc_err(AMNOFILE_ERR);	/* can't open file */
			break;
		}
		d->relopn = (d->relfp + 1) * 5;
		if (filemode == 2)
			d->relopn = -d->relopn;
		d->reladds = 0;
		retval = 0;
		break;

	  default:
		syserr("openr:bd md=%d", mode);
	}

	/* return */

#	ifdef xATR1
	if (tTf(21, 4) && mode != -1 && retval != 1)
		printdesc(d);
	if (tTf(21, 0))
		printf("openr rets %d\n", retval);
#	endif

#	ifdef xATM
	if (tTf(76, 2))
		timtrace(22, 0);
#	endif
	return (retval);
}
/*
**  GET_ATTUPLES -- get tuples from attribute relation for this relation
*/

get_attuples(d)
register DESC	*d;
{
	struct attribute	attr, attkey;
	register int		i, dom;
	int			numatts;
	TID			tid1, tid2;

	clearkeys(&Admin.adattd);

	/* zero all format types */
	for (i = 0; i <= d->reldum.relatts; i++)
		d->relfrmt[i] = 0;

	/* prepare to scan attribute relation */
	setkey(&Admin.adattd, (char *) &attkey, d->reldum.relid, ATTRELID);
	setkey(&Admin.adattd, (char *) &attkey, d->reldum.relowner, ATTOWNER);
	if (i = find(&Admin.adattd, EXACTKEY, &tid1, &tid2, &attkey))
		return (i);

	numatts = d->reldum.relatts;

	while (numatts && !get(&Admin.adattd, &tid1, &tid2, &attr, TRUE))
	{

		/* does this attribute belong? */
		if (bequal(&attr, &attkey, MAXNAME + 2))
		{

			/* this attribute belongs */
			dom = attr.attid;	/* get domain number */

			if (d->relfrmt[dom])
				break;	/* duplicate attribute. force error */

			numatts--;
			d->reloff[dom] = attr.attoff;
			d->relfrmt[dom] = attr.attfrmt;
			d->relfrml[dom] = attr.attfrml;
			d->relxtra[dom] = attr.attxtra;
		}
	}

	d->relfrmt[0] = INT;
	d->relfrml[0] = 4;
	/* make sure all the atributes were there */
	for (dom = 1; dom <= d->reldum.relatts; dom++)
		if (d->relfrmt[dom] == 0)
			numatts = 1;	/* force an error */
	if (numatts)
		i = acc_err(AMNOATTS_ERR);

	flush_rel(&Admin.adattd, TRUE);

#	ifdef xATR1
	if (tTf(21, 3))
		printf("get_attr ret %d\n", i);
#	endif

	return (i);
}
