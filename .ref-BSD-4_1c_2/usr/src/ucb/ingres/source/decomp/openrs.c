# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)openrs.c	7.1	2/5/81)




/* Defined constants for dtmode field above */
# define	DTALLOC		0	/* descriptor allocated */
# define	DTREL		1	/* has been openr'd -1 */
# define	DTATTS		2	/* has rel+atts but not opened */
# define	DTREAD		3	/* currently open for reading */
# define	DTWRITE		4	/* currently open for writing */



/* Allocation of descriptors */

/* Globals which count #files open and maximum # of files which can be open */


/*
** OPENRS -- routines associated with maintaining the range table for decomp
**
**	openrs(root) -- fill range table info about each relation.
**
**	closers() -- close all variables in range table.
**
**	openr1(varno) -- fill range table for a particular relation.
**
**	closer1(varno) -- close a particular relation.
**
**	readopen(varno) -- open a variable for reading. returns descriptor.
**
**	writeopen(varno) -- open a variable for writing. returns descriptor.
**
**	initdesc()	-- initialize the descriptor cache.
**
**	reldescrip(varno) -- returns descriptor for var (has rel/atts
**		but might not be open).
**
**	desc_get(relnum, flag) -- finds a desc_tab & alloctes it for relnum.
**
**	desc_lru()  -- returns least recently used desc_tab.
**
**	desc_top(desc_tab) -- makes desc_tab most recently used.
**
**	desc_last(desc_tab) -- makes desc_tab the least recently used.
**
**	Trace Flags:
**		62
*/
/*
** Initdesc -- initialize descriptors for range table
*/

initdesc(mode)
int	mode;
{
	register struct desc_tab	*dt;
	register int			i;
	extern int			Equel;


	for (dt = De.de_desc, i = 0; dt <= &De.de_desc[MAXRELN - 1]; dt++, i++)
	{
		dt->dtmode = DTALLOC;
		dt->relnum = -2;	/* unused relnum value */
		dt->dtpos = i;		/* lru order */
	}

	/*
	** Determine number of available file descriptors.
	**	av_files gives number of files that are def. open
	**	for users.  if we will need to open a batch file,
	**	get rid of that also.
	*/

	De.de_dfiles = av_files();
	if (mode != mdRETR)
		De.de_dfiles--;
	De.de_dopnfiles = 0;
}
/*
**	Openrs -- open source relations for query. Fill values
**		in range table.
*/

openrs(root)
QTREE	*root;
{
	register QTREE	*r;
	register int	map, i;
	DESC		*openr1();

	r = root;
	map = r->sym.value.sym_root.lvarm | r->sym.value.sym_root.rvarm;

#	ifdef xDTR1
	if (tTf(62, 0))
		printf("OPENRS-root:%x,map:%o\n", r, map);
#	endif

	for (i = 0; i < MAXRANGE; i++)
		if (map & (01 << i))
			openr1(i);

}
/*
**	Close all open relations.
**	If any relations were created but never
**	opened, destroy them. The only
**	situation under which that can occur
**	is when a rub-out occurs at an
**	in opportune moment or when an error
**	occurs in ovqp.
*/

closers()
{
	register int			i;
	register struct desc_tab	*dt;
	bool				dstr_flag;


	for (dt = De.de_desc; dt <= &De.de_desc[MAXRELN - 1]; dt++)
		desc_close(dt);

	/* destroy any temps */
	initp();	/* init parameters vector for destroys */
	dstr_flag = FALSE;
	while (i = rnum_last())
	{
		dstr_flag |= dstr_mark(i); /* indicate that there are relations to be destroyed */
	}

	if (dstr_flag)
		call_dbu(mdDESTROY, TRUE);
	else
		resetp();
}
/*
**	Openr1 -- open relation to get relation relation tuple
**
**	This will not open the relation for reading -- only
**	for getting the first part of the descriptor filled
*/

DESC *
openr1(var)
int	var;
{
	register struct desc_tab	*dt;
	register struct rang_tab	*rp;
	register DESC	*d;
	int				i;
	struct desc_tab			*desc_get();
	extern char			*rnum_convert();

	rp = &De.de_rangev[var];

#	ifdef xDTR1
	if (tTf(62, 2))
		printf("openr1: var %d (%s)\t", var, rnum_convert(rp->relnum));
#	endif

	dt = desc_get(rp->relnum, TRUE);

	if (dt->dtmode == DTALLOC)
	{
		if (i = openr(&dt->desc, -1, rnum_convert(rp->relnum)))
			syserr("openr1 open %d %s", i, rnum_convert(rp->relnum));
		dt->dtmode = DTREL;
	}

#	ifdef xDTR1
	if (tTf(62, 2))
		printf("tups=%ld\n", dt->desc.reldum.reltups);
#	endif

	d = &dt->desc;

	rp->rtspec = d->reldum.relspec;
	rp->rtstat = d->reldum.relstat;
	rp->rtwid = d->reldum.relwid;
	rp->rtcnt = d->reldum.reltups;

	return (d);
}
/*
**  CLOSER1
*/

closer1(var)
int	var;
{
	register struct desc_tab	*dt;
	register struct rang_tab	*rp;
	register int			i;
	struct desc_tab			*desc_get();
	struct desc_tab			*desc_last();

	i = var;
	rp = &De.de_rangev[i];

#	ifdef xDTR1
	if (tTf(62, 4))
		printf("closer1:var %d (%s)\n", i, rnum_convert(rp->relnum));
#	endif
	if (dt = desc_get(rp->relnum, FALSE))
	{

		/* currently a descriptor for rel */
		desc_close(dt);

		dt->relnum = -2;
		desc_last(dt);

	}
}
/*
**  READOPEN
*/

DESC *
readopen(var)
int	var;
{
	register struct desc_tab	*dt;
	struct desc_tab			*desc_get();

	/* get descv for the relation */
	dt = desc_get(De.de_rangev[var].relnum, TRUE);

	if (!(dt->dtmode == DTREAD || dt->dtmode == DTWRITE))
	{
		/* not open for reading or writing */
		openup(dt, var, 0);	/* open for reading */
	}

	return (&dt->desc);
}
/*
**  WRITEOPEN
*/

DESC *
writeopen(var)
int	var;
{
	register struct desc_tab	*dt;

	/* get descv for the relation */
	dt = desc_get(De.de_rangev[var].relnum, TRUE);

	if (dt->dtmode != DTWRITE)
	{
		/* not open for writing */
		openup(dt, var, 2);	/* open for reading */
	}

	return (&dt->desc);
}
/*
**  SPECOPEN -- open for writing not associated with any variable
*/

DESC *
specopen(relnum)
int	relnum;
{
	register struct desc_tab	*dt;
	struct desc_tab			*desc_get();

	dt = desc_get(relnum, TRUE);

	if (dt->dtmode != DTWRITE)
		openup(dt, -1, 2);

	return (&dt->desc);
}
/*
**  SPECCLOSE
*/

specclose(relnum)
int	relnum;
{
	register struct desc_tab	*dt;
	struct desc_tab			*desc_get();
	struct desc_tab			*desc_last();

	if (dt = desc_get(relnum, FALSE))
	{
		desc_close(dt);
		desc_last(dt);
		dt->relnum = -2;
	}
}
/*
**	Openup -- make sure that the given descriptor is open
**		suitably for reading or writing.
*/

openup(dt1, varno, mode)
struct desc_tab	*dt1;
int		varno;
int		mode;
{
	register struct desc_tab	*dt;
	register int			md, openmd;
	int				i;
	extern char			*rnum_convert();
	char				rnam_tmp[MAXNAME+3];

	/* quick check to handle typical case of rel being already open */
	md = mode;
	dt = dt1;
	if ((md != 2 && dt->dtmode == DTREAD) || dt->dtmode == DTWRITE)
		return;

	/* relation not opened correctly */
	switch (dt->dtmode)
	{

	  case DTALLOC:
		/*
		** Descriptor allocated but nothing else. If this
		** is for a variable then use openr1 to get range table
		** info. Else open directly.
		*/
		if (varno < 0)
		{
			/* open unassociated with a range table variable */
			openmd = md ? 2 : 0;
			bmove(rnum_convert(dt->relnum), dt->desc.reldum.relid, MAXNAME);
			break;
		}

		/* open for range table variable */
		openr1(varno);

		/* now fall through to DTREL case */

	  case DTREL:
		/* relation relation tuple present but nothing else */
		openmd = md ? -3 : -2;	/* open -2 for read, -3 for write */
		break;

	  case DTATTS:
		/* relation & attributes filled but relation closed */
		openmd = md ? -5 : -4;
		break;
	  case DTREAD:
		/* relation open for reading but we need to write */
		desc_close(dt);

		openmd = -5;
		break;

	  default:
		syserr("openup:bad md %d", dt->dtmode);
	}

	/* close a previous file if necessary */
	if (De.de_dopnfiles == De.de_dfiles)
		desc_victum();	/* close oldest file */

	/* now open relation */
	bmove(dt->desc.reldum.relid, rnam_tmp, MAXNAME + 3);
	if (i = openr(&dt->desc, openmd, rnam_tmp))
		syserr("openup:openr %d,%d,%.12s,%s", i, openmd, rnam_tmp, rnum_convert(dt->relnum));
	De.de_dopnfiles++;

	/* update mode of descriptor */
	dt->dtmode = md ? DTWRITE : DTREAD;
}
/*
**  DESC_GET
*/

struct desc_tab *
desc_get(relnum, flag)
int	relnum;
bool	flag;
{
	register struct desc_tab	*dt, *ret;
	struct desc_tab			*desc_lru();

	ret = NULL;

	/* search for one currently allocated */
	for (dt = &De.de_desc[0]; dt <= &De.de_desc[MAXRELN-1]; dt++)
	{
		if (dt->relnum == relnum)
		{
			ret = dt;
#			ifdef xDTR1
			if (tTf(62, 3))
				printf("found desc for %d\n", relnum);
#			endif
			break;
		}
	}

	if (ret == NULL && flag)
	{
		/* get a victim and deallocate desc */
		ret = desc_lru();

		/* deallocate */
#		ifdef xDTR1
		if (tTf(62, 5))
			printf("trading %d for %d\n", ret->relnum, relnum);
#		endif
		desc_close(ret);

		/* allocate */
		ret->relnum = relnum;
		ret->dtmode = DTALLOC;
	}

	if (ret != NULL)
		desc_top(ret);

	return (ret);
}
/*
**	For text space reasons only, the close relation routine varies
**	between decomp and decomp70. In decomp, the relation is opened
**	only for reading and never for writing thus inpcloser() can be
**	called. For decomp70 closer() must be called. If there were no
**	text space shortage, then closer() could always be called.
**	The routine init_decomp() assigned the value to Des_closefunc.
*/

extern int	(*Des_closefunc)();	/* either &inpcloser or &closer */

desc_close(dt1)
struct desc_tab	*dt1;
{
	register struct desc_tab	*dt;
	register int			i;

	dt = dt1;

	if (dt->dtmode == DTREAD || dt->dtmode == DTWRITE)
	{
		if (i = (*Des_closefunc)(&dt->desc))
			syserr("desc_close:closer %d,%.12s", i, dt->desc.reldum.relid);
		De.de_dopnfiles--;
		dt->dtmode = DTATTS;
	}
}
/*
** Desc_top -- make the desc_tab entry "dtx" the most recently used.
*/

desc_top(dt1)
struct desc_tab	*dt1;
{
	register struct desc_tab	*dt, *dx;
	register int			oldpos;

	dt = dt1;

	if ((oldpos = dt->dtpos) != 0)
	{
		/* descriptor isn't currently top */
		for (dx = De.de_desc; dx <= &De.de_desc[MAXRELN-1]; dx++)
			if (dx->dtpos < oldpos)
				dx->dtpos++;

		/* make descriptor first */
		dt->dtpos = 0;
	}
}
/*
** Desc_last -- make the desc_tab entry "dt" the least recently used.
*/

struct desc_tab *
desc_last(dt)
register struct desc_tab	*dt;
{
	register int			oldpos;
	register struct desc_tab	*dx;

	oldpos = dt->dtpos;
	for (dx = De.de_desc; dx <= &De.de_desc[MAXRELN-1]; dx++)
		if (dx->dtpos > oldpos)
			dx->dtpos--;

	/* make descriptor last */
	dt->dtpos = MAXRELN - 1;
}
/*
** Desc_lru -- return least recently used descriptor
*/

struct desc_tab *
desc_lru()
{
	register struct desc_tab	*dx;

	for (dx = De.de_desc; dx <= &De.de_desc[MAXRELN-1]; dx++)
	{
		if (dx->dtpos == MAXRELN - 1)
			return (dx);
	}
	syserr("desc_lru:no lru");
	/*NOTREACHED*/
}


desc_victum()
{
	register struct desc_tab	*dt, *old;

	old = NULL;
	for (dt = &De.de_desc[0]; dt <= &De.de_desc[MAXRELN-1]; dt++)
	{
		if (dt->dtmode == DTWRITE || dt->dtmode == DTREAD)
		{
			if (old == NULL || dt->dtpos > old->dtpos)
				old = dt;
		}
	}

	if (old == NULL)
		syserr("desc_victum:no victum %d,%d", De.de_dopnfiles, De.de_dfiles);
	desc_close(old);
}
