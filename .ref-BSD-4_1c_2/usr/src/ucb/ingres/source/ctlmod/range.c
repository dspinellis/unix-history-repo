# include	<ingres.h>
# include	<symbol.h>
# include	<range.h>
# include	<tree.h>
# include	<sccs.h>

SCCSID(@(#)range.c	7.1	2/5/81)



/*
**  CLRRANGE -- clear range table(s)
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		The range table (Qt.qt_rangev) is cleared.
*/

clrrange()
{
	register int	i;
	register DESC	*d;

	for (i = 0; i < MAXRANGE; i++)
	{
		Qt.qt_rangev[i].rngvmark = FALSE;
		Qt.qt_remap[i] = i;
		d = Qt.qt_rangev[i].rngvdesc;
		if (d != NULL)
		{
			xfree(d);
			Qt.qt_rangev[i].rngvdesc = NULL;
		}
	}
}
/*
**  DECLARE -- declare a range variable
**
**	A range variable is declared.  If possible, the preferred varno
**	stated is used (this is the one already in the tree).  This
**	should always be possible when reading the original tree (and
**	should probably stay this way to make debugging easier).  When
**	not possible, a new varno is chosen and the tree can later
**	be patched up by 'mapvars'.
**
**	Parameters:
**		varno -- the preferred varno.
**		desc -- the descriptor for this range variable.
**
**	Returns:
**		The actual varno assigned.
**
**	Side Effects:
**		Qt.qt_rangev is updated.
**
**	Trace Flags:
**		7.0-3
*/

declare(varno, desc)
int	varno;
DESC	*desc;
{
	register int	i;
	register int	vn;
	extern char	*trim_relname();

	vn = varno;

	/* check for preferred slot in range table available */
	if (desc != NULL && Qt.qt_rangev[vn].rngvdesc != NULL)
	{
		/* try to find another slot */
		for (i = 0; i < MAXRANGE; i++)
			if (Qt.qt_rangev[i].rngvdesc == NULL)
				break;

		if (i >= MAXRANGE)
		{
			/* too many variables */
			error(3100, trim_relname(desc->reldum.relid), 0);
		}

		vn = i;
	}

	/* if clearing, make sure something to clear */
	if (desc == NULL && Qt.qt_rangev[vn].rngvdesc == NULL)
		syserr("declare: null clr %d", vn);

	/* declare variable in the slot */
	Qt.qt_rangev[vn].rngvdesc = desc;
	Qt.qt_rangev[vn].rngvmark = (desc != NULL);

#	ifdef xQTR2
	if (tTf(7, 0))
		lprintf("declare(%d, %.14s) into slot %d\n", varno,
		    desc != NULL ? desc->reldum.relid : "(NULL)", vn);
#	endif

	return (vn);
}
