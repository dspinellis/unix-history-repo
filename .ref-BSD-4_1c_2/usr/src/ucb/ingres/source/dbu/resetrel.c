# include	<pv.h>
# include	<ingres.h>
# include	<aux.h>
# include	<batch.h>
# include	<access.h>
# include 	<func.h>
# include	<sccs.h>

SCCSID(@(#)resetrel.c	7.1	2/5/81)

extern	short	tTdbu[];
extern	int	resetrel();
extern	int	null_fn();

struct fn_def ResetrFn =
{
	"RESETREL",
	resetrel,
	null_fn,		/* initialization function */
	null_fn,
	NULL,
	0,
	tTdbu,
	100,
	'Z',
	0
};
/*
**	RESETREL -- will change a relation to an empty heap.  This is only
**		to be used on temporary relations and should only be called
**		by the DECOMP process.
*/

resetrel(pc, pv)
int	pc;
PARM	pv[];

{
	extern DESC	Reldes;
	DESC		desc;
	char		relname[MAXNAME + 4];
	long		lnum;

	opencatalog("relation", 2);
	while (pc-- > 0)
	{
		if (openr(&desc, -1, pv->pv_val.pv_str))
			syserr("RESETREL: openr %s", pv->pv_val.pv_str);
		if (!bequal(Usercode, desc.reldum.relowner, sizeof desc.reldum.relowner))
			syserr("RESETREL: not owner of %s", pv->pv_val.pv_str);
		ingresname(desc.reldum.relid, desc.reldum.relowner, relname);
		if ((desc.relfp = creat(relname, FILEMODE)) < 0)
			syserr("RESETREL: create %s", relname);
		lnum = 1;
		if (formatpg(&desc, lnum))
			syserr("RESETREL: formatpg %s", relname);
		desc.reldum.reltups = 0;
		desc.reldum.relspec = M_HEAP;
		desc.reldum.relprim = 1;
		close(desc.relfp);
		if (replace(&Reldes, &desc.reltid, &desc, FALSE) < 0)
			syserr("RESETREL: replace rel %s", relname);
		pv++;
	}
	return (0);

}
