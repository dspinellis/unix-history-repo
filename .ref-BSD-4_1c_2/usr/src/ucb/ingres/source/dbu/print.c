# include	<pv.h>
# include	<ingres.h>
# include	<access.h>
# include	<aux.h>
# include	<lock.h>
# include 	<func.h>
# include	<sccs.h>

SCCSID(@(#)print.c	7.1	2/5/81)

extern	short	tTdbu[];
extern	int	print();
extern	int	null_fn();

struct fn_def PrintFn =
{
	"PRINT",
	print,
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
**  PRINT -- print relation
**
**	Parameters:
**		parmv[0] through parmv[parmc -2] contain names
**		of relations to be printed on the standard output.
**		Note that this calls printatt, so all the output formatting 
**		features associated with printatt are available here.
**
**		parmv[parmc - 1].pv_type is PV_INT for other than a "normal"
**		print.  In this case parmv[parmc - 1] is 0 for headers on
**		every page, and 1 for all headers and footers suppressed.
**		err_array is set to 0 for a good relation, 1 for no
**		relation by that name, and -1 for a view.
**
**	Returns:
**		0 if no errors else the last error number
**
**	Trace Flags:
**		40
*/

print(parmc, parmv)
int	parmc;
PARM	parmv[];
{
	DESC			d;
	extern DESC		Attdes;
	struct attribute	att, katt;
	char			tuple[MAXTUP];
	TID			tid, limtid;
	TID			stid;
	register int		i;
	register int		ern;
	register char		*name;
	extern struct out_arg	Out_arg;
	int			mode;
	int			lineno;
	int			pc;
	int			err_array[PV_MAXPC];

#	ifdef xZTR1
	if (tTf(40, -1))
	{
		printf("entering print\n");
		prvect(parmc, parmv);
	}
#	endif

	if (parmv[parmc - 1].pv_type == PV_INT)
		mode = parmv[parmc - 1].pv_val.pv_int;
	else
		mode = -1;

	opencatalog("attribute", 0);

	for (pc = 0; pc <= parmc - 1; pc++)
	{
		name = parmv[pc].pv_val.pv_str;

		ern = openr(&d, 0, name);
		if (ern == AMOPNVIEW_ERR)
		{
			err_array[pc] = 5002;	/* can't print a view */
			continue;
		}
		if (ern > 0)
		{	
			err_array[pc] = 5001;	/* cannot open relation */
			continue;
		}
		if (ern < 0)
			syserr("printr:openr target %s, %d",
			name, ern);
		if ((d.reldum.relstat & S_PROTALL) && (d.reldum.relstat & S_PROTRET) &&
			!bequal(Usercode, d.reldum.relowner, 2))
		{
			err_array[pc] = 5003;	/* protection violation */
			closer(&d);
			continue;
		}


		/* a printable relation */
		err_array[pc] = 0;
#		ifdef xZTR2
		if (tTf(40, 1))
			printdesc(&d);
#		endif
		lineno = Out_arg.linesperpage - 6;
		if (mode <= 0)
		{
			if (mode == 0)
				putchar('\014');	/* form feed */
			printf("\n%s relation\n", name);
			lineno -= 2;
		}
	
		find(&d, NOKEY, &tid, &limtid);
	
		if (Lockrel)
			setrll(A_SLP, d.reltid.ltid, M_SHARE);	/* set shared lock on relation*/
		for (;;)
		{
			if (mode <= 0)
			{
				beginhdr();
				seq_init(&Attdes, &d);
				for (i = 1; seq_attributes(&Attdes, &d, &att); i++)
				{
					printhdr(d.relfrmt[i], d.relfrml[i], att.attname);
				}
				printeol();
				printeh();
				lineno -= 3;
			}
	
#			ifdef xZTM
			if(tTf(99, 1))
				timtrace(29, 0);
#			endif
			while ((ern = get(&d, &tid, &limtid, tuple, TRUE)) == 0)
			{
				printup(&d, tuple);

				if (mode == 0 && --lineno <= 0)
				{
					printf("\n\n\n\n\n\n");
					lineno = Out_arg.linesperpage - 6;
					break;
				}
			}
#			ifdef xZTM
			if(tTf(99, 1))
				timtrace(30, 0);
#			endif
			if (ern > 0)
				break;
	
			if (ern < 0)
				syserr("print: get %d", ern);
		}
	
		if (mode <= 0)
			printeh();
		if (Lockrel)
			unlrl(d.reltid.ltid);	/* release relation lock */
	
		closer(&d);
	}
	/* check for any error messages that should be printed */
	ern = 0;
	for (pc = 0; pc <= parmc - 1; pc++)
	{
		if (i = err_array[pc])
		{
			ern = nferror(i, parmv[pc].pv_val.pv_str, 0);
		}
	}
	return (ern);
}
