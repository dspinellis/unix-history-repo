# include	"ctlmod.h"
# include	<tree.h>
# include	<sccs.h>

SCCSID(@(#)sysdump.c	7.1	2/5/81)

/*
**  SYSDUMP -- dump all parameters & state of system.
**
**	This is a process to put in for debugging.
*/

extern int	sysdump();
extern int	null_fn();
extern int	dump_cm();

short	tTsysdump[40];

struct fn_def	SysDmpFn =
{
	"SYSDUMP",
	sysdump,
	dump_cm,
	null_fn,
	NULL,
	0,
	tTsysdump,
	40,
	'Q',
	0
};

sysdump(pc, pv)
	int		pc;
	register PARM	pv[];
{
	register int	i;
	auto char	cx;

	tTfp(30, 0, "\n\nENTERED SYSDUMP\n\n");

	tTfp(30, 1, "Parameter vector:\n");

	if (tTf(30, 2))
		prvect(pc, pv);

	tTfp(30, 3, "\nMonitor input: \"");

	if (tTf(30, 4))
	{
		while (readmon(&cx, 1) > 0)
			xputchar(cx);

		printf("\"\n");
	}

	if (tTf(30, 5))
	{
		printf("\nQuery tree area:\n");
		printf("Qmode = %d\tResvar = %d\n", Qt.qt_qmode, Qt.qt_resvar);
	}

	if (tTf(30, 6))
	{
		printf("\nRange table:\n");
		for (i = 0; i < MAXRANGE; i++)
		{
			if (Qt.qt_rangev[i].rngvdesc != NULL)
			{
				printf("\nVAR %d: ", i);
				printdesc(Qt.qt_rangev[i].rngvdesc);
			}
		}
		printf("\n\n");
	}

	return (0);
}
/*
**  DUMP_CM -- dump control module configuration table.
*/

/*ARGSUSED*/
dump_cm(argc, argv)
int	argc;
char	**argv;
{
	register int		i;
	register state_t	*s;
	register proc_t		*pr;
	static int		reenter;

	if (!tTf(30, 0) || reenter++ > 0)
		return;

	printf("\n\n\nCONTROL MODULE CONFIGURATION TABLES:\n");

	printf("\nThe states:\n");
	for (i = 0, s = Cm.cm_state; i < CM_MAXST; i++, s++)
	{
		if (s->st_type == ST_UNDEF)
			continue;
		printf("%3d: stat %3o ", i, s->st_stat);
		switch (s->st_type)
		{
		  case ST_LOCAL:
			printf("(loc) func %d next %2d\n", s->st_v.st_loc.st_funcno,
			    s->st_v.st_loc.st_next);
			break;

		  case ST_REMOT:
			printf("(rem) proc %d\n", s->st_v.st_rem.st_proc);
			break;

		  default:
			printf("bad type %d\n", s->st_type);
			break;
		}
	}

	printf("\nThe procs:\n");
	for (i = 0, pr = Cm.cm_proc; i < CM_MAXPROC; i++, pr++)
	{
		printf("%3d: stat %4o file %2d ninput %2d\n", i, pr->pr_stat,
		    pr->pr_file, pr->pr_ninput);
	}

	printf("\nInitial input = %d\n", Cm.cm_input);

	printf("\n\n\n");
}
