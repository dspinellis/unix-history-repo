# include	<ingres.h>
# include	<symbol.h>
# include	"parser.h"
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)par_init.c	7.1	2/5/81)

/*
**  PAR_INIT -- initialization call for parser
**
**	Trace Flags:
**		par_init ~~ 60.0
*/

par_init(argc, argv1)
int	argc;
char	*argv1[];
{
	register int	rt;
	register char	**argv;

	extern int		Noupdt;
	extern int		Dcase;
	extern char		*Relspec;
	extern char		*Indexspec;
	extern DESC		Attdes;
	extern struct admin	Admin;
	extern int		Qrymod;
	extern int		yydebug;

	/* set up parser */
	argv = argv1;



#	ifdef	xPTR1
	if (tTf(60, 0))
		yydebug = 1;
#	endif

#	ifdef	xPTR2
	if (tTf(60, 1))
	{
		printf("Par_init:	");
		prargs(argc, argv);
	}
#	endif

	Noupdt = !setflag(argv, 'U', 0);
	Dcase = setflag(argv, 'L', 1);

	/* if param specified, set result reln storage structures */
	Relspec = "cheapsort";		/* default to cheapsort on ret into */
	Indexspec = "isam";		/* isam on index */

	for (rt = FREEFLAGS; rt < argc; rt++)
	{
		if (argv[rt][0] == '-')
		{
			if (argv[rt][1] == 'r')
			{
				Relspec = &argv[rt][2];
			}
			if (argv[rt][1] == 'n')
			{
				Indexspec = &argv[rt][2];
				continue;
			}
		}
	}
	if (sequal(Relspec, "heap"))
		Relspec = 0;
	if (sequal(Indexspec, "heap"))
		Indexspec = 0;

	rnginit();
	opencatalog("attribute", 0);

	Qrymod = ((Admin.adhdr.adflags & A_QRYMOD) == A_QRYMOD);

#	ifdef	xPTR2
	if (tTf(60, 2))
	{
		printf("Par_init: Results:\n");
		printf("\tQrymod: %d\n", Qrymod);
		printf("\tIndexspec: %s\n", Indexspec);
		printf("\tRelspec: %s\n", Relspec);
		printf("\tDcase: %d\n", Dcase);
		printf("\tNoupdt: %d\n", Noupdt); 
	}
#	endif
}
