# include	<stdio.h>
# include	<ingres.h>
# include	<aux.h>
# include	<access.h>
# include	<lock.h>
# include	<pv.h>
# include	<sccs.h>

SCCSID(@(#)printr.c	7.1	2/5/81)


extern int	Status;
short		tTdbu[100];

main(argc, argv)
int	argc;
char 	*argv[];
{
	extern struct out_arg	Out_arg;
	register char		**av;
	register char		*q;
	register char		*p;
	int			i;
	int			badf;
	char			style;
	char			*user_ovrd;
	int			mode;
	int			nc;
	PARM			pv[PV_MAXPC];
	PARM			*pp;
	extern char		*Parmvect[];
	extern char		*Flagvect[];
	extern char		*Dbpath;

	argv[argc] = NULL;

#	ifdef xSTR1
	tTrace(argv, 'T', tTdbu, 100);
#	endif

	mode = -1;
	badf = 0;

	/*
	**  Scan the argument vector and otherwise initialize.
	*/

	i = initucode(argc, argv, TRUE, NULL, M_SHARE);
	switch (i)
	{
	  case 0:
	  case 5:
		break;

	  case 1:
	  case 6:
		printf("Database %s does not exist\n", Parmvect[0]);
		exit(-1);

	  case 2:
		printf("You are not authorized to access this database\n");
		exit(-1);

	  case 3:
		printf("You are not a valid INGRES user\n");
		exit(-1);

	  case 4:
		printf("No database name specified\n");
		badf++;
		break;

	  default:
		syserr("main: initucode %d", i);
	}

	for (av = Flagvect; (p = *av) != NULL; av++)
	{
		if (p[0] != '-')
			goto badflag;
		switch (p[1])
		{
		  case 'h':		/* do headers on each page */
			mode = 0;
			if (p[2] == 0)
				break;
			if (atoi(&p[2], &Out_arg.linesperpage))
				goto badflag;
			break;

		  case 's':		/* supress headers and footers */
			mode = 1;
			if (p[2] != 0)
				goto badflag;
			break;

		  case 'c':		/* set cNwidth */
			if (atoi(&p[2], &Out_arg.c0width))
			{
			badflag:
				printf("bad flag %s\n", p);
				badf++;
				continue;
			}
			break;

		  case 'i':		/* set iNwidth */
			switch (p[2])
			{

			  case '1':
				if (atoi(&p[3], &Out_arg.i1width))
					goto badflag;
				break;

			  case '2':
				if (atoi(&p[3], &Out_arg.i2width))
					goto badflag;
				break;

			  case '4':
				if (atoi(&p[3], &Out_arg.i4width))
					goto badflag;
				break;

			  default:
				goto badflag;

			}
			break;

		  case 'f':		/* set fNwidth */
			style = p[3];
			switch (style)
			{

			  case 'e':
			  case 'E':
			  case 'f':
			  case 'F':
			  case 'g':
			  case 'G':
			  case 'n':
			  case 'N':
				break;

			  default:
				goto badflag;

			}
			for (q = &p[4]; *q != '.'; q++)
				if (*q == 0)
					goto badflag;
			*q++ = 0;
			switch (p[2])
			{

			  case '4':
				if (atoi(&p[4], &Out_arg.f4width))
					goto badflag;
				if (atoi(q, &Out_arg.f4prec))
					goto badflag;
				Out_arg.f4style = style;
				break;

			  case '8':
				if (atoi(&p[4], &Out_arg.f8width))
					goto badflag;
				if (atoi(q, &Out_arg.f8prec))
					goto badflag;
				Out_arg.f8style = style;
				break;

			  default:
				goto badflag;

			}
			break;

		  case 'v':
			if (p[2] == 0 || p[3] != 0)
				goto badflag;
			Out_arg.coldelim = p[2];
			break;

		  default:
			goto badflag;
		}
	}

	/*
	**  Build parameter vector for print call
	*/

	for (nc = 1, pp = pv; Parmvect[nc] != NULL; nc++)
		((pp++)->pv_val).pv_str = Parmvect[nc];
	if (mode != -1)
		((pp++)->pv_val).pv_int = mode;
	pp->pv_type = PV_EOF;

	/*
	**  Check for usage errors.
	*/

	if (nc < 2)
	{
		badf++;
		printf("usage:  printr [flags] database relation ...\n");
	}
	if (badf)
	{
		fflush(stdout);
		exit(-1);
	}

	p = Parmvect[0];	/* data base is first parameter */
	if (chdir(Dbpath) < 0)
		syserr("cannot access data base %s", p);
#	ifdef xTTR2
	if (tTf(1, 0))
		printf("entered database %s\n", Dbpath);
#	endif

	/* initialize access methods (and Admin struct) for user_ovrd test */
	acc_init();

	set_so_buf();
#	ifdef xTTR1
	if (tTf(1, 1))
		printf("printing %s\n", p);
#	endif

	print(nc - 1, pv);
	fflush(stdout);
	exit(0);
}



rubproc()
{
	fflush(stdout);
	exit(0);
}
