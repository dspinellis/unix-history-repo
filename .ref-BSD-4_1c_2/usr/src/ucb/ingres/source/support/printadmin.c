# include	<ingres.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)printadmin.c	7.1	2/5/81)

short		tTdbu[100];
struct admin	Admin;

main(argc, argv)
int	argc;
char	*argv[];
{
	register int	i, fp;
	register char	*db;
	extern char	*Dbpath;
	extern char	*Parmvect[], *Flagvect[];

	argv[argc] = NULL;
#	ifdef xSTR1
	tTrace(argv, 'T', tTdbu, 100);
#	endif

	i = initucode(argc, argv, TRUE, NULL, -1);
	db = Parmvect[0];
	switch (i)
	{
	  case 0:
	  case 5:
		break;

	  case 1:
	  case 6:
		printf("Database %s does not exist\n", db);
		exit(-1);

	  case 2:
		printf("You are not authorized to access this database\n");
		exit(-1);

	  case 3:
		printf("You are not a valid INGRES user\n");
		exit(-1);

	  case 4:
		printf("No database name specified\n");
	usage:
		printf("usage: printadmin database\n");
		exit(-1);
	  default:
		syserr("initucode %d", i);
	}

	if (Flagvect[0] != NULL)
	{
		printf("No flags are allowed for this command\n");
		goto usage;
	}

	if (Parmvect[1] != NULL)
		goto usage;

	if (chdir(Dbpath) < 0)
		syserr("cannot access database %s", db);
#	ifdef xTTR2
	if (tTf(1, 0))
		printf("entered database %s\n", Dbpath);
#	endif

	/* Admin struct has been filled in by initucode */
	printf("Database %s, Dba %.2s, Adflags %o\n",
		db, Admin.adhdr.adowner, Admin.adhdr.adflags);
	printf("Code %d, adlen %d, adreldsz %d, adattdsz %d\n",
	       Admin.adhdr.adversion, Admin.adhdr.adlength,
	       Admin.adhdr.adreldsz, Admin.adhdr.adattdsz);

	printf("\n\n");
	printdesc(&Admin.adreld);

	printf("\n\n");
	printdesc(&Admin.adattd);
}


rubproc()
{
	exit(1);
}
