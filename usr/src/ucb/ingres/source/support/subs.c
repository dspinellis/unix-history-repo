# include	<stdio.h>
# include	<ingres.h>
# include	<aux.h>
# include	<access.h>
# include	<lock.h>
# include	<sccs.h>

SCCSID(@(#)subs.c	7.3	8/24/82)

/*
** These are subroutines common to RESTORE and PURGE.
*/

char		All;
char		Qrymod;
char		Superuser;
char		Ask;
char		Purge;
char		Clean;
char		Lastflag;
FILE		*Direc;
extern int	Status;
extern char	*Usercode;
char		**Dblist;




/*
**  INITIALIZE GLOBALS
**
**	Set up Usercode and Status
*/

initialize(argc, argv)
int	argc;
char	**argv;
{
	register int	i;
	long		l;
	extern char	*Flagvect[];
	extern char	*Parmvect[];
	register char	*p;
	register char	**av;
	char		datadir[MAXLINE];

#	ifdef	xTTR2
	tTfp(40, 0, "entered initialize\n");
#	endif
	i = initucode(argc, argv, FALSE, NULL, -1);
#	ifdef	xTTR2
	tTfp(40, 1, "initucode ret:%d\n", i);
#	endif
	switch (i)
	{
	  case 0:
		break;

	  case 3:
		printf("You are not a valid INGRES user\n");
		exit(-1);

	  default:
		syserr("initucode %d", i);
	}
	initdbpath(NULL, datadir, FALSE);

	/* scan flags */
#	ifdef	xTTR2
	tTfp(40, 2, "scanning flags\n");
#	endif
	for (av = Flagvect;  *av != NULL; av++)
	{
		p = *av;
		if (p[0] != '-')
		{
		badflag:
			printf("Bad flag: %s\n", p);
			return (-1);
		}
		switch (p[1])
		{
		  case 'a':
			Ask++;
			break;

		  case 'p':
			Purge++;
			break;

		  case 's':
			if (sucheck())
				Superuser++;
			else
			{
				printf("You may not use the -s flag\n");
				exit(-1);
			}
			break;

		  case 'f':
			Clean++;
			break;

		  case 'T':
			break;

		  default:
			goto badflag;
		}
	}
	Dblist = Parmvect;
	if (*Dblist == 0)
	{
#		ifdef	xTTR2
		tTfp(40, 3, "doing all\n");
#		endif
		All++;
		Direc = fopen(datadir, "r");
		if (Direc == NULL)
		{
			syserr("cannot read .../data/base");
		}
		l = 32;		/* fseek needs a long address */
		fseek(Direc, l, 0);
	}
#	ifdef	xTTR2
	tTfp(40, 0, "leaving initialize\n");
#	endif
}
/*
**  CHECK FOR SUPERUSER
**
**	The user has requested the -s flag.  Can he do it?  Will Martha
**	recover from cancer?  Will Dick get the girl?  Stay tuned for
**	"sucheck".
**
**	Permission is based on the U_SUPER bit in the status field
**	in the users file.
*/

sucheck()
{
	return (Status & U_SUPER);
}


struct directory
{
	short	inumber;
	char	fname[14];
	char	null;
};
/*
**  GET NEXT DATABASE
**
**	The next database to be purged is selected.  It comes from
**	either the directory or the database list.
**
**	Getnxtdb() leaves the user in the database directory.
*/

char *
getnxtdb()
{
	static struct directory	buf;
	register char		*db;
	register FILE		*fd;
	register int		i;
	extern struct admin	Admin;
	static char		dbpbuf[MAXLINE];

#	ifdef	xTTR2
	tTfp(41, 0, "entered getnxtdb\n");
#	endif
	for (;;)
	{
		if (All)
		{
			i = fread(&buf, 1, 16, Direc);
			if (i < 16)
				db = NULL;
			else
			{
				if (buf.inumber == 0)
				{
					continue;
				}
				db = buf.fname;
			}
			buf.null = 0;
		}
		else
		{
			db = *Dblist++;
		}
		if (db == NULL)
			return (NULL);
#		ifdef	xTTR2
		tTfp(41, 1, "using %s as Database\n", db);
#		endif
		i = initdbpath(db, dbpbuf, TRUE);
#		ifdef	xTTR2
		tTfp(41, 3, "initdbpath ret: %d, %s\n", i, dbpbuf);
#		endif
		switch (i)
		{
		  case 0:
		  case 1:
			break;

		  case 2:
		  case 3:
			printf("Database %s does not exist\n", db);
			continue;

		  default:
			syserr("initdbpath %d", i);
		}
		if (chdir(dbpbuf) < 0)
		{
			printf("Cannot enter %s", dbpbuf);
			continue;
		}
#		ifdef	xTTR2
		tTfp(41, 4, "chdir ok, Superuser: %d\n", Superuser);
#		endif
		fd = fopen("admin", "r");
		if (fd == NULL)
		{
			printf("Cannot open %s/admin\n", dbpbuf);
			continue;
		}
		fread(&Admin.adhdr, sizeof Admin.adhdr, 1, fd);
		fclose(fd);
#		ifdef	xTTR2
		tTfp(41, 5, "user: %.2s\n", Admin.adhdr.adowner);
#		endif

		/* set qrymod flag from database status */
		Qrymod = ((Admin.adhdr.adflags & A_QRYMOD) == A_QRYMOD);

		/* check for dba of database if not superuser */ 
		if (Superuser || bequal(Admin.adhdr.adowner, Usercode, 2))
			break;

		/*
		** not dba isn't an error if running in all mode since user
		** couln't have specified the database
		*/
		if (All)
			continue;
printf("You are not the dba for %s\n", db);
	}
#	ifdef	xTTR2
	tTfp(41, 6, "leaving getnxtdb, %s ok\n", db);
#	endif
	return (db);
}
/*
** ASK
**	If Ask is set desplay prompt and look for 'y' and return TRUE
**	If Ask is not set return TRUE
*/

ask(prompt)
char	*prompt;
{
	register char	*p;
	char		line[MAXLINE];
	extern char	Ask;

	if (!Ask)
		return (TRUE);
	p = prompt;

	while (*p)
	{
		putchar(*p);
		p++;
	}

	if (fgets(line, MAXLINE, stdin) == NULL)
		syserr("cannot getline in ask()");
	return (line[0] == 'y');
}
