# include	<stdio.h>
# include	<ingres.h>
# include	<aux.h>
# include	<lock.h>
# include	<pv.h>
# include	<sccs.h>
# include	<opsys.h>
# include	<dir.h>

SCCSID(%W%	%G%)

/*
**  PURGE DATABASE
**
**	This stand-alone routine cleans up a database.  This includes:
**
**	- Destroy temporary relations, i.e., relations with names
**		beginning with "_SYS".
**	- Destroy expired relations
**	- Clean out junk files, such as core, etc.
**	- As a suggested future expansion, reformat relations which
**		have many overflow pages, or heaps with lots of old
**		deleted tuples, etc.
**
**	It may be called by the ingres superuser or by the dba of
**	a database.  There are two modes.  The first is where databases
**	to be purged are explicitly named.  If none are named, then
**	all databases owned by the particular user (or all databases
**	if the INGRES superuser) are purged.
**
**	Flags:
**	-p	enable the purge feature, i.e., clean out expired
**		relations as well as temporary relations.
**	-s	attempt to run in superuser mode.  The user must be
**		login "ingres" for this to succeed.
**	-a	ask the user before each database.
**	-f	clean out rather than report junk files.
*/


char		All;
char		Superuser;
char		Ask;
char		Purge;
char		Clean;
extern int	Wait_action;
extern int	Status;
extern char	*Usercode;
long		Today;
short		tTdbu[100];

struct directory
{
	short	inumber;
	char	fname[14];
	char	null;
};

main(argc, argv)
int	argc;
char	*argv[];
{
	register char	*db;
	register int	i;
	char		fake[256];
	extern char	*getnxtdb();

	argv[argc] = NULL;
#	ifdef xTTR1
	tTrace(argv, 'T', tTdbu, 100);
#	endif

	/* set up arguments and operating modes */
	initialize(argc, argv);
	time(&Today);
#	ifdef	xTTR2
	tTfp(10, 2, "Usercode: %.2s\n", Usercode);
#	endif

	while (db = getnxtdb())
	{
		purgedb(db);
	}
	printf("\npurge completed\n");
}



rubproc()
{
	unldb();
	exit(-1);
}



/*
**  PURGE DATABASE
**
**	The database is purged of temporaries, expired relations, and
**	junk.
*/

extern DESC	Reldes;


purgedb(db)
register char	*db;
{
	struct relation		rel, key;
	TID			rtid, rlimtid;
	register int		i;
	register char		c;
	long			l;
# ifdef	DIRBLKSIZ
	DIR			*dirp;
	struct	direct		*dp;
# else	DIRBLKSIZ
	struct directory	direc;
	FILE			*fd;
# endif	DIRBLKSIZ
	int			darg[3];
	PARM			pv[2];
	char			pbuff[MAXNAME + 1];

#	ifdef	xTTR2
	tTfp(11, 0, "entered purgedb(%s)\n", db);
#	endif
	printf("Database %s", db);
	if (!ask("? "))
		return;
	if (!Ask)
		printf(":\n");
	acc_init();

	/* set exclusive lock on data base */
#	ifdef	xTTR2
	tTfp(11, 1, "calling db_lock(%d)\n", M_EXCL);
#	endif
	db_lock(M_EXCL);

	/* open the relation relation for read-write */
	opencatalog("relation", 2);

	if (find(&Reldes, NOKEY, &rtid, &rlimtid))
	{
		printf("\tcannot find in %s\n", db);
		closecatalog(TRUE);	/* really close cache */
		unldb();		/* unlock the database */
		acc_close();
		return;
	}

	while (get(&Reldes, &rtid, &rlimtid, &rel, 1) == 0)
	{
		i = 0;

		/* check for temp rel */
		if (bequal(rel.relid, "_SYS", 4))
		{
			printf("\t%.14s: temporary", rel.relid);
			i++;
		}
		else if (rel.relsave < Today && rel.relsave != 0)
		{
			printf("\t%.14s: expired", rel.relid);
			if (Purge)
				if (ask("\n\t\tPURGE? "))
					i++;
		}
		else
			i = -1;

		/* if this relation should be purged -- call destroy */
		if (i > 0)
		{
			printf("\tpurging\n");

			/* set up parameter vector for destroy */
			bmove(rel.relid, pbuff, MAXNAME);
			pbuff[MAXNAME] = '\0';
			pv[0].pv_type = PV_STR;
			pv[0].pv_val.pv_str = pbuff;
			pv[1].pv_type = PV_EOF;
			pv[1].pv_val.pv_str = NULL;
			if (destroy(1, pv) != 0)
				syserr("cannot destroy %s\n", pbuff);
			closecatalog(FALSE);	/* to flush */
		}
		else if (i == 0)
			printf("\t\t(not purged)\n");
	}

# ifdef	DIRBLKSIZ
	if ( (dirp = opendir(".")) == NULL )
	{
		printf("\tcannot open .\n");
		closecatalog(TRUE);		/* really */
		unldb();		/* unlock the database */
		acc_close();
		return;
	}
	for ( dp = readdir(dirp) ; dp != NULL ; dp = readdir(dirp) )
	{
		if ( !strcmp(".",dp->d_name) || !strcmp("..",dp->d_name) )
			continue;

		/* throw out legitimate files */
		if (sequal(dp->d_name, "admin"))
			continue;

		/* always purge _SYS files */
		if (!bequal(dp->d_name, "_SYS", 4))
		{
			if (dp->d_name[13] != 0)
			{
				/* it might be a relation */
				clearkeys(&Reldes);
				setkey(&Reldes, &key, dp->d_name, RELID);
				setkey(&Reldes, &key, &dp->d_name[MAXNAME], RELOWNER);
				if (getequal(&Reldes, &key, &rel, &rtid) <= 0)
				{
					/* it is a relation (or should be saved) */
					continue;
				}
			}

			/* it is a funny file!!! */
			if (!Clean)
			{
				printf("\t%s: file (not unlinked)\n", dp->d_name);
				continue;
			}
		}

		/* purge the file */
		printf("\tunlinking %s\n", dp->d_name);
		if (unlink(dp->d_name))
			printf("\tcannot unlink\n");
	}
# else	DIRBLKSIZ
	/* open the directory to check for extra files */
	if ((fd = fopen(".", "r")) == NULL)
	{
		printf("\tcannot open .\n");
		closecatalog(TRUE);		/* really */
		unldb();		/* unlock the database */
		acc_close();
		return;
	}
	direc.null = 0;
	l = 32;		/* fseek needs a long address */
	fseek(fd, l, 0);

	/* scan the directory */
	while (fread(&direc, 16, 1, fd) > 0)
	{
		/* throw out null entries */
		if (direc.inumber == 0)
			continue;

		/* throw out legitimate files */
		if (sequal(direc.fname, "admin"))
			continue;

		/* always purge _SYS files */
		if (!bequal(direc.fname, "_SYS", 4))
		{
			if (direc.fname[13] != 0)
			{
				/* it might be a relation */
				clearkeys(&Reldes);
				setkey(&Reldes, &key, direc.fname, RELID);
				setkey(&Reldes, &key, &direc.fname[MAXNAME], RELOWNER);
				if (getequal(&Reldes, &key, &rel, &rtid) <= 0)
				{
					/* it is a relation (or should be saved) */
					continue;
				}
			}

			/* it is a funny file!!! */
			if (!Clean)
			{
				printf("\t%s: file (not unlinked)\n", direc.fname);
				continue;
			}
		}

		/* purge the file */
		printf("\tunlinking %s\n", direc.fname);
		if (unlink(direc.fname))
			printf("\tcannot unlink\n");
	}
	fclose(fd);
# endif	DIRBLKSIZ
	closecatalog(TRUE);	/* close catalogs */
	unldb();		/* unlock the database */
	acc_close();
}
