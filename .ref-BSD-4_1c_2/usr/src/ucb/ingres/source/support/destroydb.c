# include	<stdio.h>
# include	<sys/types.h>
# include	<sys/dir.h>
# include	<ingres.h>
# include	<aux.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)destroydb.c	7.3	11/2/82)

/*
**  DESTROY DATA BASE
**
**	This program destroys an existing database.  To be able
**	to wield this awesome power, you must be the dba for
**	the database.  Also, anyone has this power if the admin
**	the database, or the ingres superuser, and have the "-s"
**	flag requested.  If admin is trashed, the INGRES superuser
**	must either destroy the database or recover it.
**
**	If -m is specified, then the directory is not removed.
**	This is useful if the directory is a mounted file system.
*/

extern char	*Usercode;
extern int	Status;
extern char	*Pathname;
extern char	*Parmvect[];
extern char	*Flagvect[];
extern char	*Dbpath;
struct admin	Admin;
short		tTdbu[100];

main(argc, argv)
int	argc;
char	*argv[];
{
	register int	i;
	register char	*dbase;
	int		superuser, mounted;
	char		**av;
	register char	*p;
	char		*q;
# ifdef	DIRBLKSIZ
	DIR		*dirp;
# else	DIRBLKSIZ
	FILE		*iop;
# endif	DIRBLKSIZ

	argv[argc] = NULL;
#	ifdef xSTR1
	tTrace(argv, 'T', tTdbu, 100);
#	endif
	
	i = initucode(argc, argv, TRUE, NULL, -1);
	dbase = Parmvect[0];
#	ifdef xSTR1
	if (tTf(1, 0))
	{
		printf("after initcode %d: ", i);
		prargs(argc, argv);
	}
#	endif
	switch (i)
	{
	  case 0:
	  case 5:
		break;

	  case 1:
	  case 6:
		printf("Database %s does not exist\n", dbase);
		exit(-1);

	  case 2:
		printf("You are not authorized to access database %s\n", dbase);
		exit(-1);

	  case 3:
		printf("You are not an authorized INGRES user\n");
		exit(-1);

	  case 4:
		printf("No database name specified\n");
	usage:
		printf("Usage: destroydb [-s] [-m] dbname\n");
		exit(-1);

	  default:
		syserr("initucode %d", i);
	}

	mounted = superuser = 0;
	for (av = Flagvect; (p = *av) != NULL; av++)
	{
#		ifdef xSTR1
		if (tTf(1, 1))
			printf("p = *av (\"%s\")\n", p);
#		endif
		if (p[0] != '-')
		{
		badflag:
			printf("Bad flag %s\n", p);
			goto usage;
		}
		switch (p[1])
		{

		  case 's':
			superuser++;
			break;

		  case 'm':
			mounted++;
			break;

		  default:
			goto badflag;
		}
	}

	if (Parmvect[1] != NULL)
	{
		printf("Too many parameters to destroydb\n");
		goto usage;
	}
	if (length(dbase) > 14)
		syserr(0, "invalid dbname %s", dbase);
	if (superuser && (Status & U_SUPER) == 0)
		syserr(0, "you may not use the -s flag");

	if (!superuser)
	{
		if (!bequal(Admin.adhdr.adowner, Usercode, 2))
		{
			printf("You are not the DBA for %s\n", dbase);
			exit(-1);
		}
	}

	if (chdir(Dbpath) < 0)
		syserr("chdir %s", Dbpath);

# ifdef	DIRBLKSIZ
	if ( (dirp = opendir(".")) == NULL )
		syserr("Can't open . in %s",Dbpath);
	clean(dirp);
	closedir(dirp);
# else	DIRBLKSIZ
	iop = fopen(".", "r");
	if (iop == NULL)
		syserr("Cannot open dot in %s", Dbpath);
	clean(iop);
	fclose(iop);
# endif	DIRBLKSIZ

	if (!mounted)
	{
		/* find end of Dbpath and trim it off. */
		for (p = q = Dbpath; *p != '\0'; p++)
			if (*p == '/')
				q = p;
		*q++ = '\0';
		if (chdir(Dbpath) < 0)
			syserr("chdir(%s)", Dbpath);
		if ( i == 5 )
			if ( unlink(ztack(ztack(Pathname,"/data/base/"),dbase)) == -1 )
				syserr("Can't unlink the indirect file %s",dbase);
		execl("/bin/rmdir", "/bin/rmdir", q, 0);
		perror("/bin/rmdir");
	}
}



# ifdef	DIRBLKSIZ
clean(dirp)
register DIR	*dirp;
{
	struct	direct	*dp;

#	ifdef xSTR1
	if (tTf(2, 0))
		printf("clean: ");
#	endif

	for ( dp = readdir(dirp) ; dp != NULL ; dp = readdir(dirp) )
	{
		if ( !strcmp(".",dp->d_name) || !strcmp("..",dp->d_name) )
			continue;
#		ifdef xSTR1
		if (tTf(2, 1))
			printf("unlinking %s\n", dp->d_name);
#		endif
		unlink(dp->d_name);
	}
}/* clean */
		
# else	DIRBLKSIZ

clean(f)
register FILE	*f;
{
	struct direc
	{
		struct direct	d;
		char		null;
	};
	struct direc	cur;

#	ifdef xSTR1
	if (tTf(2, 0))
		printf("clean: ");
#	endif

	cur.null = 0;

	/* skip "." and ".." entries */
	fread(&cur, sizeof cur.d, 1, f);
	fread(&cur, sizeof cur.d, 1, f);

	/* scan directory */
	while (fread(&cur, sizeof cur.d, 1, f) > 0)
	{
		/* skip null entries */
		if (cur.d.d_ino == 0)
			continue;

#		ifdef xSTR1
		if (tTf(2, 1))
			printf("unlinking %s\n", cur.d.d_name);
#		endif
		unlink(cur.d.d_name);
	}
}
# endif	DIRBLKSIZ



/*
**  Rubout processing.
*/

rubproc()
{
	exit(-2);
}
