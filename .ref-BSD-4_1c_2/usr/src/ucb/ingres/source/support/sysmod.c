# include	<ingres.h>
# include	<aux.h>
# include	<lock.h>
# include	<access.h>
# include	<batch.h>
# include	<pv.h>
# include	<sccs.h>

SCCSID(@(#)sysmod.c	7.1	2/5/81)

extern int	Status;
short		tTdbu[100];


char	*Fileset;
char	Noupdt		= FALSE;
char	*Dummy;
char	**Xparams	= &Dummy;

struct modtabl
{
	char	*rname;
	char	**parvec;
	int	goahead;
	int	normgo;
	int	optn;
};

char	*Relpar[] =
{
	"relation",	"hash",		"name",
	"relid",	NULL
};

char	*Attpar[] =
{
	"attribute",	"hash",		"name",
	"attrelid",	"attowner",	"#attid",
	NULL
};

char	*Indpar[] =
{
	"indexes",	"hash",		"name",
	"irelidp",	"iownerp",	"",
	"minpages",	"5",		NULL
};

char	*Trepar[] =
{
	"tree",		"hash",		"name",
	"treerelid",	"treeowner",	"treetype",
	NULL
};

char	*Propar[] =
{
	"protect",	"hash",		"name",
	"prorelid",	"prorelown",	NULL
};

char	*Intpar[] =
{
	"integrities",	"hash",		"name",
	"intrelid",	"intrelowner",	NULL
};

struct modtabl  Modtabl[] =
{
	"relation",	&Relpar[0],	FALSE,	TRUE,	FALSE,
	"attribute",	&Attpar[0],	FALSE,	TRUE,	FALSE,
	"indexes",	&Indpar[0],	FALSE,	TRUE,	FALSE,
	"tree",		&Trepar[0],	FALSE,	TRUE,	TRUE,
	"protect",	&Propar[0],	FALSE,	TRUE,	TRUE,
	"integrities",	&Intpar[0],	FALSE,	TRUE,	TRUE,
	0
};


/*
**	SYSMOD -- Modify system catalogs to a predetermined
**		storage structure with predetermined keys.
*/

main(argc, argv)
int	argc;
char	*argv[];
{
	register int	i;
	register int	j;
	register char	**av;
	PARM		newpvec[40];
	char		*p;
	int		retval;
	char		fileset[10], proctab[100];
	extern char	*Parmvect[];
	extern char	*Flagvect[];
	extern char	*Dbpath;
	int		superuser;

	argv[argc] = NULL;

#	ifdef xSTR1
	tTrace(argv, 'T', tTdbu, 100);
#	endif

	itoa(getpid(), fileset);
	Fileset = fileset;
	i = initucode(argc, argv, TRUE, NULL, M_EXCL);
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
		printf("You are not authorized for database %s\n", Parmvect[0]);
		exit(-1);

	  case 3:
		printf("You are not a valid INGRES user\n");
		exit(-1);

	  case 4:
		printf("No database name specified\n");
	usage:
		printf("Usage: sysmod [-s] [+-w] dbname [relation ...]\n");
		exit(-1);

	  default:
		syserr("initucode %d", i);
	}

	concat(Pathname, "/bin/ksort", proctab);
	Dummy = proctab;

	superuser = FALSE;
	for (av = Flagvect; (p = *av) != NULL; av++)
	{
		if (p[0] != '-')
		{
		badflag:
			printf("Bad flag: %s\n", p);
			goto usage;
		}
		switch (p[1])
		{
		  case 's':
			if ((Status & U_SUPER) == 0)
			{
				printf("Only INGRES can use the -s flag\n");
				exit(-1);
			}
			superuser = TRUE;
			break;

		  case 'T':
			break;

		  default:
			goto badflag;
		}
	}
	if (chdir(Dbpath) < 0)
	{
		printf("data base %s does not exist\n", Parmvect[0]);
		exit(1);
	}
	if (superuser)
		bmove(Admin.adhdr.adowner, Usercode, 2);
	if (!bequal(Usercode, Admin.adhdr.adowner, 2))
	{
		printf("you are not the dba for %s\n", Parmvect[0]);
		exit(1);
	}
/*
**	if there are any arguments, verify that they are valid
**	names of relations which can be modified by this program.
**	if there are no arguments, assume all system relations are to be
**	modified.
*/
	if (Parmvect[1] != NULL)
		if ((Parmvect[2] == NULL) && sequal(Parmvect[1], "all"))
			for (i = 0; Modtabl[i].rname; i++)
				Modtabl[i].goahead = TRUE;
		else
			for (av = &Parmvect[1]; (p = *av) != NULL; av++)
			{
				for (j = 0; Modtabl[j].rname; j++)
				{
					if (sequal(p, Modtabl[j].rname))
					{
						if (Modtabl[j].goahead)
						{
							printf("%s duplicate relation name\n", p);
							exit(1);
						}
						Modtabl[j].goahead = TRUE;
						break;
					}
				}
				if (!Modtabl[j].rname)
				{
					printf("%s is not a system relation\n", p);
					exit(1);
				}
			}
	else
		for (i = 0; Modtabl[i].rname; i++)
			Modtabl[i].goahead = Modtabl[i].normgo;
	for (i = 0; Modtabl[i].rname; i++)
	{
		if (Modtabl[i].goahead == 0 || optn_rel(&Modtabl[i]))
			continue;
		printf("modifying %s\n", Modtabl[i].rname);
		av = Modtabl[i].parvec;
		j = 0;
		while (*av != NULL)
		{
			newpvec[j].pv_type = PV_STR;
			newpvec[j].pv_val.pv_str = *av;
			newpvec[j].pv_len = length(*av) + 1;
			j++;
			av++;
		}
		newpvec[j].pv_type = PV_EOF;
		smove(Fileset, Batchbuf.file_id);
#		ifdef xSTR2
		if (tTf(1, 0))
		{
			prvect(j, newpvec);
		}
#		endif
		if (retval = modify(j, newpvec))
		{
			printf("Error %d on %s\n", retval, Modtabl[i].rname);
			exit(1);
		}
	}
	printf("sysmod done\n");
	exit(0);
}


optn_rel(mx)
struct modtabl	*mx;
{
	register struct modtabl	*m;
	register int		ret;

	struct descriptor	des;

	m = mx;
	ret = FALSE;

	if (m->optn)
	{
		if (openr(&des, -1, m->rname))
		{
			ret = TRUE;
		}
	}
	return (ret);
}

rubproc()
{
	printf("sysmod interrupted\n");
	exit(1);
}
