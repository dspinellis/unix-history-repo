





























typedef char	bool;		


















 












































































































extern short	*tT;













































struct relation
{
		char	relid[	12		];	
		char	relowner[2];	
		char	relspec;	
				
				
				
				
				
				
		char	relindxd;	
		short	relstat2;	
		short	relstat;	
		long	relsave;	
		long	reltups;	
		short	relatts;	
		short	relwid;		
		long	relprim;	
		long	relfree;	
		long	relstamp;	
};

















struct attribute
{
		char 	attrelid[	12		];	
		char	attowner[2];	
		short	attid;		
		char	attname[	12		];	
		short	attoff;		
		char	attfrmt;	
		char	attfrml;	
		char	attxtra;	
};









struct tup_id
{

		char	line_id, pg2, pg1, pg0;

};

typedef struct tup_id	TID;

typedef union
{
	long	ltid;
	TID	s_tupid;
} tid_type;


























struct descriptor
{
	struct relation	reldum;
		




	char	relvname[	12		];	
		short	relfp;		
		short	relopn;		
	tid_type reltid;	


		long	reladds;	
		short	reloff[	50		];	
		char	relfrmt[	50		]; 

		char	relfrml[	50		]; 


		char	relxtra[	50		]; 

		char	relgiven[	50		]; 


};

typedef struct descriptor	DESC;


typedef struct
{
	DESC	*rngvdesc;	
	bool	rngvmark;	
}  RANGEV;
















union anytype
{
	char		i1type;
	short		i2type;
	long		i4type;
	float		f4type;
	double		f8type;
	char		c0type[1];	


	char		*cptype;
	char		**cpptype;
};

typedef union anytype	ANYTYPE;



























struct accessparam
{
	short	mode;		
	short	sec_index;	
	char	keydno[	50		 + 1];
};










struct desxx
{
	char	*cach_relname;	
	DESC	*cach_desc;	
	DESC	*cach_alias;	
};











































































extern char	*Usercode;
extern char	*Pathname;

















struct out_arg
{
	int	c0width;	
	int	i1width;	
	int	i2width;	
	int	i4width;	
	int	f4width;	
	int	f8width;	
	int	f4prec;		
	int	f8prec;		
	char	f4style;	
	char	f8style;	
	int	linesperpage;	
	char	coldelim;	
};













































struct lockreq
{
	char	lract;			





	char	lrtype;			





	char	lrmod;			



					
	char	dbnode[4];		
	char	lrel[4];		
	char	lpage[4];		
};

extern char	Acclock;		
extern int	Alockdes;		
extern int	Lockrel;		






















































struct accbuf
{
	
	long		mainpg;		
	long		ovflopg;	
	short		nxtlino;	
	char		firstup[	1024		 - 12];	
	short		linetab[1];	
					



	
	long		rel_tupid;	
	long		thispage;	
	int		filedesc;	
	struct accbuf	*modf;		
	struct accbuf	*modb;		
	int		bufstatus;	
};







struct
{
	char	acc_buf[3		];
};


extern struct accbuf	*Acc_head;	
extern struct accbuf	*Acc_tail;	
extern struct accbuf	Acc_buf[3		];	












struct adminhdr
{
	char	adowner[2];	
	short	adflags;	
	short	adlength;	
	short	adversion;	
	short	adreldsz;	
	short	adattdsz;	
};

struct admin
{
	struct adminhdr		adhdr;
	struct descriptor	adreld;
	struct descriptor	adattd;
};














extern struct admin		Admin;





struct pgtuple
{
	struct tup_id	childtid;		
	char		childtup[	1010		];
};






extern long	Accuread, Accuwrite;





extern char	*Acctuple;		
extern		Accerror;		
extern char	Accanon[	1010		];	
















struct batchbuf
{
	char	file_id[	6	];	
	char	bbuf[506	];	
};


struct si_doms
{
	short	rel_off;	
	short	tupo_off;	
	short	dom_size;	
				
};
struct batchhd
{
	char	db_name[15];	
	char	rel_name[13];	
	char	userid[2];	
	long	num_updts;	
	short	mode_up;	
	short	tido_size;	
	short	tupo_size;	
	short	tupn_size;	
	short	tidn_size;	
	short	si_dcount;	
	struct si_doms	si[	50		+1];	
};



short	Batch_fp;	
short	Batch_cnt;	
short	Batch_dirty;	
short	Batch_lread;	
short	Batch_recovery;	

extern char	*Fileset;	
struct batchbuf	Batchbuf;
struct batchhd	Batchhd;





















typedef struct
{
	short	pv_type;	
	short	pv_len;		
	union
	{
		short			pv_int;		
		struct querytree	*pv_qtree;	
		char			*pv_str;	
		char			*pv_tuple;	
	} pv_val;
}  PARM;













 static char Sccsid[] = "@(#)sysmod.c	7.1	2/5/81";

extern int	Status;
short		tTdbu[100];


char	*Fileset;
char	Noupdt		= 	0	;
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
	"relid",		0	
};

char	*Attpar[] =
{
	"attribute",	"hash",		"name",
	"attrelid",	"attowner",	"#attid",
		0	
};

char	*Indpar[] =
{
	"indexes",	"hash",		"name",
	"irelidp",	"iownerp",	"",
	"minpages",	"5",			0	
};

char	*Trepar[] =
{
	"tree",		"hash",		"name",
	"treerelid",	"treeowner",	"treetype",
		0	
};

char	*Propar[] =
{
	"protect",	"hash",		"name",
	"prorelid",	"prorelown",		0	
};

char	*Intpar[] =
{
	"integrities",	"hash",		"name",
	"intrelid",	"intrelowner",		0	
};

struct modtabl  Modtabl[] =
{
	"relation",	&Relpar[0],		0	,		1	,		0	,
	"attribute",	&Attpar[0],		0	,		1	,		0	,
	"indexes",	&Indpar[0],		0	,		1	,		0	,
	"tree",		&Trepar[0],		0	,		1	,		1	,
	"protect",	&Propar[0],		0	,		1	,		1	,
	"integrities",	&Intpar[0],		0	,		1	,		1	,
	0
};







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

	argv[argc] = 	0	;


	tTrace(argv, 'T', tTdbu, 100);


	itoa(getpid(), fileset);
	Fileset = fileset;
	i = initucode(argc, argv, 	1	, 	0	, 	1);
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

	superuser = 	0	;
	for (av = Flagvect; (p = *av) != 	0	; av++)
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
			if ((Status & 	0100000		) == 0)
			{
				printf("Only INGRES can use the -s flag\n");
				exit(-1);
			}
			superuser = 	1	;
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






	if (Parmvect[1] != 	0	)
		if ((Parmvect[2] == 	0	) && sequal(Parmvect[1], "all"))
			for (i = 0; Modtabl[i].rname; i++)
				Modtabl[i].goahead = 	1	;
		else
			for (av = &Parmvect[1]; (p = *av) != 	0	; av++)
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
						Modtabl[j].goahead = 	1	;
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
		while (*av != 	0	)
		{
			newpvec[j].pv_type = 	2	;
			newpvec[j].pv_val.pv_str = *av;
			newpvec[j].pv_len = length(*av) + 1;
			j++;
			av++;
		}
		newpvec[j].pv_type = 	0	;
		smove(Fileset, Batchbuf.file_id);

		if (	(( 0 < 0) ? tT[1] : (tT[1] & (1 <<  0))))
		{
			prvect(j, newpvec);
		}

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
	ret = 	0	;

	if (m->optn)
	{
		if (openr(&des, -1, m->rname))
		{
			ret = 	1	;
		}
	}
	return (ret);
}

rubproc()
{
	printf("sysmod interrupted\n");
	exit(1);
}
