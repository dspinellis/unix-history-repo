# include	<sccs.h>

SCCSID(@(#)indconv.q	7.1	2/5/81)


# define	MAXNAME		12
# define	M_HEAP		5
# define	M_ISAM		11
# define	M_HASH		21

##	char		*Usercode;
	struct
	{
		char	relname[MAXNAME + 1], indname[MAXNAME + 1], relsp[10];
		char	domname[7][MAXNAME + 1], modseq[7];
	}	Indinfo[100];

struct relspec
{
	char	specnum, *specname;
};

struct relspec	relspecs[]
{
	M_ISAM,		"isam",
	M_HASH,		"hash",
	-M_ISAM,	"cisam",
	-M_HASH,	"chash",
	M_HEAP,		"heap",
	-M_HEAP,	"cheap",
	0
};

main(argc, argv)
int	argc;
char	*argv[];
{
	char	*kp;
##	char	aname[20];
##	char	keystring[100];
##	char	pname[20], iname[20];
##	char	*db;
##	int	cnt, i, j, rspec, aid, akey;

	if (argc != 2)
		syserr(0, "wrong number of parameters");
	db = argv[1];
	if (initucode(db, 0, 0, 0, 0))
		syserr(0, "cannot access %s data base", db);
##	ingres db "-nheap"
	cnt = 0;
##	range of r is relation
##	retrieve (iname = r.relid, rspec = r.relspec)
##		where r.relindxd < 0 and r.relindxd != -2 and r.relowner = Usercode
##	{
		smove(iname, Indinfo[cnt].indname);
		for (i = 0; relspecs[i].specnum != 0; i++)
		{
			if (rspec == relspecs[i].specnum)
			{
				smove(relspecs[i].specname, Indinfo[cnt].relsp);
				break;
			}
		}
		for (i = 0; i < 7; i++)
			Indinfo[cnt].modseq[i] = -1;
		if (++cnt >= 100)
		{
			printf("There are more indices than I can convert\n");
			printf("\tso run me again to finish the rest\n");
			break;
		}
##	}
##	range of x is indexes
##	retrieve (pname = x.irelidp, iname = x.irelidi)
##		where x.iownerp = Usercode
##	{
		for (i = 0; i < cnt; i++)
		{
			if (sequal(iname, Indinfo[i].indname))
			{
				smove(pname, Indinfo[i].relname);
				break;
			}
		}
##	}
##	range of a is attribute
##	retrieve (iname = a.attrelid, aname = a.attname, aid = a.attid,
##			akey = a.attxtra)
##		where a.attowner = Usercode
##	{
		for (i = 0; i < cnt; i++)
		{
			if (sequal(iname, Indinfo[i].indname))
			{
				if (akey > 0)
					Indinfo[i].modseq[akey - 1] = aid - 1;
				smove(aname, Indinfo[i].domname[aid - 1]);
				break;
			}
		}
##	}

	for (i = 0; i < cnt; i++)
	{
		smove(Indinfo[i].indname, iname);
##		destroy iname
	}
	for (i = 0; i < cnt; i++)
	{
		kp = &keystring[0];
		for (j = 0; j < 7; j++)
		{
			if (sequal("tidp", Indinfo[i].domname[j]))
				break;
			if (j > 0)
				xmove(", ", &kp);
			xmove(Indinfo[i].domname[j], &kp);
		}
		smove(Indinfo[i].relname, pname);
		smove(Indinfo[i].indname, iname);
##		index on pname is iname (keystring)
	}
	for (i = 0; i < cnt; i++)
	{
		kp = &keystring[0];
		for (j = 0; j < 7 && Indinfo[i].modseq[j] != -1; j++)
		{
			if (j > 0)
				xmove(", ", &kp);
			xmove(Indinfo[i].domname[Indinfo[i].modseq[j]], &kp);
		}
		smove(Indinfo[i].indname, iname);
		smove(Indinfo[i].relsp, pname);
		if (j == 0)
		{
##			modify iname to pname
			printf("write to Bob Epstein for a free INGRES T-shirt\n");
		}
		else
		{
##			modify iname to pname on keystring
		}
		printf("index %s converted.\n", iname);
	}
	printf("done\n");
}

sequal(ax, bx)
char	*ax, *bx;
{
	register char	*a, *b;

	a = --ax;
	b = --bx;
	while (*++a == *++b)
		if (*a == 0)
			return (1);
	if (*a == 0)
	{
		while (*b == ' ')
			b++;
		if (*b)
			return (0);
		return (1);
	}
	if (*b == 0)
	{
		while (*a == ' ')
			a++;
		if (*a)
			return (0);
		return (1);
	}
	return (0);
}

smove(ax, bx)
char	*ax, *bx;
{
	register char	*a, *b;

	a = ax;
	b = bx;
	while (*b++ = *a++);
	return (0);
}

xmove(ax, bx)
char	*ax, **bx;
{
	register char	*a, *b;

	a = ax;
	b = *bx;
	while (*b++ = *a++);
	*bx = --b;
	return (0);
}
