# include	<stdio.h>
# include	<sccs.h>

SCCSID(@(#)copydb.q	7.2	3/11/82)


# define	MAXNAME	13	/* one more for the null */
# define	MAXKEYS	6	/* changing this is insuficient */
# define	MAXINDEX	10	/* change as needed */
# define	MAXREL	1000	/* can be increased as needed */
# define	MAXDOM 	50
# define	INT	30
# define	FLOAT	31
# define	CHAR	32
##char	Usercode[100];
char	**Wanted;
int	Status;

FILE	*Ifile, *Ofile;

struct relation
{
	char	name[MAXNAME];
	char	indx;
	int	spec;
	int	atts;
};

struct attribute
{
	char	aname[MAXNAME];
	char	format;
	int	length;
};

struct index
{
	char	iname[MAXNAME];
	char	ispec;
	char	idom[MAXKEYS];
};


main(argc,argv)
int	argc;
char	**argv;
{

##	char	*db;
	char	*path;
##	char	name[MAXNAME];
##	int	indx;
##	int	atts;
##	int	spec;
##	char	iname[MAXNAME];
##	int	idomv1, idomv2, idomv3, idomv4, idomv5, idomv6;

##	int	n;

	register struct relation	*relptr;
	register struct attribute	*attptr;
	register int			i;
	struct relation			relations[MAXREL];
	struct attribute		attributes[MAXDOM], indatts[MAXKEYS];
	struct index			indexes[MAXINDEX];
	struct index			*indxptr;
	int				attxtra[MAXDOM], indattx[MAXKEYS];
	int				xcount;
	int				j;
	char				line[100];
##	char				*uover;

	umask(022);
	if (argc < 3)
		syserr(0,"usage: copydb database targetdir [relation] ...");

	uover = 0;
	if (bequal(*++argv, "-u", 2))
		uover = *argv++;
	db = *argv++;
	path = *argv++;
	Wanted = argv++;

	if(path[0] != '/')
		printf("Warning %s is not a full path name\n",path);


	if ((Ifile = fopen(ztack(path,"/copy.in"), "w")) == NULL)
		syserr(0,"Cannot create %s!",ztack(path,"/copy.in"));
	if ((Ofile = fopen(ztack(path,"/copy.out"), "w")) == NULL)
		syserr(0,"Cannot create %s!",ztack(path,"/copy.out"));


##	ingres	db uover

##	retrieve (Usercode = usercode)

##	range of r is relation
	i = 0;

	/*
	**	Bring the relations into core
	*/
	relptr = relations;
##	retrieve (name = r.relid, spec = r.relspec, indx = r.relindxd,
##		atts = r.relatts)
##		where r.relowner = Usercode
##		and mod(r.relstat,2) != 1 /* sys catalog */
##		and r.relindxd >= 0
##		and r.relid != "_SYS*" /* system temporaries */
##	{
		if (notwanted(name))
			continue;
		if(i++>=MAXREL)
			syserr("Too many relations!!!");
		smove(name,(relptr->name));
		relptr->spec = spec;
		relptr->indx = indx;
		relptr++->atts = atts;
##	}


	/*
	**	For each relation bring its attributes into core
	**	in domain order
	*/
##	range of a is attribute
##	range of i is indexes
	n = i;
	printf("%d relations found\n",n);
	for (relptr = relations; relptr < &relations[n]; relptr++)
	{
		writein("create ");
		writeout("copy ");
		smove(relptr->name,name);
		writeboth(name);
		writeboth("(\n\t");

		xcount = retatts(name,attributes,attxtra);
		for(i = 0; i < relptr->atts; i++)
		{
			attptr = &attributes[i];
			if(i)
				writeboth(",\n\t");
			writeboth(attptr->aname);
			putboth('=');
			switch(attptr->format)
			{

			  case 'i':
				putboth('i');
				break;

			  case 'f':
				putboth('f');
				break;

			  case 'c':
				putboth('c');
				break;

			  default:
				syserr("Bad type: %d, in %c of %s!!",
					attptr->format,attptr->aname,
					name);

			}
				attptr->length &= 0377;
				writeboth(iocv(attptr->length));
		}

		writeboth("\n)");
		writeout(" into \"");
		writein("\n\\p\\g\ncopy ");
		writein(name);
		writein("() from \"");
		writeboth(path);
		putboth('/');
		writeboth(name);
		writeboth(Usercode);
		writeboth("\"\n\\p\\g\n");


		if (relptr->spec != 5 && relptr->spec != 0)
			modify(name,attributes,attxtra,relptr->spec,xcount);
		if (relptr->indx)
		{
			indxptr = indexes;
			i = 0;

##			retrieve (iname = i.irelidi, spec = i.irelspeci,
##				idomv1 = i.idom1, idomv2 = i.idom2, idomv3 = i.idom3,
##				idomv4 = i.idom4, idomv5 = i.idom5, idomv6 = i.idom6)
##			where i.irelidp = name and i.iownerp = Usercode
##			{

				if (i++ >= MAXINDEX)
					syserr("Too many indexes on %s!!",name);
				smove(iname,indxptr->iname);
				indxptr->ispec = spec;
				indxptr->idom[0] = idomv1;
				indxptr->idom[1] = idomv2;
				indxptr->idom[2] = idomv3;
				indxptr->idom[3] = idomv4;
				indxptr->idom[4] = idomv5;
				indxptr->idom[5] = idomv6;
				indxptr++;
##			}

			while (--indxptr >= indexes)
			{
	
				writein("index on ");
				writein(name);
				writein(" is ");
				writein(indxptr->iname);
				writein("(\n\t");
	
				for (i=0; indxptr->idom[i] && i < MAXKEYS; i++)
				{
					if(i)
						writein(",\n\t");
					writein(attributes[indxptr->idom[i]-1].aname);
				}
				writein(")\n\\p\\g\n");
				if(indxptr->ispec != 10 && indxptr->ispec != 11)
				{
					xcount = retatts(indxptr->iname,indatts,indattx);
					modify(indxptr->iname,indatts,indattx,indxptr->ispec,xcount);
				}
			}
		}
	}
	fflush(Ifile);
	fflush(Ofile);
	printf("All done!\n\n");
}
exitfn()
{
	fflush(Ifile);
	fflush(Ofile);
	exit(-1);
}


modify(name, attributes, attxtra, spec, xcount)
char	*name;
struct attribute	*attributes;
int	*attxtra;
int	spec;
int	xcount;
{
	register	i,j;
	writein("modify ");
	writein(name);
	writein(" to ");
	switch(spec)
	{

	  case -5:
		writein("cheap");
		goto nokeys;

	  case -10:
	  case -11:
		writein("c");
	  case 10:
	  case 11:
		writein("isam on ");
		break;

	  case -20:
	  case -21:
		writein("c");
	  case 21:
	  case 20:
		writein("hash on ");
		break;

	  default:
		syserr("Unimplimented spec: %d on %s",
			spec,name);

	}
	j = 0;
	for (i = 1; i <= xcount; i++)
	{
		if(j++)
			writein(",\n\t");
		writein(attributes[attxtra[i]].aname);
	}
	if (!j)
		syserr("No keys found for %s!!!",name);
nokeys:
	writein("\n\\p\\g\n");
}
retatts(name,attributes,attxtra)
char	*name;
struct attribute	*attributes;
int	attxtra[];
{
	register	xcount, i;
	register struct attribute	*attptr;

##	char	aname[MAXNAME];
##	int	domain, length, extra;
##	char	format[1];
	i = xcount = 0;
##	retrieve (aname = a.attname,domain = a.attid, format = a.attfrmt,
##		 length = a.attfrml, extra = a.attxtra)
##
##	where a.attrelid = name and a.attowner = Usercode
##	{
		if (i++ >= MAXDOM)
			syserr("Too many attributes!!!");
		attptr = &attributes[domain-1];
		smove(aname,attptr->aname);
		attptr->format = format[0];
		attptr->length = length;
		if(extra)
		{
			attxtra[extra] = domain - 1;
			xcount++;
		}
##	}
	return(xcount);
}


writeboth(string)
char	*string;
{
	register char	*sp;

	sp = string;

	while(*sp)
	{
		putc(*sp, Ofile);
		putc(*sp++, Ifile);
	}
}

putboth(ch)
char	ch;
{
	putc(ch, Ofile);
	putc(ch, Ifile);
}


writeout(string)
char	*string;
{
	register char 	*sp;

	sp = string;
	while (*sp)
		putc(*sp++,Ofile);
}


writein(string)
char	*string;
{
	register char	*sp;

	sp = string;
	while (*sp)
		putc(*sp++,Ifile);
}




/*
** Check to see if relation is wanted.
*/


notwanted(relname)
char	*relname;

{

	register char	**wantlist;
	register char	*nptr;

	if (*(wantlist = Wanted) == (char *) 0)
		return (0);

	nptr = relname;
	do
		if (!scompare(nptr, 0, *wantlist++, 0))
			return (0);
	while (*wantlist != (char *) 0);
	return (1);
}
