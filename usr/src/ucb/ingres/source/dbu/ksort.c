# include	<stdio.h>
# include	<ingres.h>
# include	<aux.h>
# include	<symbol.h>
# include	<access.h>
# include 	<func.h>
# include	<sccs.h>

SCCSID(@(#)ksort.c	7.1	2/5/81)

# define	N	7
# define	MEM	(32768 - 2)
# define	BUCKETSIZE	4
# define	ENDKEY	MAXDOM + 1



/*
**	Parameters:
**
**		argv[1]:	Fileset
**		argv[2]:	trace info (see below)
**		argv[3]:	file from which Desc is read
**		argv[4]:	Infile
**		argv[5]:	Outfile
**
**
**	Trace info comes from trace flag Z37 passed as the
**	second parameter. The bits used are:
**
**		0001	main trace info
**		0002	secondary trace info
**		0004	terciary trace info
**		0010	don't truncate temps
**		0020	don't unlink temps
**		0040	print am page refs
**		0100	print am tuple gets
**		0200	print tuples as output
*/

char		*Infile;
char		*Outfile;
DESC		Desc;
char		Descsort[MAXDOM+1];
FILE		*Oiop;
int		Trace;
int		Tupsize;
int		Bucket;
char		File[15];
char		*Fileset;
char		*Filep;
int		Nfiles		= 1;
int		Nlines;
long		Ccount;
char		**Lspace;
char		*Tspace;
extern int	cmpa();
long		Tupsout;

main(argc, argv)
int	argc;
char	**argv;
{
	extern char	*Proc_name;
	register int	i;
	register int	j;
	unsigned int	mem;
	char		*start;
	int		maxkey, rev;
	extern char	*malloc();


	Proc_name = "KSORT";
	if (argc != 6)
		syserr("argc");
	Fileset = argv[1];
	if (atoi(argv[2], &Trace) < 0)
		syserr("ksort: bad Trace param: %s", argv[2]);
	if ((i = open(argv[3], 0)) < 0)
		cant(argv[3]);
	if (read(i, &Desc, sizeof Desc) < sizeof Desc)
		syserr("read(Desc)");
	close(i);

	/* set up Descsort to indicate the sort order for tuple */
	/* if domain zero is given prepare to generate "hash bucket"
	** value for tuple */

	maxkey = 0;
	for (i = 0; i <= Desc.reldum.relatts; i++)
		if (j = Desc.relgiven[i])
		{
			if ((rev = j) < 0)
				j = -j;
			if (maxkey < j)
				maxkey = j;
			Descsort[--j] = rev < 0 ? -i : i;
		}

	Descsort[maxkey] = ENDKEY;	/* mark end of list */

	Tupsize = Desc.reldum.relwid;

	if (Bucket = (Descsort[0] == 0))
	{
		/* we will be generating hash bucket */
		Tupsize += BUCKETSIZE;
		Desc.relfrml[0] = BUCKETSIZE;
		Desc.relfrmt[0] = INT;
		Desc.reloff[0] = Desc.reldum.relwid;
	}

	if (Trace & 01)
	{
		printf("ksort: reldum.relatts is %d\n", Desc.reldum.relatts);
		printf("Bucket is %d,Sort is:\n", Bucket);
		for (i = 0; (j = Descsort[i]) != ENDKEY; i++)
			printf("Descsort[%d]=%d\n", i, j);
	}
	if (i = (maxkey - Bucket - Desc.reldum.relatts))
		syserr("%d domains missing\n", -i);
	Infile = argv[4];
	Outfile = argv[5];

	/* get up to 2**15 - 1 bytes of memory for buffers */
	/* note that mem must end up positive so that Nlines computation is right */
	mem = MEM;	/* take at most 2**15 - 1 bytes */
	while ((Lspace = (char **) malloc(mem)) == NULL)
		mem -= 1024;

	/* compute pointers and sizes into buffer memory */
	Nlines = mem / (Tupsize + sizeof(char *));
	Tspace = (char *) (Lspace + Nlines);
	if (Trace & 01)
		printf("Tspace=%x,Lspace=%x,Nlines=%x,mem=%d\n",
			Tspace, Lspace, Nlines, mem);

	/* set up temp files */
	concat(ztack("_SYSS", Fileset), "Xaa", File);
	Filep = File;
	while (*Filep != 'X')
		Filep++;
	Filep++;

	/* sort stage -- create a bunch of temporaries */
	Ccount = 0;
	if (Trace & 01)
		printf("sorting\n");
	sort();
	close(0);
	if (Trace & 01)
	{
		printf("done sorting\n%ld tuples written to %d files\n", Tupsout, Nfiles - 1);
		printf("sort required %ld compares\n", Ccount);
	}

	/* merge stage -- merge up to N temps into a new temp */
	Ccount = 0;
	for (i = 1; i + N < Nfiles; i += N) 
	{
		newfile();
		merge(i, i + N);
	}

	/* merge last set of temps into target file */
	if (i != Nfiles) 
	{
		oldfile();
		merge(i, Nfiles);
	}
	if (Trace & 01)
	{
		printf("%ld tuples in out file\n", Tupsout);
		printf("merge required %ld compares\n", Ccount);
	}
	term(0);
}
/*
**  SORT
*/

sort()
{
	register char	*cp;
	register char	**lp;
	register int	i;
	int		done;
	long		ntups;
	struct tup_id	tid, ltid;
	char		*xp;
	long		pageid;
	long		rhash();

	done = 0;
	ntups = 0;
	Tupsout = 0;
	if ((Desc.relfp = open(Infile, 0)) < 0)
		cant(Infile);
	Desc.relopn = (Desc.relfp + 1) * 5;

	/* initialize tids for full scan */
	pageid = 0;
	tid.line_id = -1;
	stuff_page(&tid, &pageid);
	pageid = -1;
	ltid.line_id = -1;
	stuff_page(&ltid, &pageid);

	do 
	{
		cp = Tspace;
		lp = Lspace;
		while (lp < Lspace + Nlines)
		{
			if ((i = get(&Desc, &tid, &ltid, cp, TRUE)) != 0)
			{
				if (i < 0)
					syserr("get %d", i);
				close(Desc.relfp);
				Desc.relopn = 0;
				done++;
				break;
			}
			if (Trace & 0100)
				printup(&Desc, cp);
			if (Bucket)
			{
				/* compute hash bucket and insert at end */
				pageid = rhash(&Desc, cp);
				bmove(&pageid, cp + Desc.reldum.relwid, BUCKETSIZE);
			}
			*lp++ = cp;
			cp += Tupsize;
			ntups++;
		}
		qsort(Lspace, lp - Lspace, sizeof(char *), cmpa);
		if (done == 0 || Nfiles != 1)
			newfile();
		else
			oldfile();
		while (lp > Lspace) 
		{
			cp = *--lp;
			xp = cp;
			if ((lp == Lspace) || (cmpa(&xp, &lp[-1]) != 0))
			{
				if (Trace & 0200)
				{
					printf("writing ");
					printup(&Desc, cp);
				}
				if ((i = fwrite(cp, 1, Tupsize, Oiop)) != Tupsize)
					syserr("cant write outfile %d (%d)", i, Nfiles);
				Tupsout++;
			}
		}
		fclose(Oiop);
	} while (done == 0);
	if (Trace & 01)
		printf("%ld tuples in\n", ntups);
}
/*
**  MERGE
*/

struct merg
{
	char		tup[MAXTUP+BUCKETSIZE];
	int		filedes;
	FILE		*fiop;
};

merge(a, b)
int	a;
int	b;
{
	register struct merg	*merg;
	register int		i, j;
	char			*f, *yesno;
	struct merg		*mbuf[N + 1];
	char			*setfil();

	if (Trace & 02)
		printf("merge %d to %d\n", a, b);
	merg = (struct merg *) Lspace;
	j = 0;
	for (i = a; i < b; i++) 
	{
		f = setfil(i);
		mbuf[j] = merg;
		merg->filedes = i;
		if ((merg->fiop = fopen(f, "r")) == NULL)
			cant(f);
		if (!rline(merg))
			j++;
		merg++;
	}

	i = j - 1;
	if (Trace & 04)
		printf("start merg with %d\n", i);
	while (i >= 0) 
	{
		if (Trace & 04)
			printf("mintup %d\n", i);
		if (mintup(mbuf, i, cmpa))
		{
			if (fwrite(mbuf[i]->tup, 1, Tupsize, Oiop) != Tupsize)
				syserr("cant write merge output");
			Tupsout++;
		}
		merg = mbuf[i];
		if (rline(merg))
		{
			yesno = "not ";
			if (!(Trace & 010))
			{
				/* truncate temporary files to zero length */
				yesno = "";
				close(creat(setfil(merg->filedes), 0600));
			}
			if (Trace & 02 || Trace & 010)
				printf("dropping and %struncating %s\n", yesno, setfil(merg->filedes));
			i--;
		}
	}

	fclose(Oiop);
}
/*
**	Mintup puts the smallest tuple in mbuf[cnt-1].
**	If the tuple is a duplicate of another then
**	mintup returns 0, else 1.
**
**	Cnt is the number of compares to make; i.e.
**	mbuf[cnt] is the last element.
*/

mintup(mbuf, cnt, cmpfunc)
struct merg	*mbuf[];
int		cnt;
int		(*cmpfunc)();
{
	register struct merg	**next, **last;
	struct merg		*temp;
	register int		nodup;
	int			j;

	nodup = TRUE;
	next = mbuf;
	last = &next[cnt];

	while (cnt--)
	{
		if (j = (*cmpfunc)(last, next))
		{
			/* tuples not equal. keep smallest */
			if (j < 0)
			{
				/* exchange */
				temp = *last;
				*last = *next;
				*next = temp;
				nodup = TRUE;
			}
		}
		else
			nodup = FALSE;

		next++;
	}
	return (nodup);
}


rline(mp)
struct merg	*mp;
{
	register struct merg	*merg;
	register int		i;

	merg = mp;
	if ((i = fread(merg->tup, 1, Tupsize, merg->fiop)) != Tupsize)
	{
		if (i == 0)
		{
			fclose(merg->fiop);
			return (1);
		}
		syserr("rd err %d on %s", i, setfil(merg->filedes));
	}
	return (0);
}

newfile()
{
	char	*setfil();

	makfile(setfil(Nfiles));
	Nfiles++;
}
/*
**	Convert the number i to a char
**	sequence aa, ab, ..., az, ba, etc.
*/

char *
setfil(i)
int	i;
{
	register int	j;

	j = i;
	j--;
	Filep[0] = j/26 + 'a';
	Filep[1] = j%26 + 'a';
	return (File);
}

oldfile()
{
	makfile(Outfile);
	Tupsout = 0;
}
/*
**	Create a file by the name "name"
**	and place its fio pointer in Oiop
*/

makfile(name)
char	*name;
{
	if ((Oiop = fopen(name, "w")) == NULL)
		cant(name);
}

cant(f)
char	*f;
{
	syserr("open %s", f);
}

term(error)
int	error;
{
	register int	i;

	if (Nfiles == 1)
		Nfiles++;
	if (Trace & 020)
		printf("temp files not removed\n");
	else
		for (i = 1; i < Nfiles; i++) 
		{
			unlink(setfil(i));
		}
	exit(error);
}
/*
**  CMPA -- compare tuples
*/

cmpa(a, b)
char	**a;
char	**b;
{
	int			af[4];
	int			bf[4];
	char			*pa, *pb;
	register union anytype	*tupa, *tupb;
	int			dom;
	register int		frml;
	int			frmt;
	int			off;
	int			temp;
	int			rt;
	char			*dp;

	pa = *a;
	pb = *b;
	Ccount++;
	dp = Descsort;
	while ((temp = *dp++) != ENDKEY)
	{
		if ((dom = temp) < 0)
			dom = -temp;
		frml = Desc.relfrml[dom];
		frmt = Desc.relfrmt[dom];
		off = Desc.reloff[dom];
		tupa = (union anytype *) &pa[off];
		tupb = (union anytype *) &pb[off];
		if (temp < 0)
		{
			tupb = tupa;
			tupa = (union anytype *) &pb[off];
		}
		if (frmt == CHAR)
		{
			frml &= 0377;
			if (rt = scompare(tupb, frml, tupa, frml))
				return (rt);
			continue;
		}

		/* domain is a numeric type */
		if (bequal(tupa, tupb, frml))
			continue;
		/* copy to even word boundary */
		bmove(tupa, af, frml);
		bmove(tupb, bf, frml);
		tupa = (union anytype *) af;
		tupb = (union anytype *) bf;

		switch (frmt)
		{

		  case INT:
			switch (frml)
			{

			  case 1:
				return (tupa->i1type > tupb->i1type ? -1 : 1);

			  case 2:
				return (tupa->i2type > tupb->i2type ? -1 : 1);

			  case 4:
				return (tupa->i4type > tupb->i4type ? -1 : 1);
			}

		  case FLOAT:
			switch (frml)
			{

			  case 4:
				return (tupa->f4type > tupb->f4type ? -1 : 1);

			  case 8:
				return (tupa->f8type > tupb->f8type ? -1 : 1);
			}
		}
	}
	return (0);
}
/*
**	Replacement for access method routine get_page();
**	and associated globals and routines.
*/

struct accbuf	*Acc_head, Accbuf;
long		Accuread, Accuwrite;

get_page(d, tid)
register DESC	*d;
struct tup_id	*tid;
{
	register int		i;
	long			pageid;
	register struct accbuf	*b;

	if (Trace & 0100)
	{
		printf("get_page: %.14s,", d->reldum.relid);
		dumptid(tid);
	}
	b = Acc_head;
	if (b == 0)
	{
		/* initialize buffer */
		Acc_head = &Accbuf;
		b = &Accbuf;
		b->thispage = -1;
	}
	pluck_page(tid, &pageid);
	i = 0;
	if (b->thispage != pageid)
	{
		if (Trace & 040)
			printf("get_page: rdg pg %ld\n", pageid);
		b->thispage = pageid;
		if ((lseek(d->relfp, pageid * PGSIZE, 0) < 0) ||
		    ((read(d->relfp, b, PGSIZE)) != PGSIZE))
		{
			i = AMREAD_ERR;
		}
		Accuread++;
	}
	return (i);
}

resetacc(buf)
struct accbuf	*buf;
{
	return (0);
}

acc_err(err)
int	err;
{
	return (err);

}
