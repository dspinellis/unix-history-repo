# include	<stdio.h>
# include	<func.h>
# include	<pv.h>
# include	<ingres.h>
# include	<aux.h>
# include	<access.h>
# include	<symbol.h>
# include	<lock.h>
# include	<sccs.h>

SCCSID(@(#)copy.c	7.3	4/7/82)

/*
**  COPY -- Performs an ingres COPY.
**
**	Trace Flags:
**		30
*/


# define	MAXMAP		3 * MAXDOM
# define	DUMMY		'd'
# define	ESCAPE		'\\'

extern short	tTdbu[100];
extern int	copy();
extern int	null_fn();

struct fn_def CopyFn =
{
	"COPY",
	copy,
	null_fn,
	null_fn,
	NULL,
	0,
	tTdbu,
	100,
	'Z',
	0
};




struct map
{
	char	name[MAXNAME+1];	/* attribute name */
	char	ftype;		/* attfrmt of file domain */
	char	rtype;		/* attfrmt of relation domain */
	int	flen;		/* attfrml of file domain */
	int	rlen;		/* attfrml of relation domain */
	int	roffset;	/* attoff of relation domain */
	int	used;		/* tag for duplicate checking */
	char	*fdelim;	/* pointer to list of file param delims */
	char	*paramname;	/* pointer to original parameter name */
				/* used for supplying domain name in case of error */
};
struct map	Map[MAXMAP];		/* one entry for each user
				   specified domain in copy statement. */

int	Mapcount;		/* number of Map entries   */


DESC	Des;		/* descriptor for copied relation     */

extern struct out_arg	Out_arg;	/* user defined formats for numeric output */

FILE	*File_iop;		/* i/o file pointer */
char	*Filename;		/* pointer to file name */

int	Into;			/* into is one if this is a copy into file */

char	Inbuf[BUFSIZ];		/* input holder */
char	Outbuf[BUFSIZ];		/* output holder */

long	Tupcount;		/* number of tuples processed */
char	*Relname;		/* name of relation */
long	Duptuple;		/* number of duplicate tuples */
long	Baddoms;		/* number of domains with control chars */
long	Truncount;		/* number of truncations on a c0 field */
int	Piped[2];		/* pipe descriptor for copy communication */
char	*Cpdomains[] =		/* dummy domain names for copy "into" */
{
	"nl",		"\n",
	"tab",		"\t",
	"sp",		" ",
	"nul",		"\0",
	"null",		"\0",
	"comma",	",",
	"colon",	":",
	"dash",		"-",
	"lparen",	"(",
	"rparen",	")",
	0
};

char	Delimitor[] =	",\n\t";	/* default delims for c0 & d0 */



copy(pc,pv)
int	pc;
PARM	pv[];
{
	extern char	*Usercode;
	extern int	Noupdt;
	register int	i, pid;
	register char	*cp;
	int		stat;
	int		copydone();
	int		op;

#	ifdef xZTR1
	if (tTf(30,1))
	{
		printf("entered copy\n");
		prvect(pc, pv);
	}
#	endif
	Duptuple = 0;
	Truncount = 0;
	Tupcount = 0;
	Baddoms = 0;
	Relname = pv[0].pv_val.pv_str;
	Into = (pv[pc-2].pv_val.pv_str[0] == 'i');
	Filename = pv[pc-1].pv_val.pv_str;

	/* relation must exist and not be a system relation */
	/* in addition a copy "from" can't be done if the user */
	/* doesn't own the relation */
	/* and furthermore it can't have an index */
	i = 0;	/* assume all is well */
	if (op = openr(&Des, 2, Relname))
	{
		if (op == AMOPNVIEW_ERR)
			i = 5818;
		else
		{
			if (op < 0)
				syserr("COPY: openr 1 (%.14s) %d", Relname, op);
			else
				/* non-existant relation */
				i = 5800;
		}
	}
	else
	{
		if (Into)
		{
			if ((Des.reldum.relstat & S_PROTALL)
				&& (Des.reldum.relstat & S_PROTRET)
				&& !bequal(Usercode, Des.reldum.relowner, 2))
				i = 5822;
		}
		else
		{
			/* extra checking if this is a copy "from" */

			/* must be owned by the user */
			if (!bequal(Usercode, Des.reldum.relowner, 2))
				i = 5814;
			else
				/* must be updateable */
				if ((Des.reldum.relstat & S_NOUPDT) && Noupdt)
					i = 5813;
				else
					/* must not be indexed */
					if (Des.reldum.relindxd > 0)
						i = 5812;
		}
	}
	if (i)
	{
		closer(&Des);
		return (error(i, Relname, 0));	/* relation doesn't exist for this user */
	}

	/* check that file name begins with a "/" */
	cp = Filename;
	while (*cp == ' ')
		cp++;
	if (*cp != '/')
	{
		closer(&Des);
		return (error(5816, Filename, 0));
	}

	/* fill map structures with transfer information */
	if (i = mapfill(&pv[1]))
	{
		closer(&Des);
		return (i);	/* error in user semantics */
	}

	/* fork a child process which will run as the real user */
	/* that child will complete the copy and exit */
	if (pipe(Piped))
		syserr("copy:can't make pipe");
	if ((pid = fork()) < 0)
		syserr("copy:can't fork");
	if (pid)
	{
		/* the ingres parent */
		close(Piped[1]);
		ruboff(0);	/* interrupts off */
		stat = fullwait(pid, "copy");
		if (read(Piped[0], &Des.reladds, 4) != 4)
			syserr("copy:can't read pipe");
		close(Piped[0]);
		closer(&Des);	/* close the rel */
		rubon();
		/* if stat is != 0 then add on 5800 for error */
		if (stat)
			stat += 5800;
		return (stat);	/* done */
	}

	/* the child. change to run as the real user */
	if (signal(2, 1) != 1)
		signal(2, copydone);	/* clean up on rubout */
	setuid(getuid());
#	ifndef xB_UNIX
	setgid(getgid());
#	endif
	if (Into)	/* from relation into file */
	{
		if ((File_iop = fopen(Filename, "w")) == NULL) /* create file for user */
			i = nferror(5806, Filename, 0);	/* cant create file */
		else
		{
			if (Lockrel)	/* set a shared lock on relation*/
				setrll(A_SLP, Des.reltid.ltid, M_SHARE);
			i = rel_file();
		}
	}
	else		/* from UNIX file into relation */
	{
		if ((File_iop = fopen(Filename, "r")) == NULL)
			i = nferror(5805, Filename, 0);	/* cant open user file */
		else
		{
			if (Lockrel)	/* set an exclusive lock on relat*/
				setrll(A_SLP, Des.reltid.ltid, M_EXCL);
			i = file_rel();
			if (Duptuple)
				nferror(5819, locv(Duptuple), 0);	/* warning only */
			if (Baddoms)
				nferror(5820, locv(Baddoms), 0);	/* warning only */
		}
	}
	copydone(i);
}
/*
**	Finish up and exit after a copy or interrupt
**
**	I is the return code. Since only a byte can be
**	returned, only the least significant 2 decimal
**	digits are returned. i is either 0 or a number like 58??
*/

copydone(i)
int	i;
{
	if (Lockrel)	/* unlock relation */
		unlrl(Des.reltid.ltid);
	if (Truncount)
		nferror(5821, locv(Truncount), 0);	/* warning only */
	/*  force the updates to be flushed */
	cleanrel(&Des);
	if (File_iop)
		fclose(File_iop);
	if (write(Piped[1], &Des.reladds, 4) != 4)
		syserr("copyc:can't writepipe");
	exit (i % 100);
}
/*
**  REL_FILE -- copy from relation to file
*/

rel_file()
{
	int			j;
	struct tup_id		tid, limtid;
	char			*cp, save;
	register int		offset;
	register int		i;
	register struct map	*mp;

	/* set scan limits to scan the entire relation */
	if (find(&Des, NOKEY, &tid, &limtid))
		syserr("find error");

	while ((i = get(&Des, &tid, &limtid, Inbuf, 1)) == 0)
	{
		mp = Map;
		offset = 0;
		for (i = 0; i < Mapcount; i++)
		{
			/* For cases of char to numeric conversion,
			   there must be a null byte at the end of the
			   string. The character just past the current
			   domain is saved an a null byte inserted */
			cp = &Inbuf[mp->roffset + mp->rlen];	/* compute address */
			save = *cp;	/* get the character */
			*cp = '\0';	/* insert a null */
			j = transfer(&Inbuf[mp->roffset], mp->rtype, mp->rlen,
				     mp->ftype, mp->flen, offset);
			if (j)
			{
				/* bad ascii to numeric conversion or field length too small */
				return (nferror(j, mp->paramname, &Inbuf[mp->roffset], locv(Tupcount), Relname, Filename, 0));
			}
			*cp = save;	/* restore the saved character */
			offset += mp->flen;
			mp++;
		}
		Tupcount++;
		if (fwrite(Outbuf, 1, offset, File_iop) != offset)
			syserr("copy:cant write to user file %s", Filename);
	}
	if (i < 0)
		syserr("bad get from rel %d", i);
	return (0);
}
/*
**	file_rel is called to transfer tuples from
**	the input file and append them to the relation
**
**	Char domains are initialized to blank and numeric
**	domains are initialized to zero.
*/

file_rel()
{
	register int		i, j;
	register struct map	*mp;
	struct tup_id		tid;

	clr_tuple(&Des, Outbuf);

	/* copy domains until an end of file or an error */
	for (;;)
	{
		mp = Map;
		for (i = 0; i < Mapcount; i++)
		{
			if ((j = bread(mp)) <= 0)
			{
				if (j < 0)
				{
					i = 1;	/* force an error */
					j = 5815;	/* unterminated string */
				}
				else
					j = 5810;	/* end of file */
				if (i)	/* error only if end of file during a tuple or unterminated string */
				{
					i = nferror(j, mp->paramname, locv(Tupcount), Filename, Relname, 0);
				}
				return (i);
			}
			j = transfer(Inbuf, mp->ftype, mp->flen, mp->rtype, mp->rlen, mp->roffset);
			if (j)
			{
				/* bad ascii to numeric or field length too small */
				return (nferror(j, mp->paramname, Inbuf, locv(Tupcount), Filename, Relname, 0));
			}
			mp++;
		}
		Tupcount++;
		if ((j = insert(&Des, &tid, Outbuf, 1)) < 0)
			syserr("insert error %d rel=%s", j, Relname);
		if (j == 1)
			Duptuple++;
		mp++;
	}
	/* 
	** This statement was here -- i don'T think it does anything, but we'll see
	** return (0); 
	*/
}
/*
**	transfer copies data from "*in" to
**	Outbuf doing conversions whenever
**	necessary
*/

transfer(in, sf, sl, df, dl, doff)
ANYTYPE	*in;	/* pointer to input chars */
char	sf;	/* source format */
int	sl;	/* source length */
char	df;	/* destination format */
int	dl;	/* destination length */
int	doff;	/* destination offset */
{
	register char		*outp;
	register ANYTYPE	*inp;
	register int		i;
	int			j;
	char			temp[MAXFIELD];	/* holds char during conversions to ascii */
	float			f;
	double			d;
	long			l;

	outp = &Outbuf[doff];
	inp = in;

	if (sf == DUMMY)
		/* if source format is a dummy fields then
		   nothing else need be done */
		return (0);

	if (df == DUMMY)
	{
		/* fill field with dummy domain character */
		i = dl;	/* i equals the number of chars */
		while (i--)
			*outp++ = sf;	/* sf holds dummy char */
		return (0);
	}

	if (sf != CHAR)
	{
		if (df == CHAR)	/* numeric to char conversion */
		{
			switch (sl)
			{
			  /* int of size 1 or 2 */
			  case 1:
				itoa(inp->i1type, temp);
				break;

			  case 2:
				itoa(inp->i2type, temp);	/* convert to ascii */
				break;

			  /* int or float of size 4 */
			  case 4:
				if (sf == INT)
				{
					smove(locv(inp->i4type), temp);	/* convert and copy */
				}

				else
				{
					ftoa(inp->f4type, temp, dl, Out_arg.f4prec, Out_arg.f4style);
				}
				break;

			  /* float of size 8 */
			  case 8:
				ftoa(inp->f8type, temp, dl, Out_arg.f8prec, Out_arg.f8style);
				break;

			  /* there is no possible default */
			  default:
				syserr("bad domain length %d",sl);
			}

			j = length(temp);
			if ((i = dl - j) < 0)
				return (5808);	/* field won't fit */

			/* blank pad from left. Number will be right justified */
			while (i--)
				*outp++ = ' ';

			bmove(temp, outp, j);
			return (0);
		}

		if (convert(inp, outp, sf, sl, df, dl))	/* numeric to numeric transfer */
			return (5808);	/* numeric truncation error */
		return (0);
	}

	/* character to numeric conversion */
	/* and character to character conversion */
	switch (df)
	{

	  case CHAR:
		i = sl;
		if (!i)
		{
			i = length(inp->c0type);
		}
		if (i > dl)
			i = dl;
		if (charmove(inp->c0type, outp, i))
			Baddoms++;
		for (outp += i; i<dl; i++)
			*outp++ = ' ';
		return (0);

	  case FLOAT:
		if (atof(inp->c0type, &d))
			return (5809);	/* bad conversion to numeric */
		if (dl == 8)
			bmove(&d, outp, dl);
		else
		{
			f = d;	/* f8 to f4 conversion */
			bmove(&f, outp, dl);
		}
		return (0);

	  case INT:
		if (dl == 4)
		{
			if (atol(inp->c0type, &l))
				return (5809);
			bmove(&l, outp, 4);
			return (0);
		}
		if (atoi(inp->c0type, &j))
			return (5809);
		if ((dl == 1) && ((j < -128) || (j > 127)))
			return (5809);
		bmove(&j, outp, dl);
		return (0);
	}
}
/*
**	moves a character string from "in"
**	to "out" removing any control characters.
**	returns true if any control characters were found
*/

charmove(in, out, length)
char	*in, *out;
int	length;
{
	register char	*ip, *op;
	register int	l;
	int		bad;

	bad = FALSE;
	ip = in;
	op = out;
	l = length;

	while (l--)
		if ((*op++ = *ip++) < ' ')
		{
			*(op-1) = ' ';
			bad = TRUE;
		}
	return (bad);
}
/*
**	Mapfill fills the Map structure with the list
**	of user supplied attributes. It then reads
**	the list of relation attributes and checks
**	for matching attribute names.
**
**	if an error occures then mapfill returns -1
**		else it returns 0
**
**	Mapfill performs special processing on
**	dummy domains.
**
**	If no user attributes are given, then "given"=FALSE
**	and each attribute in the relation is set up to be
**	copied in the formats and order in which they
**	exist in the relation
*/

mapfill(aptr)
PARM	aptr[];
{
	register PARM		*ap;
	register struct map	*mp;
	register int		i;
	char			*fp;
	extern DESC		Attdes;
	struct attribute	att;
	struct tup_id		tid, limtid;
	int			given, cnt;
	char			*zcheck();
	char			*dumvalue();

	Mapcount = 0;
	mp = Map;
	ap = aptr;

	/* Gather list of user supplied attributes */

	while (*(ap->pv_val.pv_str) != '\0')
	{
		/* check for overflow */
		if (Mapcount == MAXMAP)
			return (error(5803, 0));	/* more than MAXMAP specifiers */

		mp->paramname = (ap->pv_val).pv_str;	/* save pointer to user supplied name */
		pmove(((ap++)->pv_val).pv_str, mp->name, MAXNAME, ' ');
		fp = ((ap++)->pv_val).pv_str;	/* fp points to format string */
		mp->used = 0;
		mp->rlen = 0;	/* zero in case this is a dummy domain */
		mp->roffset = 0;
		mp->fdelim = 0;
		/* check domain type in *fp */
		switch (*fp++)
		{

		  case 'c':
			i =  CHAR;
			if ((mp->fdelim = zcheck(fp)) == 0)
				return (-1);	/* bad delimitor */
			break;

		  case 'f':
			i = FLOAT;
			break;

		  case 'i':
			i = INT;
			break;

		  case DUMMY:
			i = DUMMY;
			if ((mp->fdelim = zcheck(fp)) == 0)
				return (-1);
			break;

		  default:
			return (error(5811, mp->paramname, --fp, 0));
		}
		mp->ftype = i;


		/* convert format length to binary */
		if (atoi(fp, &mp->flen) ||
		    mp->flen < 0 ||
		    mp->flen > 511 ||
		    (mp->ftype == FLOAT && mp->flen != 4 && mp->flen != 8) ||
		    (mp->ftype == INT && mp->flen != 1 && mp->flen != 2 && mp->flen != 4))
		{
			return (error(5804, mp->paramname, --fp, 0));	/* bad length for attribute */
		}

		/* process dummy domain if any */
		if (Into && mp->ftype == DUMMY && mp->flen)
		{
			if ((fp = dumvalue(mp->paramname)) == 0)
				return (5807);	/* bad dummy name */
			mp->rtype = *fp;	/* use first char of string */
		}

		/* check for format of type "c0delim" on copy "into" */
		if (Into && mp->flen == 0 && mp->fdelim != Delimitor)
		{
			fp = mp->fdelim;

			/* is there room for a dummy domain? */
			mp++;
			if (++Mapcount == MAXMAP)
				return (error(5803, 0));	/* no room */

			/* create a dummy entry */
			mp->ftype = DUMMY;
			mp->flen = 1;
			mp->rtype = *fp;
			mp->roffset = mp->rlen = 0;
		}

		mp++;
		Mapcount++;
	}
	/* if no atributes were given, set flag */
	if (Mapcount)
		given = TRUE;
	else
		given = FALSE;

	/* open attribute relation and prepare for scan */
	opencatalog("attribute", 0);

	setkey(&Attdes, &att, Des.reldum.relid, ATTRELID);
	setkey(&Attdes, &att, Des.reldum.relowner, ATTOWNER);

	if (find(&Attdes, EXACTKEY, &tid, &limtid, &att))
		syserr("find error for att-rel");

	/* scan Map for each relation attribute */
	while ((i = get(&Attdes, &tid, &limtid, &att, 1)) == 0)
	{
		if (!bequal(&Des, &att, MAXNAME+2))
			continue;
		/* if no user attributes were supplied, fake an entry */
		if (!given)
		{
			Mapcount++;
			mp = &Map[att.attid -1];
			mp->rtype = mp->ftype = att.attfrmt;
			mp->rlen = mp->flen = att.attfrml & 0377;
			mp->roffset = att.attoff;
			mp->used = 1;
			mp->paramname = mp->name;	/* point to name */
			bmove(att.attname, mp->name, MAXNAME);	/* copy name */
			continue;
		}
		mp = Map;

		/* check each user domain for match with relation domain */
		for (i = Mapcount; i--;  mp++)
		{
			if (mp->ftype == DUMMY)
				continue; /* ignore dummy */
			if (!bequal(mp->name, att.attname, 12))
				continue;

			mp->rtype = att.attfrmt;
			mp->rlen = att.attfrml & 0377;
			mp->roffset = att.attoff;
			mp->used++;

			/* check for special case of C0 in a copy "into" */
			if (Into && (mp->flen == 0) && mp->ftype == CHAR)
			{
				switch (mp->rtype)
				{
				  case CHAR:
					mp->flen = mp->rlen;
					break;
	
				  case INT:
					switch (mp->rlen)
					{

					  case 1:
						mp->flen = Out_arg.i1width;
						break;

					  case 2:
						mp->flen = Out_arg.i2width;
						break;

					  case 4:
						mp->flen = Out_arg.i4width;
					}
					break;
	
				  case FLOAT:
					if (mp->rlen == 4)
						mp->flen = Out_arg.f4width;
					else
						mp->flen = Out_arg.f8width;
				}
			}
			/*  if this is a copy "from" then break
			    otherwise continue. In a copy "into"
			    an attribute might be copied more than once */
			if (!Into)
				break;
		}
	}
	if (i < 0)
		syserr("bad get from att-rel %d", i);

	/* check that all user domains have been identified */
	cnt = 0;
	mp = Map;
	for (i = Mapcount; i--; mp++)
	{
		cnt += mp->flen;
		if (mp->ftype == DUMMY)
			continue;
		if (!mp->used)
		{
			return (error(5801, mp->paramname, Relname, 0));	/* unrecognizable domain name */
		}
	}
	/* check that copy into doesn't exceed buffer size */
	if (Into && cnt > BUFSIZ)
		return (error(5817, 0));	/* cnt too large */
	return (0);
}
/*
**  BREAD
*/

bread(mp)
struct map	*mp;
{
	register int	count,	i;
	register char	*inp;
	char		*dl;
	int		esc;	/* escape flag */

	count = mp->flen;
	inp = Inbuf;

	if (count)
	{
		/* block mode. read characters */
		i = fread(inp, 1, count, File_iop);

		/* null terminate */
		*(inp + count) = '\0';

		return (i == count);	/* true -> normal, false ->eof */
	}

	/* string mode read */
	/*
	** Determine the maximum size the C0 field being read can be.
	** In the case where it is being copied into a CHAR field, then
	** the size is that of the char field (+1 for the delimitor).
	** In the case of a numeric, it is limited only by the size of the
	** buffer area.
	*/
	count = mp->rtype == CHAR ? mp->rlen + 1 : BUFSIZ;
	esc = FALSE;

	for (;;)
	{
		if ((i = getc(File_iop)) == EOF)
			return (inp == Inbuf ? 0 : -1);	/* -1 -> unexpected EOF, 0 -> normal EOF */

		if (count > 0)
		{
			count--;
			*inp++ = i;
		}
		else
		{
			if (count == 0)
			{
				/* determine type of overflow */
				if (mp->rtype == CHAR)
				{
					Truncount++;
					count--;	/* read until delim */
				}
				else
				{
					return (-1);
				}
			}
		}
		if (esc)
		{
			esc = FALSE;
			continue;
		}
		if (i == ESCAPE)
		{
			esc = TRUE;
			/*
			** If esc was stored, back it up.
			*/
			if (count >= 0)
			{
				inp--;		/* remove escape char */
				count++;	/* restore counter */
			}
		}
		else
		{
			for (dl = mp->fdelim; *dl; dl++)
				if (*dl == i)
				{
					*(inp-1) = '\0';
					return (1);
				}
		}
	}
}
/*
**	Look for the existence of a param of the
**	form "0nl" or "00comma" etc.
**
**	Returns the correct delim list or 0
**	if there was a user error
**
**	If successful, a null is inserted at the
**	rightmost '0' so the subsequent atoi will work.
*/

char *
zcheck(param)
char	*param;
{
	register char	*np, *ret;

	np = param;
	ret = Delimitor;	/* assume default delimitors */

	if (*np++ == '0')
	{
		/* we have a starting zero. trim the rest */
		while (*np == '0')
			np++;

		if (*np > '9' || (*np < '0' && *np >= ' '))
		{
			/* we have a special delim on a 0 width field */
			if (ret = dumvalue(np))
				*(--np) = '\0';	/*
						** end string before delim
						** Do not alter delimitor but
						** instead destroy last '0'.
						*/
		}
	}
	return (ret);
}
/*
**	Search list of valid dummy names looking
**	for 'name'. If 'name' is a single char
**	then use just that name else it is
**	an error if the name is not found
*/

char *
dumvalue(name)
char	*name;
{
	register char	**dp, *np, *ret;

	dp = Cpdomains;	/* get list of valid dummy names */
	np = name;
	ret = 0;

	/* first look for a matching key word */
	while (*dp)
	{
		if (sequal(np, *dp++))
		{
			ret = *dp;
			break;
		}
		dp++;
	}

	/* If single char, use that char */
	if (length(np) == 1)
		ret = np;	/* use first char of name */
	if (ret == 0)
		error(5807, np, 0);

	return (ret);
}
