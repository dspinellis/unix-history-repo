# include	<sccs.h>

SCCSID(@(#)timefix.c	7.1	2/5/81)

/*
** TIMEFIX -- patch binary program to correct for timezone changes.
**
**	Timefix is compiled with the ctime(III) variables:
**		daylight,
**		timezone,
**		tzname[]
**
**	Each file specified is examined to see if it contains
**	all of these variables. If it does then the current values
**	of those variables for that file are given. If the "-u" flag
**	is specified then the values are not overwritten.
**
**	The other flags can be used to override the values of the
**	variables; specifically:
**		-sxxx  --> timezone	(xxx converted from ascii to binary)
**		-dx    --> daylight	(x converted from ascii to binary)
**		-tXXXYYY -> tzname[0] = "XXX" and tzname[1] = "YYY"
*/

struct header
{
	int	magic;
	int	tsize;
	int	dsize;
	int	bss;
	int	ssize;
	int	start_ad;
	int	unused;
	int	reloc_flag;
};

struct sym
{
	char	symname[8];
	int	type;
	int	value;
};


/* these values are defined in the unix ctime() routine */
extern int	daylight;
extern int	timezone;
extern char	*tzname[];

int	Noupdate;

main(argc, argv)
int	argc;
char	*argv[];
{

	if (argc < 2)
	{
		printf("usage: timefix filename ...\n");
		exit (1);
	}

	argc--;
	argv++;

	if (checkflags(&argc, argv))
		exit (1);

	pr_values("your installation", timezone, tzname[0], tzname[1], daylight);

	while (argc--)
		newtime(*argv++);
}
/*
**  NEWTIME
*/

newtime(filename)
char	*filename;
{
	struct sym	timez, name_st, name_dy, dayflag;
	struct header	head;
	register int	i, fdes;
	int		adr, secs;
	char		std[4], dyl[4];

	if ((fdes = openheader(&head, filename)) < 0)
		return (-1);

	bmove("_timezon", timez.symname, 8);
	bmove("_tzname\0", name_st.symname, 8);
	bmove("_dayligh", dayflag.symname, 8);

	/* pick up addresses from symbol table */
	i = findsymbol(&head, fdes, &timez);
	i |= findsymbol(&head, fdes, &name_st);
	i |= findsymbol(&head, fdes, &dayflag);

	if (i)
	{
		printf("File %s does not need to be corrected\n", filename);
		close(fdes);
		return (-2);
	}

	/* form entries for pointer to "PST" and "PDT" */
	i = getvalue(&head, fdes, &name_st, &adr, 2);
	name_st.value += 2;
	i |= getvalue(&head, fdes, &name_st, &name_dy.value, 2);
	name_dy.type = name_st.type;
	name_st.value = adr;

	if (i)
	{
		printf("can't find pointers to timezone names in %s\n", filename);
		close(fdes);
		return (-3);
	}

	/* now print out current values */
	i = getvalue(&head, fdes, &timez, &secs, 2);
	i |= getvalue(&head, fdes, &name_st, std, 4);
	i |= getvalue(&head, fdes, &name_dy, dyl, 4);
	i |= getvalue(&head, fdes, &dayflag, &adr, 2);

	if (i)
	{
		printf("one or more symbols cannot be read from %s\n", filename);
		close(fdes);
		return (-4);
	}

	pr_values(filename, secs, std, dyl, adr);

	if (!Noupdate)
	{
		if (putvalue(&head, fdes, &timez, &timezone, 2)
			|| putvalue(&head, fdes, &name_st, tzname[0], 4)
			|| putvalue(&head, fdes, &name_dy, tzname[1], 4)
			|| putvalue(&head, fdes, &dayflag, &daylight, 2))
		{
			printf("cannot update %s with new values\n", filename);
			close(fdes);
			return (-2);
		}
		else
			printf("File %s updated.\n", filename);
	}


	close(fdes);

	return (0);
}
/*
**	Open the indicated file and read & verify header
*/

openheader(hd, filename)
struct header	*hd;
char		*filename;
{
	register int	fd, i;

	if ((fd = open(filename, 2)) < 0)
	{
		printf("can't open file %s\n", filename);
		return (-1);
	}

	if ((i = read(fd, hd, sizeof (*hd))) != sizeof (*hd))
	{
		printf("can't read in header\n");
		close(fd);
		return  (-2);
	}

	switch (hd->magic)
	{

	  case 0407:
	  case 0410:
	  case 0411:
		if (hd->ssize)
			break;
		printf("File %s does not have a symbol table.\n", filename);
		return (-4);

	  default:
		printf("%s not an object file\n", filename);
		return (-3);
	}

	return (fd);
}
/*
**	Seek to beginning of symbol table
*/

startsymbol(hd, fdx)
struct header	*hd;
int		fdx;
{
	register int		i, fd;
	register struct header	*h;
	long			offset;
	long			itol();

	h = hd;
	fd = fdx;

	/* seek past header */
	i = lseek(fd, 16L, 0);

	/* seek to start of symbol table */
	offset = itol(h->tsize);
	offset = offset + itol(h->dsize);
	if (h->reloc_flag == 0)
		offset += offset;

	i |= lseek(fd, offset, 1);

	if (i < 0)
	{
		printf("can't seek to symbol table\n");
		return (-1);
	}

	return (0);
}
/*
**	Locate symbol in symbol table and return sucess-failure
*/

findsymbol(hd, fd, s)
struct header	*hd;
int		fd;
struct sym	*s;
{
	register int	i, j;
	struct sym	next;

	if (startsymbol(hd, fd))
		return (-1);

	for (i = hd->ssize; i--; )
	{
		j = read(fd, &next, sizeof (next));

		if (j != sizeof (next))
		{
			if (j)
				printf("symbol table error %d,%d,%d\n", hd->ssize, i, j);
			return (-1);
		}

		if (bequal(next.symname, s->symname, sizeof (next.symname)))
		{
			s->type = next.type;
			s->value = next.value;
			return (0);
		}
	}

	return (1);
}
/*
**  GETVALUE
*/

getvalue(hd, fd, s, cp, len)
struct header	*hd;
int		fd;
struct sym	*s;
char		*cp;
int		len;
{
	register int	i;
	long		getaddr(), addr;

	addr = getaddr(hd, s);
	if (addr == -1)
		return (-1);

	if (lseek(fd, addr, 0) < 0)
		return (-1);

	if ((i = read(fd, cp, len)) != len)
		return (-1);

	return (0);
}
/*
**  PUTVALUE
*/


putvalue(hd, fd, s, loc, len)
struct header	*hd;
int		fd;
struct sym	*s;
char		*loc;
int		len;
{
	long		adr, getaddr();

	adr = getaddr(hd, s);
	if (adr == -1)
		return (-1);

	if (lseek(fd, adr, 0) < 0)
		return (-1);

	if (write(fd, loc, len) != len)
		return (-2);

	return (0);
}
/*
**  PR_VALUES
*/

pr_values(str, secs, std, dyl, flag)
char	*str;
int	secs;
char	*std;
char	*dyl;
int	flag;
{
	printf("\nCurrent values for %s are:\n\t# seconds past greenwich:  %d\n", str, secs);
	printf("\ttimezones:  %.4s and %.4s\n\tdaylight saving flag:  %d\n", std, dyl, flag);
}
/*
**  CHECKFLAGS
*/

checkflags(argc, argv)
int	*argc;
char	*argv[];
{
	register char	*cp, **nargv;
	register int	cnt;
	int		ret;

	ret = 0;
	cnt = *argc;
	nargv = argv;
	while (cnt--)
	{
		cp = *argv++;
		if (*cp == '-')
		{
			(*argc)--;
			cp++;
			switch (*cp++)
			{

			  case 's':
				timezone = atoi(cp);
				break;

			  case 't':
				bmove(cp, tzname[0], 3);
				bmove(cp+3, tzname[1], 3);
				break;

			  case 'd':
				daylight = atoi(cp);
				break;

			  case 'u':
				Noupdate++;
				break;

			  default:
				printf("bad flag %s\n", cp - 2);
				ret = -1;
			}
		}
		else
			*nargv++ = cp;
	}

	return (ret);
}


bmove(src, dst, len)
char	*src;
char	*dst;
int	len;
{
	register int	i;
	register char	*s, *d;

	s = src;
	d = dst;
	i = len;

	while (i--)
		*d++ = *s++;
}


bequal(s1, s2, len)
char	*s1;
char	*s2;
int	len;
{
	register int	i;
	register char	*s, *d;

	s = s1;
	d = s2;
	i = len;

	while (i--)
		if (*s++ != *d++)
			return (0);

	return (1);
}
/*
**  GETADDR
*/

long
getaddr(hd, s)
struct header	*hd;
struct sym	*s;
{
	long	addr;
	int	i;
	long	l;
	long	itol();

	addr = s->value + 16;	/* offset past header */

	switch (s->type)
	{

	  /* extern text segment */
	  case 042:
		return (addr);

	  /* data segment extern */
	  case 043:
		switch (hd->magic)
		{

		  case 0407:
			return (addr);

		  case 0410:
			/* subtract space between text and data */
			l = itol(hd->tsize) + 017777;
			l &= ~017777;

			return (addr - (l - itol(hd->tsize)));

		  case 0411:
			return (addr + itol(hd->tsize));

		}
	  default:
		printf("Invalid symbol type %o\n", s->type);
		return (-1);
	}
}


long itol(value)
int	value;
{
	long	ret;
	int	*ip;

	ret = value;
	ip = &ret;
	*ip = 0;

	return (ret);
}
