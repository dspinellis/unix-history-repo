#ifndef LINT
static char *sccsid="@(#)showtc.c	1.2	(Berkeley) %G%";
#endif

/*
** show termcap entries
**
** where:
**	-S	sort entries before display
**	-b	show bare entries
**	-d	look for duplicate names
**	-f	following arg is FULL PATHNAME of termcap file
**	-g	sort on generic names
**	-n	-d and stop
**	-s	don't print two char name at the front of every line
**	-x	expand tc= capabilities
**	[ent]	display specific entry. tc= will be expanded.
**
** David L. Wasley, U.C.Berkeley
** Modified for 4.1c by Kevin Layer
*/

#include <stdio.h>
#include <sys/file.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

#define NO		0
#define YES		1
#define CNULL		'\0'
#define NOENTRIES	1024
#define USAGE		"usage: %s [-Sxdngb] [-f termcapfile] [entry] ...\n"

struct TcName {
	char	name_buf[124];
	long	file_pos;
} tcNames[NOENTRIES];

#ifdef DEBUG
int		Dflag = NO;
#endif
int		xflag = NO;
int		Sflag = YES;
int		sflag = NO;
int		dflag = NO;
int		nflag = NO;
int		gflag = NO;
int		bflag = NO;
int		tc_loopc;		/* loop counter */
char		*tcfile;		/* termcap database pathname */
char		tcbuf[1024];		/* buffer for termcap description */
char		*lastchar();
int		name_cmp();
int		ent_cmp();
struct TcName	*find_name();
char		*getenv();
char		*ctime();
char		*strncpy();
long		ftell();

main(argc, argv, envp)
	int		argc;
	char		**argv;
	char		**envp;
{
	char		*av;
	struct TcName	*tn;
	register char	*bp;
	long		pos;
	int		n;
	struct stat	st;
	char		envbuf[256];
	FILE		*tcfp;

	if ((bp = getenv("TERMCAP")) && *bp == '/')
		tcfile = bp;
	else
		tcfile = "/etc/termcap";

	while (--argc > 0)
	{
		if (*(av = *++argv) == '-')
		{
			while (*++av)
			{
				switch (*av)
				{
				/* use alternate termcap file */
				case 'f':
					if (argc-- <= 0)
					{
						fprintf(stderr,
						    "-f needs a filename\n");
						exit(1);
					}
					tcfile = *++argv;
					break;

				/* only check for dup names */
				case 'n':
					nflag = YES;
					/* fall thru */

				/* look for duplicated names */
				case 'd':
					dflag = YES;
					continue;

				/* strip the two name off */
				case 's':
					sflag = YES;
					continue;

				/* sort the name array */
				case 'S':
					Sflag = NO;
					continue;

#ifdef DEBUG
				case 'D':
					Dflag = YES;
					continue;
#endif

				/* sort on generic names */
				case 'g':
					gflag = YES;
					continue;

				/* expand entries in 'full mode' */
				case 'x':
					xflag = YES;
					continue;

				/* show bare entry */
				case 'b':
					bflag = YES;
					continue;

				default:
					fprintf(stderr, "showtc: unknown flag: -%c\n", *av);
					fprintf(stderr, USAGE, argv[0]);
					exit(1);
				}
			}
		}
		else
			break;
	}

	/*
	 * insert the specified TERMCAP file into the environment
	 */
	(void) sprintf(envbuf, "TERMCAP=%s", tcfile);
	while (*envp)
	{
		if (strncmp(*envp, "TERMCAP=", 8) == 0)
		{
			*envp = envbuf;
			break;
		}
		envp++;
	}
	if (! *envp)
		*envp = envbuf;	/* this may be dangerous */

	/*
	** if user specified type(s), do only those
	*/
	if (argc > 0)
	{
		/*
		** look for the users specified term types
		*/
		while (argc > 0)
		{
			switch (n = tgetent(tcbuf, *argv))
			{
				case 1:
					if (bflag)
						(void) prnt_raw(tcbuf);
					else
						(void) prnt_ent(tcbuf);
					break;

				case 0:
					fprintf(stderr,
					   "showtc: bad entry: %s\n", *argv);
					break;

				case -1:
					fputs("showtc: ", stderr);
					perror(tcfile);
					exit(1);

				default:
					fprintf(stderr, "bad return from tgetent: %d\n", n);
					exit(1);
			}
			argc--;
			argv++;
		}
		exit(0);
	}

	if (bflag)
	{
 		fprintf(stderr, "showtc: -b flag with no entries makes no sense.\n");
		exit(1);
	}


	/*
	** if no type was specified, do the whole file
	*/
	if ((tcfp = fopen(tcfile, "r")) == NULL)
	{
		perror(tcfile);
		exit(1);
	}

	/*
	** identify database, for the record
	*/
	if (stat(tcfile, &st))
	{
		perror(tcfile);
		exit(1);
	}
	printf("File: %s, last modified: %s\n", tcfile, ctime(&st.st_mtime));

	
	/*
	** build termcap entry table
	*/
	tn = tcNames;
	pos = ftell(tcfp);
	bp = tcbuf;
	while (fgets(bp, sizeof (tcbuf), tcfp) != NULL)
	{
		if (tcbuf[0] == '#')
		{
			pos = ftell(tcfp);
			bp = tcbuf;
			continue;
		}

		tn->file_pos = pos;

		/*
		** get full entry
		*/
		while (*(bp = lastchar(bp)) == '\\' && fgets(bp, (sizeof tcbuf) - (bp - tcbuf), tcfp))
			;
		/*
		** save the names
		*/
		for (bp = tcbuf; *bp && *bp != ':'; bp++)
			;
		*bp = '\0';
		(void) strncpy(tn->name_buf, tcbuf,
				sizeof tcNames[0].name_buf);

		pos = ftell(tcfp);
		bp = tcbuf;
		tn++;
	}
	tn->file_pos = -1;

	/*
	** Look for duplicate names
	*/
	if (dflag)
		check_dup();
	if (nflag)
		exit(0);

#ifdef DEBUG
	if (Dflag)
	{
		for (tn = tcNames; tn->file_pos >= 0; tn++)
		{
			printf("Entry #%d:\n\t%s\n\tfile_pos = %ld\n",
			tn - tcNames, tn->name_buf, tn->file_pos);
		}
		exit(0);
	}
#endif

	/*
	** Order the list
	*/
	if (Sflag)
		qsort((char *)tcNames, tn - tcNames,
			sizeof (struct TcName), name_cmp);

	/*
	** List termcap entry for each name in table
	*/
	for (tn = tcNames; tn->file_pos >= 0; tn++)
	{
		tc_loopc = 0;
		/*** working toward this ...
		(void) prnt_ent(tn);
		***/
		(void) fseek(tcfp, tn->file_pos, 0);
		bp = tcbuf;
		while (fgets(bp, (sizeof tcbuf) - (bp - tcbuf), tcfp)
			&& *(bp = lastchar(bp)) == '\\')
			;
		(void) prnt_ent(tcbuf);
	}
}

char *
lastchar(b)
	char	*b;
{
	register char	*p;

	p = b + strlen(b) - 1;
	while (*p == '\n' || *p == ' ')
		p--;
	return(p);
}

name_cmp(a, b)
	char	*a, *b;
{
	if (gflag)	/* sort on generic names */
	{
		a += 3;
		b += 3;
		while (*a && *b && *a != '|' && *a == *b)
		{
			a++;
			b++;
		}
		if (*a == '|' || *a == CNULL)
			return((*b == '|' || *b == CNULL)? 0:-1);
		if (*b == '|' || *b == CNULL)
			return(1);
		return(*a - *b);
	}
	return(strncmp(a, b, 2));
}

prnt_ent(buf)
	register char	*buf;
{
	register char	*name;
	char		*caps[256];
	register char	**cp;
	register char	**tp;
	char		tname[3];

	cp = caps;
	name = buf;
	tname[3] = '\0';

	while (*buf)
	{
		switch (*buf)
		{
		case ':':
			*buf++ = '\0';
			while (*buf && !isalnum(*buf))
				buf++;
			if (*buf)
			{
				/*
				 * ignore duplicate cap entries
				 */
				for (tp = caps; tp < cp; tp++)
					if (strncmp(buf, *tp, 2) == 0)
						goto skip;
				*cp++ = buf;
			skip:
				/*
				 * does user want tc= expanded?
				 */
				if (xflag && strncmp(buf, "tc=", 3) == 0)
				{
					/*
					 * find end of tc=
					 */
					while (*buf != ':')
						buf++;
					*buf = '\0';
					/*
					 * save term name
					 */
					tname[0] = name[0];
					tname[1] = name[1];
					printf("%s: expanding %s\n",
						tname, cp[-1]);
					/*
					 * let tgetent do the work
					 */
					tgetent(tcbuf, tname);
					prnt_ent(tcbuf);
					return;
				}
			}
			continue;

		case '|':
			*buf++ = ',';
			continue;

		default:
			buf++;
		}
	}
	*cp = CNULL;		/* was (char *)0 */

	if (Sflag)
		qsort((char *) caps, cp - caps, sizeof (char *), ent_cmp);

	printf("%s\n", name);
	for (cp = caps; *cp; cp++)
		if (sflag)
			printf("	%s\n", *cp);
		else
			printf("%2.2s	%s\n", name, *cp);
	(void) putchar('\n');
}

prnt_raw(buf)
	char		*buf;
{
	register char	*b;
	register int	len;
	register int	n;
	char		*index();

	len = 0;
	b = buf;
	while (*b)
	{
		if ((n = index(b, ':') - b + 1) <= 0)
			n = strlen(b);
		if (len == 0)			/* first part */
		{
			printf("%.*s\\\n\t:", n, b);
			len = 9 - n;
		}
		else
		{
			if ((len + n) >= 75)
			{
				printf("\\\n\t:");
				len = 9;
			}
			printf("%.*s", n, b);
		}
		len += n;
		b += n;
		while (*b && index(" \t:\n", *b))
			b++;
	}
	if (b[-1] != ':')
		(void) putchar(':');
	(void) putchar('\n');
}

ent_cmp(a, b)
	char **a, **b;
{
	return(strncmp(*a, *b, 2));
}

check_dup()
{
	/*
	** Look for duplicated names
	*/
	register char		*p;
	register char		*q;
	register struct TcName	*tn;
	register struct TcName	*tm;

	tn = tcNames;
	while (tn->file_pos >= 0)
	{
		p = q = tn->name_buf;
		while (*q)
		{
			while (*p && *p != '|')
				p++;
			if (p != q && (tm = find_name(q, tn + 1, p - q)))
			{
				fputs("Duplicate name: ", stdout);
				while (q != p)
					(void) putchar(*q++);
				(void) putchar('\n');
				puts(tn->name_buf);
				puts(tm->name_buf);
				puts("---\n");
			}
			if (*p == '|')
				p++;
			q = p;
		}
		tn++;
	}
}

struct TcName *
find_name(name, tn, len)
	register char		*name;
	register struct TcName	*tn;
	register int		len;
{
	/*
	** find name of length len in tcname structure buffers.
	*/
	register char	*p;
	register char	*buf;
	register int	 n;

	while (tn->file_pos >= 0)
	{
		buf = tn->name_buf;
		while (*buf)
		{
			p = name;
			n = len;
			if (*buf == '|')
				buf++;

			while (*buf && *buf != '|')
			{
				if (*p != *buf)
				{
					while (*buf && *buf != '|')
						buf++;
					break;
				}

				if (--n <= 0)
				{
					buf++;
					if (*buf == '|' || *buf == '\0')
						return(tn);
					while (*buf && *buf != '|')
						buf++;
					break;
				}
				buf++;
				p++;
			}
		}
		tn++;
	}
	return((struct TcName *)0);
}
