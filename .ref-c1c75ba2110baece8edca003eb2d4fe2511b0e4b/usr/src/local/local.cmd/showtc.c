#ifndef lint
static char *sccsid="@(#)showtc.c	1.9	(Berkeley) %G%";
#endif

/*
** show termcap entries
**
** where:
**	-D	look for duplicate names and print termcap file
**	-S	sort entries before display
**	-T	trace (-DDEBUG only)
**	-U	print unknown capabilities
**	-b	show bare entries
**	-d	-D and stop
**	-f	following arg is FULL PATHNAME of termcap file
**	-g	sort on generic names
**	-s	don't print two char name at the front of every line
**	-x	expand tc= capabilities
**	[ent]	display specific entry. tc= will be expanded.
**
** David L. Wasley, U.C.Berkeley
** Kevin Layer: modified for 4.1c and misc changes.
** Kevin Layer: added the printing of terminal capabilities
**	in `human' readable form (like that in "man 5 termcap").
*/

#include <stdio.h>
#include <sys/file.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/stat.h>

#define NO		0
#define YES		1
#define CNULL		'\0'
#define NOENTRIES	1024
#define USAGE		"usage: %s [-Sxdngb] [-f termcapfile] [entry] ...\n"

#ifndef	MAXPATHLEN
#define MAXPATHLEN	1024
#endif

struct TcName {
	char	name_buf[124];
	long	file_pos;
} tcNames[NOENTRIES];

struct Caps {
	char	*cap;
	char	*desc;
} capList[] =
{
	"AL",	"Add N new blank lines",
	"CC",	"Command char in prototype if settable",
	"DC",	"Delete N characters",
	"DL",	"Delete N lines",
	"DO",	"Move cursor down N lines",
	"IC",	"Insert N blank characters",
	"LE",	"Move cursor left N positions",
	"RI",	"Move cursor right N positions",
	"UP",	"Move cursor up N lines",
	"ae",	"End alternate character set",
	"al",	"Add new blank line",
	"am",	"Has automatic margins",
	"as",	"Start alternate character set",
	"bc",	"Backspace if not ^H",
	"bl",	"Audible Bell (default ^G)",
	"bs",	"Can backspace with ^H",
	"bt",	"Back tab",
	"bw",	"Backspace wraps from col 0 to last col",
	"cd",	"Clear to end of display",
	"ce",	"Clear to end of line",
	"ch",	"Like cm, but horizontal motion only",
	"cl",	"Clear screen",
	"cm",	"Cursor motion",
	"co",	"Number of columns in a line",
	"cr",	"Carriage return (default ^M)",
	"cs",	"Change scrolling region (vt100), like cm",
	"ct",	"Clear all tab stops",
	"cv",	"Like ch but vertical only.",
	"dB",	"Number of millisec of bs delay needed",
	"dC",	"Number of millisec of cr delay needed",
	"dF",	"Number of millisec of ff delay needed",
	"dN",	"Number of millisec of nl delay needed",
	"dT",	"Number of millisec of tab delay needed",
	"da",	"Display may be retained above",
	"db",	"Display may be retained below",
	"dc",	"Delete character",
	"dl",	"Delete line",
	"dm",	"Start Delete mode",
	"do",	"Down one line",
	"ds",	"Disable status display",
	"ed",	"End delete mode",
	"ei",	"End insert mode;give \":ei=:\" if ic",
	"eo",	"Can erase overstrikes with a blank",
	"es",	"Escape seq's ok on status line",
	"ff",	"Hardcopy page eject (default ^L)",
	"fs",	"From status line sequence",
	"hc",	"Hardcopy terminal",
	"hd",	"Half-line down (forward 1/2 lf)",
	"ho",	"Home cursor (if no cm)",
	"hs",	"Has status line",
	"hu",	"Half-line up (reverse 1/2 lf)",
	"hz",	"Hazeltine; can't print ~'s",
	"i2",	"Initialization string (used by sysline(1))",
	"ic",	"Insert character",
	"if",	"Name of file containing is",
	"im",	"Start insert mode;give \":im=:\" if ic",
	"in",	"Insert mode distinguishes nulls on display",
	"ip",	"Insert pad after character inserted",
	"is",	"Initialization string",
	"k0",	"Sent by function key 0",
	"k1",	"Sent by function key 1",
	"k2",	"Sent by function key 2",
	"k3",	"Sent by function key 3",
	"k4",	"Sent by function key 4",
	"k5",	"Sent by function key 5",
	"k6",	"Sent by function key 6",
	"k7",	"Sent by function key 7",
	"k8",	"Sent by function key 8",
	"k9",	"Sent by function key 9",
	"kb",	"Sent by backspace key",
	"kd",	"Sent by down arrow key",
	"ke",	"Out of \"keypad transmit\" mode",
	"kh",	"Sent by home key",
	"kl",	"Sent by left arrow key",
	"km",	"Has a \"meta\" key (shift, sets parity bit)",
	"kn",	"Number of \"other\" keys",
	"ko",	"Tc entries for other non-function keys",
	"kr",	"Sent by right arrow key",
	"ks",	"Put in \"keypad transmit\" mode",
	"ku",	"Sent by up arrow key",
	"l0",	"Label on function key 0 (if not \"0\")",
	"l1",	"Label on function key 1 (if not \"1\")",
	"l2",	"Label on function key 2 (if not \"2\")",
	"l3",	"Label on function key 3 (if not \"3\")",
	"l4",	"Label on function key 4 (if not \"4\")",
	"l5",	"Label on function key 5 (if not \"5\")",
	"l6",	"Label on function key 6 (if not \"6\")",
	"l7",	"Label on function key 7 (if not \"7\")",
	"l8",	"Label on function key 8 (if not \"8\")",
	"l9",	"Label on function key 9 (if not \"9\")",
	"le",	"Move left",
	"li",	"Number of lines on screen or page",
	"ll",	"Last line, first column (if no cm)",
	"ma",	"Arrow key map, used by vi V2 only",
	"mb",	"Enter blinking mode",
	"md",	"Enter bold mode",
	"me",	"Reset video attributes",
	"mh",	"Enter halfbright mode",
	"mi",	"Safe to move while in insert mode",
	"mk",	"Enter protected mode",
	"ml",	"Memory lock on above cursor.",
	"mp",	"Turn on protected attribute",
	"mr",	"Enter reverse video mode",
	"ms",	"Ok to move while in standout/underline mode",
	"mu",	"Memory unlock (turn off memory lock).",
	"nc",	"No working CR (DM2500,H2000)",
	"nd",	"Non-destructive space (cursor right)",
	"nl",	"Newline character (default \\n)",
	"ns",	"Is a CRT but doesn't scroll.",
	"os",	"Terminal overstrikes",
	"pb",	"Lowest baud where delays are required",
	"pc",	"Pad character (rather than null)",
	"pl",	"Program function key N to execute string S (terminfo only)",
	"pt",	"Has hardware tabs (may need to use is)",
	"rc",	"Restore cursor to position of last sc",
	"rf",	"Name of file containing reset codes",
	"rs",	"Reset terminal completely to sane modes",
	"sc",	"Save cursor position",
	"se",	"End stand out mode",
	"sf",	"Scroll forwards",
	"sg",	"Number of blank chars left by so/se",
	"so",	"Begin stand out mode",
	"sr",	"Scroll reverse (backwards)",
	"st",	"Set a tab in all rows, current column",
	"ta",	"Tab (other than ^I or with padding)",
	"tc",	"Entry of similar terminal - must be last",
	"te",	"String to end programs that use cm",
	"ti",	"String to begin programs that use cm",
	"ts",	"To status line sequence",
	"uc",	"Underscore one char and move past it",
	"ue",	"End underscore mode",
	"ug",	"Number of blank chars left by us or ue",
	"ul",	"Underlines, though no overstrike",
	"up",	"Upline (cursor up)",
	"us",	"Start underscore mode",
	"vb",	"Visible bell (may not move cursor)",
	"ve",	"Sequence to end open/visual mode",
	"vs",	"Sequence to start open/visual mode",
	"vt",	"Virtual terminal number (not supported on all systems)",
	"xb",	"Beehive (f1=escape, f2=ctrl C)",
	"xn",	"A newline is ignored after a wrap (Concept)",
	"xr",	"Return acts like ce \\r \\n (Delta Data)",
	"xs",	"Standout not erased by writing over it (HP 264?)",
	"xt",	"Destructive tabs, magic so char (Teleray 1061)"
};

#define NOCAPS	(sizeof capList / sizeof *capList)

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
int		Uflag = NO;
int		tc_loopc;		/* loop counter */
char		*tcfile;		/* termcap database pathname */
char		cwd[MAXPATHLEN];	/* current working directory */
char		tcbuf[2048];		/* buffer for termcap description */
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
				case 'd':
					nflag = YES;
					/* fall thru */

				/* look for duplicated names */
				case 'D':
					dflag = YES;
					continue;

				case 'U':
					Uflag = YES;
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
				case 'T':
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
	if (*tcfile != '/') {
		char	*getwd();

		if (getwd(cwd) == NULL) {
			fprintf(stderr, "showtc: %s\n", cwd);
			exit(1);
		} else if (strlen(cwd) + strlen(tcfile) + 2 > sizeof cwd) {
			fprintf(stderr, "showtc: %s\n",
				"Current working directory name too long");
			exit(1);
		} else {
			if (cwd[strlen(cwd) - 1] != '/')
				strcat(cwd, "/");
			strcat(cwd, tcfile);
			tcfile = cwd;
		}
	}
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
	char		*getdesc();
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
		if (Uflag) {
			if (unknowncap(*cp)) {
				printf("%3.3s\n", *cp);
			}
		} else if (sflag) {
			printf("%-45s %s\n", getdesc(*cp), *cp);
		} else {
			printf("%2.2s  %-45s %s\n", name, getdesc(*cp), *cp);
		}
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

char *
getdesc(key)
	char		*key;
{
	register int	i;

	for (i = 0; i < NOCAPS; i++)
		if (strncmp(key, capList[i].cap, 2) == 0)
			return (capList[i].desc);
	return("");
}

unknowncap(key)
	char		*key;
{
	register int	i;

	for (i = 0; i < NOCAPS; i++)
		if (strncmp(key, capList[i].cap, 2) == 0)
			return (0);
	return(1);
}
