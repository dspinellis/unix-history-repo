/* $Header: fortune.c,v 1.10 85/11/01 15:19:49 arnold Exp $ */

# include	<sys/types.h>
# include	<stdio.h>
# include	<sys/file.h>
# include	"strfile.h"

# define	TRUE	1
# define	FALSE	0
# define	bool	short

# define	MINW	6		/* minimum wait if desired */
# define	CPERS	20		/* # of chars for each sec */
# define	SLEN	160		/* # of chars in short fortune */

# define	FORTFILE	"/usr/games/lib/fortunes.dat"

bool	Wflag		= FALSE,	/* wait desired after fortune */
	Sflag		= FALSE,	/* short fortune desired */
	Lflag		= FALSE,	/* long fortune desired */
	Oflag		= FALSE,	/* offensive fortunes only */
	Aflag		= FALSE;	/* any fortune allowed */

char	*Fortfile	= FORTFILE,	/* fortune database */
	*Usage[]	= {
       "usage:  fortune [ - ] [ -wsloa ] [ file ]",
       "	- - give this summary of usage",
       "	w - have program wait after printing message in order",
       "	    to give time to read",
       "	s - short fortune only",
       "	l - long fortune only",
       "	o - offensive fortunes only",
       "	a - any fortune",
       "		Mail suggested fortunes to \"fortune\"",
	NULL
	};

long	Seekpts[2];			/* seek pointers to fortunes */

FILE	*Inf;				/* input file */

STRFILE	Tbl;				/* input table */

time_t	time();

main(ac, av)
int	ac;
char	*av[];
{
	register char	c;
	register int	nchar = 0;
	register int	i;

	getargs(ac, av);
	if ((Inf = fopen(Fortfile, "r+")) == NULL) {
		perror(Fortfile);
		exit(-1);
	}
	fread((char *) &Tbl, sizeof Tbl, 1, Inf);	/* NOSTRICT */
	if (Tbl.str_longlen <= SLEN && Lflag) {
		puts("Sorry, no long strings in this file");
		exit(0);
	}
	if (Tbl.str_shortlen > SLEN && Sflag) {
		puts("Sorry, no short strings in this file");
		exit(0);
	}

	/*
	 * initialize the pointer to the first -o fortune if need be.
	 */
	if (Tbl.str_delims[2] == 0)
		Tbl.str_delims[2] = Tbl.str_delims[0];

	do {
		getfort();
	} while ((Sflag && !is_short()) || (Lflag && !is_long()));

	fseek(Inf, Seekpts[0], 0);
	while (c = getc(Inf)) {
		nchar++;
		putchar(c);
	}
	fflush(stdout);
	fseek(Inf, 0L, 0);
#ifdef	LOCK_EX
	/*
	 * if we can, we exclusive lock, but since it isn't very
	 * important, we just punt if we don't have easy locking
	 * available.
	 */
	flock(fileno(Inf), LOCK_EX);
#endif	LOCK_EX
	fwrite(&Tbl, 1, sizeof Tbl, Inf);
#ifdef	LOCK_EX
	flock(fileno(Inf), LOCK_UN);
#endif	LOCK_EX
	if (Wflag)
		sleep(max((int) nchar / CPERS, MINW));
	exit(0);
}

/*
 * is_short:
 *	Return TRUE if fortune is "short".
 */
is_short()
{
	register int	nchar;

	if (!(Tbl.str_flags & (STR_RANDOM | STR_ORDERED)))
		return (Seekpts[1] - Seekpts[0] <= SLEN);
	fseek(Inf, Seekpts[0], 0);
	nchar = 0;
	while (getc(Inf))
		nchar++;
	return (nchar <= SLEN);
}

/*
 * is_long:
 *	Return TRUE if fortune is "long".
 */
is_long()
{
	register int	nchar;

	if (!(Tbl.str_flags & (STR_RANDOM | STR_ORDERED)))
		return (Seekpts[1] - Seekpts[0] > SLEN);
	fseek(Inf, Seekpts[0], 0);
	nchar = 0;
	while (getc(Inf))
		nchar++;
	return (nchar > SLEN);
}

/*
 *	This routine evaluates the arguments on the command line
 */
getargs(ac, av)
register int	ac;
register char	*av[];
{
	register int	i;
	register char	*sp;
	register int	j;
	register short	bad = 0;

	for (i = 1; i < ac; i++)  {
		if (av[i][0] != '-') {
			setuid(getuid());
			setgid(getgid());
			Fortfile = av[i];
		}
		else if (av[i][1] == '\0') {
			j = 0;
			while (Usage[j] != NULL)
				puts(Usage[j++]);
			exit(0);
			/* NOTREACHED */
		}
		else
			for (sp = &av[i][1]; *sp != '\0'; sp++)
				switch (*sp) {
				  case 'w':	/* give time to read */
					Wflag++;
					break;
				  case 's':	/* short ones only */
					Sflag++;
					break;
				  case 'l':	/* long ones only */
					Lflag++;
					break;
				  case 'o':	/* offensive ones only */
					Oflag++;
					break;
				  case 'a':	/* any fortune */
					Aflag++;
					/*
					 * initialize the random number
					 * generator; throw away the first
					 * few numbers to avoid any non-
					 * randomness in startup
					 */
					srnd(time(NULL) + getpid());
					for (j = 0; j < 20; j++)
						(void) rnd(100);
					break;
				  default:
					printf("unknown flag: '%c'\n", *sp);
					bad++;
					break;
				}
	}
	if (bad) {
		printf("use \"%s -\" to get usage\n", av[0]);
		exit(-1);
	}
}

/*
 * getfort:
 *	Get the fortune data file's seek pointer for the next fortune.
 */
getfort()
{
	register int	fortune;

	/*
	 * Make sure all values are in range.
	 */

	if (Tbl.str_delims[1] >= Tbl.str_delims[0])
		Tbl.str_delims[1] = 0;
	if (Tbl.str_delims[2] >= Tbl.str_numstr)
		Tbl.str_delims[2] = Tbl.str_delims[0];

	if (Aflag) {
		if (rnd(Tbl.str_numstr) < Tbl.str_delims[0])
			fortune = Tbl.str_delims[1]++;
		else
			fortune = Tbl.str_delims[2]++;
	}
	else if (Oflag)
		fortune = Tbl.str_delims[2]++;
	else
		fortune = Tbl.str_delims[1]++;

	fseek(Inf, (long)(sizeof Seekpts[0]) * fortune + sizeof Tbl, 0);
	fread((char *) Seekpts, (sizeof Seekpts[0]), 2, Inf);
}

max(i, j)
register int	i, j;
{
	return (i >= j ? i : j);
}
