#ifdef UCB_SCCSID
	char	*sccsid = "@(#)fortune.c	2.6";
#endif
# include	<stdio.h>
# include	"strfile.h"

# define	MINW	6		/* minimum wait if desired	*/
# define	CPERS	20		/* # of chars for each sec	*/
# define	SLEN	160		/* # of chars in short fortune	*/

# define	reg	register

short	wflag		= 0,		/* wait desired after fortune	*/
	sflag		= 0,		/* short fortune desired	*/
	lflag		= 0,		/* long fortune desired		*/
	oflag		= 0,		/* offensive fortunes only	*/
	aflag		= 0;		/* any fortune allowed		*/

char	fortfile[100]	= FORTFILE,	/* fortune database		*/
	*usage[]	= {
       "usage:  fortune [ - ] [ -wsloa ] [ file ]",
       "	- - give this summary of usage",
       "	w - have program wait after printing message in order",
       "	    to give time to read",
       "	s - short fortune only",
       "	l - long fortune only",
       "	o - offensive fortunes only",
       "	a - any fortune",
       "		Mail suggested fortunes to \"fortune\""
	};

long	seekpts[2];			/* seek pointers to fortunes	*/

STRFILE	tbl;				/* input table			*/

main(ac, av)
int	ac;
char	*av[]; {

	reg char	c;
	reg int		nchar = 0;
	reg FILE	*inf;
	int		numforts,	/* number of fortunes		*/
			fortune;	/* fortune number		*/

	getargs(ac, av);
	if ((inf = fopen(fortfile, "r")) == NULL) {
		perror(fortfile);
		exit(-1);
	}
	fread(&tbl, (sizeof tbl), 1, inf);
	numforts = tbl.str_numstr - 1;	 /* always a null string at the end */
	if (tbl.str_longlen < SLEN && lflag) {
		puts("Sorry, no long strings in this file");
		exit(0);
	}
	if (tbl.str_shortlen > SLEN && sflag) {
		puts("Sorry, no short strings in this file");
		exit(0);
	}
	if (oflag)
		numforts -= tbl.str_delims[0];
	else if (!aflag)
		numforts = tbl.str_delims[0];
	srand(getpid());
	for (nchar = 15; nchar--; )
		rand();		/* throw away first rand()s to make it better */
	getfort(numforts, inf);
	if (sflag)
		while (seekpts[1] - seekpts[0] >= SLEN)
			getfort(numforts, inf);
	else if (lflag)
		while (seekpts[1] - seekpts[0] < SLEN)
			getfort(numforts, inf);
	fseek(inf, seekpts[0], 0);
	while (c = getc(inf)) {
		nchar++;
		putchar(c);
	}
	fflush(stdout);
	if (wflag)
		sleep(max((int) nchar/CPERS, MINW));
}

/*
 *	This routine evaluates the arguments on the command line
 */
getargs(ac, av)
int		ac;
reg char	*av[]; {

	reg short	bad = 0;
	reg int		i, j;

	for (i = 1; i < ac; i++)  {
		if (av[i][0] != '-')
			strcpy(fortfile, av[i]);
		else
			switch (av[i][1]) {
			  case '\0':	/* give usage			*/
				for (j = 0; j < sizeof usage / sizeof (char *); j++)
					puts(usage[j]);
				exit(0);
			  case 'w':	/* give time to read		*/
				wflag++;
				break;
			  case 's':	/* short ones only		*/
				sflag++;
				break;
			  case 'l':	/* long ones only		*/
				lflag++;
				break;
			  case 'o':	/* offensive ones only		*/
				oflag++;
				break;
			  case 'a':	/* any fortune			*/
				aflag++;
				break;
			  default:
				printf("unknown flag: '%c'\n", av[1][1]);
				bad++;
				break;
			}
	}
	if (bad) {
		printf("use \"%s -\" to get usage\n", av[0]);
		exit(-1);
	}
}

getfort(numforts, inf)
reg int		numforts;
reg FILE	*inf; {

	reg int		fortune;

	fortune = roll(1, numforts) - 1;
	if (oflag && !aflag)
		fortune += tbl.str_delims[0];
	fseek(inf, (long)(sizeof seekpts[0]) * fortune + sizeof tbl, 0);
	fread(seekpts, (sizeof seekpts[0]), 2, inf);
}

max(i, j)
reg int	i, j; {

	return (i >= j ? i : j);
}

roll(ndie, nsides)
int	ndie, nsides; {

	reg int		tot;

	tot = ndie;
	while (ndie--)
		tot += (rand() >> 3) % nsides;
	return tot;
}
