/* Copyright (c) 1983 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)jot.c	4.3	(Berkeley)	%G%";
#endif not lint

/*
 *	jot - print sequential or random data
 *	Author:  John Kunze, Office of Comp. Affairs, UCB
 */

#include <stdio.h>
#include <ctype.h>

#define	REPS_DEF	100
#define	BEGIN_DEF	1
#define	ENDER_DEF	100
#define	STEP_DEF	1

#define BSD4_2		1
#if BSD4_2
#define RAND		random
#define SRAND		srandom
long RAND();
#else
#define RAND		rand
#define SRAND		srand
#endif

#define	isdefault(s)	(strcmp((s), "-") == 0)
#define	LARGESTINT	(~ (unsigned) 0 >> 1)
#define	CASE		break; case

char	*sepstring = "\n";
char	format[BUFSIZ];
char	buf[BUFSIZ];
int	randomize;
int	infinity;
int	boring;
int	prec;
int	dox;
int	chardata;
int	nofinalnl;

long	reps;
double	begin;
double	ender;
double	s;

main(argc, argv)
int	argc;
char	**argv;
{
	double	xd, yd;
	long	id;
	register double	*x = &xd;
	register double	*y = &yd;
	register long	*i = &id;

	setbuf(stdout, buf);
	getargs(argc, argv);
	if (randomize) {
		*x = (ender - begin) * (ender > begin ? 1 : -1);
		SRAND((int) s);
		for (*i = 1; *i <= reps || infinity; (*i)++) {
			*y = (double) RAND() / LARGESTINT;
			putdata(*y * *x + begin, reps - *i);
		}
	}
	else
		for (*i = 1, *x = begin; *i <= reps || infinity; (*i)++, *x += s)
			putdata(*x, reps - *i);
	if (!nofinalnl)
		putchar('\n');
	exit(0);
}

getargs(ac, av)
int	ac;
char	**av;
{
	register unsigned int	mask = 0;
	register int		n = 0;

	while (--ac && **++av == '-' && !isdefault(*av))
		switch ((*av)[1]) {
		case 'r':
			randomize = 1;
			break;
		case 'c':
			chardata = 1;
			break;
		case 'n':
			nofinalnl = 1;
			break;
		case 'b':
			boring = 1;
		case 'w':
			if ((*av)[2])
				strcpy(format, *av + 2);
			else if (!--ac)
				error("Need context word after -w or -b", "");
			else
				strcpy(format, *++av);
			break;
		case 's':
			if ((*av)[2])
				strcpy(sepstring, *av + 2);
			else if (!--ac)
				error("Need string after -s", "");
			else
				strcpy(sepstring, *++av);
			break;
		case 'p':
			if ((*av)[2])
				prec = atoi(*av + 2);
			else if (!--ac)
				error("Need number after -p", "");
			else
				prec = atoi(*++av);
			if (prec <= 0)
				error("Bad precision value", "");
			break;
		default:
			error("Unknown option %s", *av);
		}
	switch (ac) {	/* examine args right to left, falling thru cases */
	case 4:
		if (!isdefault(av[3])) {
			if (!sscanf(av[3], "%F", &s))
				error("Bad s value:  %s", av[3]);
			mask |= 01;
		}
	case 3:
		if (!isdefault(av[2])) {
			if (!sscanf(av[2], "%F", &ender))
				ender = av[2][strlen(av[2])-1];
			mask |= 02;
			if (!prec)
				n = getprec(av[2]);
		}
	case 2:
		if (!isdefault(av[1])) {
			if (!sscanf(av[1], "%F", &begin))
				begin = av[1][strlen(av[1])-1];
			mask |= 04;
			if (!prec)
				prec = getprec(av[1]);
			if (n > prec)		/* maximum precision */
				prec = n;
		}
	case 1:
		if (!isdefault(av[0])) {
			if (!sscanf(av[0], "%D", &reps))
				error("Bad reps value:  %s", av[0]);
			mask |= 010;
		}
		break;
	case 0:
		error("jot - print sequential or random data", "");
	default:
		error("Too many arguments.  What do you mean by %s?", av[4]);
	}
	getformat();
	while (mask)	/* 4 bit mask has 1's where last 4 args were given */
		switch (mask) {	/* fill in the 0's by default or computation */
		CASE 001:
			reps = REPS_DEF;
			mask = 011;
		CASE 002:
			reps = REPS_DEF;
			mask = 012;
		CASE 003:
			reps = REPS_DEF;
			mask = 013;
		CASE 004:
			reps = REPS_DEF;
			mask = 014;
		CASE 005:
			reps = REPS_DEF;
			mask = 015;
		CASE 006:
			reps = REPS_DEF;
			mask = 016;
		CASE 007:
			if (randomize) {
				reps = REPS_DEF;
				mask = 0;
				break;
			}
			if (s == 0.0) {
				reps = 0;
				mask = 0;
				break;
			}
			reps = (ender - begin + s) / s;
			if (reps <= 0)
				error("Impossible stepsize", "");
			mask = 0;
		CASE 010:
			begin = BEGIN_DEF;
			mask = 014;
		CASE 011:
			begin = BEGIN_DEF;
			mask = 015;
		CASE 012:
			s = (randomize ? time(0) : STEP_DEF);
			mask = 013;
		CASE 013:
			if (randomize)
				begin = BEGIN_DEF;
			else if (reps == 0)
				error("Must specify begin if reps == 0", "");
			begin = ender - reps * s + s;
			mask = 0;
		CASE 014:
			s = (randomize ? time(0) : STEP_DEF);
			mask = 015;
		CASE 015:
			if (randomize)
				ender = ENDER_DEF;
			else
				ender = begin + reps * s - s;
			mask = 0;
		CASE 016:
			if (randomize)
				s = time(0);
			else if (reps == 0)
				error("Infinite sequences cannot be bounded", "");
			else if (reps == 1)
				s = 0.0;
			else
				s = (ender - begin) / (reps - 1);
			mask = 0;
		CASE 017:
			if (!randomize && s != 0.0) {	/* if reps given and implied, */
				long t = (ender - begin + s) / s;
				if (t <= 0)
					error("Impossible stepsize", "");
				if (t < reps)		/* take lesser */
					reps = t;
			}
			mask = 0;
			break;
		default:
			error("Bad mask", "");
		}
	if (reps == 0)
		infinity = 1;
}

putdata(x, notlast)
double	x;
long	notlast;
{
	long		d = x;
	register long	*dp = &d;

	if (boring)				/* repeated word */
		printf(format);
	else if (dox)				/* scalar */
		printf(format, *dp);
	else					/* real */
		printf(format, x);
	if (notlast != 0)
		fputs(sepstring, stdout);
}

error(msg, s)
char	*msg;
char	*s;
{
	char	buf[BUFSIZ];

	setbuf(stderr, buf);
	fprintf(stderr, "jot: ");
	fprintf(stderr, msg, s);
	fprintf(stderr, "\nUsage:  jot [ options ] [ reps [ begin [ end [ s ] ] ] ]\n");
	if (strncmp("jot - ", msg, 6) == 0)
		fprintf(stderr, "Options:\n\t%s\t%s\t%s\t%s\t%s\t%s",
			"-r		random data\n",
			"-c		character data\n",
			"-n		no final newline\n",
			"-b word		repeated word\n",
			"-w word		context word\n",
			"-s string	data separator\n",
			"-p precision	number of characters\n");
	exit(1);
}

getprec(s)
char	*s;
{
	register char	*p;
	register char	*q;

	for (p = s; *p; p++)
		if (*p == '.')
			break;
	if (!*p)
		return(0);
	for (q = ++p; *p; p++)
		if (!isdigit(*p))
			break;
	return(p - q);
}

getformat()
{
	register char	*p;

	if (boring)				/* no need to bother */
		return;
	for (p = format; *p; p++)		/* look for '%' */
		if (*p == '%' && *(p+1) != '%')	/* leave %% alone */
			break;
	if (!*p && !chardata)
		sprintf(p, "%%.%df", prec);
	else if (!*p && chardata) {
		strcpy(p, "%c");
		dox = 1;
	}
	else if (!*(p+1))
		strcat(format, "%");		/* cannot end in single '%' */
	else {
		while (!isalpha(*p))
			p++;
		switch (*p) {
		case 'f': case 'e': case 'g': case '%':
			break;
		case 's':
			error("Cannot convert numeric data to strings", "");
			break;
		/* case 'd': case 'o': case 'x': case 'D': case 'O': case 'X':
		case 'c': case 'u': */
		default:
			dox = 1;
			break;
		}
	}
}
