/* Copyright (c) 1983 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)lam.c	4.5	(Berkeley)	5/19/88";
#endif not lint

/*
 *	lam - laminate files
 *	Author:  John Kunze, UCB
 */

#include <stdio.h>

#define	MAXOFILES	20
#define	BIGBUFSIZ	5 * BUFSIZ

struct	openfile {		/* open file structure */
	FILE	*fp;		/* file pointer */
	short	eof;		/* eof flag */
	short	pad;		/* pad flag for missing columns */
	char	eol;		/* end of line character */
	char	*sepstring;	/* string to print before each line */
	char	*format;	/* printf(3) style string spec. */
}	input[MAXOFILES];

int	morefiles;		/* set by getargs(), changed by gatherline() */
int	nofinalnl;		/* normally append \n to each output line */
char	buf[BUFSIZ];
char	line[BIGBUFSIZ];
char	*linep;

main(argc, argv)
int	argc;
char	**argv;
{
	register struct	openfile	*ip;
	char	*gatherline();

	setbuf(stdout, buf);
	getargs(argv);
	if (!morefiles)
		error("lam - laminate files", "");
	for (;;) {
		linep = line;
		for (ip = input; ip->fp != NULL; ip++)
			linep = gatherline(ip);
		if (!morefiles)
			exit(0);
		fputs(line, stdout);
		fputs(ip->sepstring, stdout);
		if (!nofinalnl)
			putchar('\n');
	}
}

getargs(av)
char	**av;
{
	register struct	openfile	*ip = input;
	register char	*p;
	register char	*c;
	static	char	fmtbuf[BUFSIZ];
	char	*fmtp = fmtbuf;
	int	P, S, F, T;

	P = S = F = T = 0;		/* capitalized options */
	while (p = *++av) {
		if (*p != '-' || !p[1]) {
			morefiles++;
			if (*p == '-')
				ip->fp = stdin;
			else if ((ip->fp = fopen(p, "r")) == NULL) {
				perror(p);
				exit(1);
			}
			ip->pad = P;
			if (!ip->sepstring)
				ip->sepstring = (S ? (ip-1)->sepstring : "");
			if (!ip->format)
				ip->format = ((P || F) ? (ip-1)->format : "%s");
			if (!ip->eol)
				ip->eol = (T ? (ip-1)->eol : '\n');
			ip++;
			continue;
		}
		switch (*(c = ++p) | 040) {
		case 's':
			if (*++p || (p = *++av))
				ip->sepstring = p;
			else
				error("Need string after -%s", c);
			S = (*c == 'S' ? 1 : 0);
			break;
		case 't':
			if (*++p || (p = *++av))
				ip->eol = *p;
			else
				error("Need character after -%s", c);
			T = (*c == 'T' ? 1 : 0);
			nofinalnl = 1;
			break;
		case 'p':
			ip->pad = 1;
			P = (*c == 'P' ? 1 : 0);
		case 'f':
			F = (*c == 'F' ? 1 : 0);
			if (*++p || (p = *++av)) {
				fmtp += strlen(fmtp) + 1;
				if (fmtp > fmtbuf + BUFSIZ)
					error("No more format space", "");
				sprintf(fmtp, "%%%ss", p);
				ip->format = fmtp;
			}
			else
				error("Need string after -%s", c);
			break;
		default:
			error("What do you mean by -%s?", c);
			break;
		}
	}
	ip->fp = NULL;
	if (!ip->sepstring)
		ip->sepstring = "";
}

char	*
pad(ip)
struct	openfile	*ip;
{
	register char	*p = ip->sepstring;
	register char	*lp = linep;

	while (*p)
		*lp++ = *p++;
	if (ip->pad) {
		sprintf(lp, ip->format, "");
		lp += strlen(lp);
	}
	return(lp);
}

char	*
gatherline(ip)
struct	openfile	*ip;
{
	char	s[BUFSIZ];
	register int	c;
	register char	*p;
	register char	*lp = linep;
	char	*end = s + BUFSIZ;

	if (ip->eof)
		return(pad(ip));
	for (p = s; (c = fgetc(ip->fp)) != EOF && p < end; p++)
		if ((*p = c) == ip->eol)
			break;
	*p = '\0';
	if (c == EOF) {
		ip->eof = 1;
		if (ip->fp == stdin)
			fclose(stdin);
		morefiles--;
		return(pad(ip));
	}
	p = ip->sepstring;
	while (*p)
		*lp++ = *p++;
	sprintf(lp, ip->format, s);
	lp += strlen(lp);
	return(lp);
}

error(msg, s)
char	*msg;
char	*s;
{
	char	buf[BUFSIZ];

	setbuf(stderr, buf);
	fprintf(stderr, "lam: ");
	fprintf(stderr, msg, s);
	fprintf(stderr, "\nUsage:  lam [ -[fp] min.max ] [ -s sepstring ] [ -t c ] file ...\n");
	if (strncmp("lam - ", msg, 6) == 0)
		fprintf(stderr, "Options:\n\t%s\t%s\t%s\t%s\t%s",
			"-f min.max	field widths for file fragments\n",
			"-p min.max	like -f, but pad missing fragments\n",
			"-s sepstring	fragment separator\n",
			"-t c		input line terminator is c, no \\n after output lines\n",
			"Capitalized options affect more than one file.\n");
	exit(1);
}
