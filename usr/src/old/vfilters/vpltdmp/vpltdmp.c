/****************************************************************/
/*								*/
/*								*/
/* Copyright (C) 1981, Regents of the University of California	*/
/*	All rights reserved					*/
/*								*/
/****************************************************************/
/*  VPLTDMP: version 4.3			updated %G%
 *
 *  reads raster file created by vplot and dumps it onto the
 *  Varian or Versatec plotter.
 *  Input comes from file descriptor 0, output is to file descriptor 1.
 */
#include <stdio.h>
#include <signal.h>
#include <sys/vcmd.h>

#define IN	0
#define OUT	1

static	char *Sid = "@(#)vpltdmp.c	4.3\t%G%";

int	plotmd[] = { VPLOT };
int	prtmd[]  = { VPRINT };

char	buf[BUFSIZ];		/* output buffer */

int	lines;			/* number of raster lines printed */
int	varian;			/* 0 for versatec, 1 for varian. */
int	BYTES_PER_LINE;		/* number of bytes per raster line. */
int	PAGE_LINES;		/* number of raster lines per page. */

char	*name, *host, *acctfile;

main(argc, argv)
char *argv[];
{
	register int n, bytes;

	while (--argc) {
		if (**++argv == '-') {
			switch (argv[0][1]) {
			case 'x':
				BYTES_PER_LINE = atoi(&argv[0][2]) / 8;
				varian = BYTES_PER_LINE == 264;
				break;

			case 'y':
				PAGE_LINES = atoi(&argv[0][2]);
				break;

			case 'n':
				argc--;
				name = *++argv;
				break;

			case 'h':
				argc--;
				host = *++argv;
			}
		} else
			acctfile = *argv;
	}

	ioctl(OUT, VSETSTATE, plotmd);

	bytes = 0;
	while ((n = read(IN, buf, sizeof(buf))) > 0) {
		if (write(OUT, buf, n) != n)
			exit(1);
		bytes += n;
	}
	if (bytes & 1) {	/* make sure even number bytes are sent */
		write(OUT, "", 1);
		bytes++;
	}
	lines = bytes / BYTES_PER_LINE;

	ioctl(OUT, VSETSTATE, prtmd);
	if (varian)
		write(OUT, "\f", 2);
	else
		write(OUT, "\n\n\n\n\n", 6);
	account(name, host, *argv);
	exit(0);
}

account(who, from, acctfile)
	char *who, *from, *acctfile;
{
	register FILE *a;

	if (who == NULL || acctfile == NULL)
		return;
	if (access(acctfile, 02) || (a = fopen(acctfile, "a")) == NULL)
		return;
	/*
	 * Varian accounting is done by 8.5 inch pages;
	 * Versatec accounting is by the (12 inch) foot.
	 */
	fprintf(a, "t%6.2f\t", (lines / 200.0) / PAGE_LINES);
	if (from != NULL)
		fprintf(a, "%s:", from);
	fprintf(a, "%s\n", who);
	fclose(a);
}
