/****************************************************************/
/*								*/
/*								*/
/* Copyright (C) 1981, Regents of the University of California	*/
/*	All rights reserved					*/
/*								*/
/****************************************************************/
/*  VPLTDMP: version 4.1			updated %G%
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

static	char *Sid = "@(#)vpltdmp.c	4.1\t%G%";

int	plotmd[] = { VPLOT, 0, 0 };
int	prtmd[]  = { VPRINT, 0, 0 };

char	buf[BUFSIZ];		/* output buffer */

int	lines;			/* number of raster lines printed */
int	varian = 1;		/* default is the varian */
int	BytesPerLine = 264;	/* Number of bytes per raster line */

char	*name, *host, *acctfile;

main(argc, argv)
char *argv[];
{
	register int n, bytes;

	if (argv[0][strlen(argv[0])-1] == 'W') {
		varian = 0;
		BytesPerLine = 880;
	}

	while (--argc) {
		if (**++argv == '-') {
			switch (argv[0][1]) {
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
	lines = bytes / BytesPerLine;

	ioctl(OUT, VSETSTATE, prtmd);
	if (varian)
		write(OUT, "\014", 2);
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
	 * Varian accounting is done by 11 inch pages;
	 * Versatec accounting is by the (12 inch) foot.
	 */
	fprintf(a, "t%6.2f\t", (lines / 200.0) / (varian ? 11.0 : 12.0));
	if (from != NULL)
		fprintf(a, "%s:", from);
	fprintf(a, "%s\n", who);
	fclose(a);
}
