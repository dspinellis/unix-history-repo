/*
 *
 * dver: VAX Versatec driver for the new troff
 *
 * Authors:	BWK(BELL)
 *		VCAT(berkley)
 *		Richard L. Hyde, Perdue University
 *		and David Slattengren, U.C. Berkeley
 *
 *		Jaap Akkerhuis
 *			added Versatec80 support
 *			removed Berkeley specific stuff (like nstips)
 *			by #ifdef BERK, allthough a lot of things are
 *			around (like polygon and gremlin...)
 *
 *		Carol Orange and Denise Draper
 *			Changed to make the versatec a Harris typesetter
 *			look-alike.
 */
#ifndef LINT
static char sccsid[] = "@(#)ndver80	1.2 (CWI) 87/07/10";
#endif
/*******************************************************************************

    output language from troff:
    all numbers are character strings

#..\n	comment
sn	size in points
fn	font as number from 1 to n
cx	ascii character x
Cxyz	funny char \(xyz. terminated by white space
Hn	go to absolute horizontal position n
Vn	go to absolute vertical position n (down is positive)
hn	go n units horizontally (relative)
vn	ditto vertically
nnc	move right nn, then print c (exactly 2 digits!)
		(this wart is an optimization that shrinks output file size
		 about 35% and run-time about 15% while preserving ascii-ness)
pn	new page begins (number n) -- set v to 0
P	spread ends -- output it. (Put in by vsort).
nb a	end of line (information only -- no action needed)
	b = space before line, a = after
w	paddable word space -- no action needed

Dt ..\n	draw operation 't':
	Dl x y .	line from here by x,y (drawing char .)
	Dc d		circle of diameter d with left side here
	De x y		ellipse of axes x,y with left side here
	Da x y r	arc counter-clockwise by x,y of radius r
	D~ x y x y ...	B-spline curve by x,y then x,y ...
			vectors, with extents from miny to maxy (no border)

x ..\n	device control functions:
     x i	init
     x T s	name of device is s
     x r n h v	resolution is n/inch h = min horizontal motion, v = min vert
     x p	pause (can restart)
     x s	stop -- done for ever
     x t	generate trailer
     x f n s	font position n contains font s
     x H n	set character height to n
     x S n	set slant to N

	Subcommands like "i" are often spelled out like "init".

*******************************************************************************/

#include <sys/vcmd.h>
#include "the.h"

public char	*devname = "har";
public char	*fontdir = "/usr/local/lib/ditroff/font";

public int	debug = 0;		/* two different debugging modes	*/
public int	dbg = 0;

extern int	virtRES;
					/* externs: set here, used elsewhere	*/
extern int	nolist, olist[];	
extern int	pltmode[], prtmode[];

char *operand();



main(argc, argv)
char *argv[];
{
	register FILE *fp;
	char *dummy;	

	while (--argc > 0 && **++argv == '-') {
		switch ((*argv)[1]) {
		case 'F':	
			dummy = operand(&argc, &argv);		/* ignore */
			break;
		case 'D': 
			debug = 1;
			break;
		case 'f':
			fontdir = operand(&argc, &argv);
			break;
		case 'o':
			outlist(operand(&argc, &argv));
			break;
#ifdef DEBUGABLE
		case 'd':
			dbg = atoi(operand(&argc, &argv));
			if (dbg == 0) dbg = 1;
			break;
#endif
		case 'n':
			dummy = operand(&argc, &argv);		/* ignore */
			break;

		case 'h':
			dummy = operand(&argc, &argv);		/* ignore */
			break;
		case 'x':
		case 'y':
			dummy = operand(&argc, &argv);		/* ignore */
			break;

		}
	}
	/* ignore accounting argument... */
	argc--;
	argv++;

#ifdef DRIVER
	ioctl(OUTFILE, VSETSTATE, pltmode);
#endif

	initfonts();
	initgraph(virtRES);

	if (argc < 1)
		conv(stdin);
	else
		while (argc-- > 0) {
			if (strcmp(*argv, "-") == 0)
				fp = stdin;
			else if ((fp = fopen(*argv, "r")) == NULL)
				error(FATAL, "can't open %s", *argv);
			conv(fp);
			fclose(fp);
			argv++;
		}
	exit(0);
}


/*----------------------------------------------------------------------------*
 | Routine:	char  * operand (& argc,  & argv)
 |
 | Results:	returns address of the operand given with a command-line
 |		option.  It uses either "-Xoperand" or "-X operand", whichever
 |		is present.  The program is terminated if no option is present.
 |
 | Side Efct:	argc and argv are updated as necessary.
 *----------------------------------------------------------------------------*/

char *operand(argcp, argvp)
int * argcp;
char ***argvp;
{
	if ((**argvp)[2]) return(**argvp + 2); /* operand immediately follows */
	if ((--*argcp) <= 0) {			/* no operand */
	    error (FATAL, "command-line option operand missing.");
	}
	return(*(++(*argvp)));			/* operand next word */
}


outlist(s)	/* process list of page numbers to be printed */
char *s;
{
	int n1, n2;
#ifdef DEBUGABLE
	int i;
#endif

	nolist = 0;
	while (*s) {
		n1 = 0;
		if (isdigit(*s))
			do
				n1 = 10 * n1 + *s++ - '0';
			while (isdigit(*s));
		else
			n1 = -9999;
		n2 = n1;
		if (*s == '-') {
			s++;
			n2 = 0;
			if (isdigit(*s))
				do
					n2 = 10 * n2 + *s++ - '0';
				while (isdigit(*s));
			else
				n2 = 9999;
		}
		olist[nolist++] = n1;
		olist[nolist++] = n2;
		if (*s != '\0')
			s++;
	}
	olist[nolist] = 0;
#ifdef DEBUGABLE
	if (dbg)
		for (i=0; i<nolist; i += 2)
			fprintf(stderr,"%3d %3d\n", olist[i], olist[i+1]);
#endif
}



/*VARARGS1*/

error(f, s, a1, a2, a3, a4, a5, a6, a7)
{
	fprintf(stderr, "dver: ");
	/*NOSTRICT*/
	fprintf(stderr, s, a1, a2, a3, a4, a5, a6, a7);
	fprintf(stderr, "\n");
	if (f) exit(ABORT);
}




/*VARARGS*/
/*NOSTRICT*/

eprintf(s1, s2, s3, s4, s5, s6, s7, s8)
{
	/*NOSTRICT*/
	fprintf(stderr, s1, s2, s3, s4, s5, s6, s7, s8);
}
