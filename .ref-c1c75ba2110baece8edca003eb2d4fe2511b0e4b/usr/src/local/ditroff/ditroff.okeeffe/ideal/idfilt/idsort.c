#ifndef lint
static char *sccsid = "idsort.c	(CWI)	1.1	85/03/01";
#endif
#include "idfilt.h"

FILE *infile;
FILE *tempfile;
char *filename;
int lineno = 0;

float maxx, maxy;
float minx, miny;
float width = 4.0;
float colwid = 6.0;
float xscale;

boolean maxxset, maxyset;
boolean minxset, minyset;
boolean widset, colset;
boolean boundset;

boolean veryfirst = TRUE;

main (argc, argv)
int argc;
char *argv[];
{
	while (argc > 1 && argv[1][0] == '-') {
		fprintf (stderr, "ideal filter: unknown flag %c\n", argv[1][1]);
		argc--;
		argv++;
	}
	if (argc < 2) {
		infile = stdin;
		lineno = 0;
		interpret (infile);
	} else {
		while (argc-- > 1) {
			if (!(infile = fopen (*++argv, "r"))) {
				fprintf (stderr, "ideal sorter: can't open %s\n", *argv);
				exit (1);
			}
			filename = *argv;
			lineno = 0;
			interpret (infile);
			fclose (infile);
		}
	}
	exit (0);
}

interpret (infile)
register FILE *infile;
{
	char buf[250];

	int numitems;
	char cmd[10];
	int i[10];
	float f[30];
	char *string;
	boolean indots;


	indots = FALSE;
	while (fgets (buf, sizeof buf, infile)) {
		if (!indots) {
			fputs (buf, stdout);
			if (strncmp(buf,".IS",3) == 0) {
				indots = TRUE;
				maxxset = minxset = maxyset = minyset = FALSE;
				colset = boundset = widset = FALSE;
				boundset = FALSE;
				tempfile = fopen ("jUnKfOo", "w");
			}
		} else {
			if (strncmp(buf,"...line",7)) {
				if (!strncmp(buf,".IE",3) || !strncmp(buf,".IF",3)) {
					idendE ();
					indots = FALSE;
				}
				fputs (buf, stdout);
				if (!boundset) {
					if (strcmp (cmd, "...maxx") == 0) {
						sscanf (buf, "%s %f", cmd, &f[0]);
						idmaxx (f[0]);
					} else if (strcmp (cmd, "...maxy") == 0) {
						sscanf (buf, "%s %f", cmd, &f[0]);
						idmaxy (f[0]);
					} else if (strcmp (cmd, "...minx") == 0) {
						sscanf (buf, "%s %f", cmd, &f[0]);
						idminx (f[0]);
					} else if (strcmp (cmd, "...miny") == 0) {
						sscanf (buf, "%s %f", cmd, &f[0]);
						idminy (f[0]);
					} else if (strcmp (cmd, "...width") == 0) {
						sscanf (buf, "%s %f", cmd, &f[0]);
						idwidth (f[0]);
					} else if (strcmp (cmd, "...colwid") == 0) {
						sscanf (buf, "%s %f", cmd, &f[0]);
						idcolwid (f[0]);
					} else if (strcmp (cmd, "...obbox") == 0) {
						if (!veryfirst) {
							maxxset = maxyset = TRUE;
							minxset = minyset = TRUE;
							boundset = TRUE;
						}
					} else {
						idendbound ();
						veryfirst = FALSE;
					}
				}
			} else {
				if (!boundset)
					idendbound();
				sscanf (buf, "%s %f %f %f %f", cmd, &f[0], &f[1], &f[2], &f[3]);
				idline (f[0], f[1], f[2], f[3]);
			}
		}
	}
}

void idmaxx (x)
float x;
{
	if (!maxxset) {
		maxx = x;
		maxxset = TRUE;
	}
}

void idmaxy (y)
float y;
{
	if (!maxyset) {
		maxy = y;
		maxyset = TRUE;
	}
}

void idminx (x)
float x;
{
	if (!minxset) {
		minx = x;
		minxset = TRUE;
	}
}

void idminy (y)
float y;
{
	if (!minyset) {
		miny = y;
		minyset = TRUE;
	}
}

void idwidth (wid)
float wid;
{
	if (!widset) {
		width = wid;
		widset = TRUE;
	}
}

void idcolwid (wid)
float wid;
{
	if (!colset) {
		colwid = wid;
		colset = TRUE;
	}
}

void idendbound ()
{
	if (boundset)
		return;
	idminx (-6.0);
	idmaxy (6.0);
	idmaxx (6.0);
	idminy (-6.0);
	if (maxx - minx < 0.2) {
		maxx += 1;
		minx -= 1;
	}
	if (maxy - miny < 0.2) {
		maxy += 1;
		miny -= 1;
	}
	xscale = width*972.0/(maxx - minx);
	boundset = TRUE;
}

void idendE ()
{
	char c;
	fclose (tempfile);
	system ("sort +2 -r -o jUnKfOo jUnKfOo");
	tempfile = fopen ("jUnKfOo", "r");
	while ((c = getc(tempfile)) != EOF)
		putchar(c);
	fclose (tempfile);
	system ("rm jUnKfOo");
}


void idline (x1, y1, x2, y2)
float x1, y1, x2, y2;
{
	double t;
	int numsegs, i;
	if (y1 < y2 ) {
		t = x1;
		x1 = x2;
		x2 = t;
		t = y1;
		y1 = y2;
		y2 = t;
	}
	numsegs = xscale*abs(y2-y1)/250;
	if (numsegs <= 0) numsegs = 1;
	for (i = 0; i < numsegs; i ++)
		fprintf (tempfile, "...line %f %f %f %f\n",
			x1 + i*(x2-x1)/numsegs,
			y1 + i*(y2-y1)/numsegs,
			x1 + (i+1)*(x2-x1)/numsegs,
			y1 + (i+1)*(y2-y1)/numsegs
		);
}
