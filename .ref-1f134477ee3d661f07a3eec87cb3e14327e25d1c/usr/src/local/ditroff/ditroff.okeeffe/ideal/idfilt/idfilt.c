#ifndef lint
static char *sccsid = "idfilt.c	(CWI)	1.1	85/03/01";
#endif
#include "idfilt.h"

FILE *infile;
char *filename;
int lineno = 0;

float maxx, maxy;
float minx, miny;
float width = 4.0;
float colwid = 6.0;

boolean maxxset, maxyset;
boolean minxset, minyset;
boolean widset, colset;
boolean boundset;

boolean veryfirst = TRUE;

boolean wantquality = FALSE;

main (argc, argv)
int argc;
char *argv[];
{
	while (argc > 1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
		case 'q':
			wantquality = TRUE;
			break;
		default:
			fprintf (stderr, "ideal filter: unknown flag %c\n", argv[1][1]);
			break;
		}
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
				fprintf (stderr, "ideal filter: can't open %s\n", *argv);
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


	while (fgets (buf, sizeof buf, infile)) {
		lineno++;
		idjusttext (buf);
		if (buf[0] == '.') {
			if (buf[1] == 'I') {
				switch (buf[2]) {
				case 'S':
					numitems = sscanf (buf, "%s %d %d %d %d %d %d",
						cmd, &i[0], &i[1], &i[2], &i[3], &i[4], &i[5]
					);
					idstart (numitems, i);
					maxxset = minxset = FALSE;
					maxyset = minyset = FALSE;
					colset = boundset = widset = FALSE;
					break;
				case 'E':
					idendE ();
					break;
				case 'F':
					idendF ();
					break;
				default:
					break;
				}
			} else if (buf[1] == '.' && buf[2] == '.') {
				sscanf (buf, "%s", cmd);
				if (!boundset) {
					if (strcmp (cmd, "...maxx") == 0) {
						sscanf (buf, "%s %f",
							cmd, &f[0]
						);
						idmaxx (f[0]);
					} else if (strcmp (cmd, "...maxy") == 0) {
						sscanf (buf, "%s %f",
							cmd, &f[0]
						);
						idmaxy (f[0]);
					} else if (strcmp (cmd, "...minx") == 0) {
						sscanf (buf, "%s %f",
							cmd, &f[0]
						);
						idminx (f[0]);
					} else if (strcmp (cmd, "...miny") == 0) {
						sscanf (buf, "%s %f",
							cmd, &f[0]
						);
						idminy (f[0]);
					} else if (strcmp (cmd, "...width") == 0) {
						sscanf (buf, "%s %f",
							cmd, &f[0]
						);
						idwidth (f[0]);
					} else if (strcmp (cmd, "...colwid") == 0) {
						sscanf (buf, "%s %f",
							cmd, &f[0]
						);
						idcolwid (f[0]);
					} else if (strcmp (cmd, "...obbox") == 0) {
						if (!veryfirst) {
							maxxset = maxyset = TRUE;
							minxset = minyset = TRUE;
							boundset = TRUE;
						}
					} else if (strcmp (cmd, "...noerase") == 0) {
						idnoerase ();
					} else if (strcmp (cmd, "...yeserase") == 0) {
						idyeserase ();
					} else {
						idendbound ();
						veryfirst = FALSE;
					}
				}
				if (boundset) {
					if (strcmp (cmd, "...line") == 0) {
						sscanf (buf, "%s %f %f %f %f",
							cmd, &f[0], &f[1], &f[2], &f[3]
						);
						idline (f[0], f[1], f[2], f[3]);
					} else if (strcmp (cmd, "...circle") == 0) {
						sscanf (buf, "%s %f %f %f",
							cmd, &f[0], &f[1], &f[2]
						);
						idcircle (f[0], f[1], f[2]);
					} else if (strcmp (cmd, "...arc") == 0) {
						sscanf (buf, "%s %f %f %f %f %f %f %f %f %f",
							cmd, &f[0], &f[1], &f[2], &f[3], &f[4], &f[5], &f[6], &f[7], &f[8]
						);
						idarc (f[0], f[1], f[2], f[3], f[4], f[5], f[6], f[7], f[8]);
					} else if (strcmp (cmd, "...left") == 0) {
						sscanf (buf, "%s %f %f",
							cmd, &f[0], &f[1]
						);
						buf[strlen(buf)-1] = '\0';
						string = buf;
						while (*string != '\'')
							string ++;
						idleft (f[0], f[1], string);
					} else if (strcmp (cmd, "...center") == 0) {
						sscanf (buf, "%s %f %f",
							cmd, &f[0], &f[1]
						);
						buf[strlen(buf)-1] = '\0';
						string = buf;
						while (*string != '\'')
							string ++;
						idcenter (f[0], f[1], string);
					} else if (strcmp (cmd, "...right") == 0) {
						sscanf (buf, "%s %f %f",
							cmd, &f[0], &f[1]
						);
						buf[strlen(buf)-1] = '\0';
						string = buf;
						while (*string != '\'')
							string ++;
						idright (f[0], f[1], string);
					} else if (strcmp (cmd, "...spline") == 0) {
						sscanf (buf, "%s %f %f",
							cmd, &f[0], &f[1]
						);
						idspline (f[0], f[1]);
					} else if (strcmp (cmd, "...knot") == 0) {
						sscanf (buf, "%s %f %f",
							cmd, &f[0], &f[1]
						);
						idknot (f[0], f[1]);
					} else if (strcmp (cmd, "...endspline") == 0) {
						idendspline ();
					}
				}
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
