#ifndef lint
static char *sccsid ="ideal.c	(CWI)	1.1	85/03/01";
#endif
#include "ideal.h"
#include "y.tab.h"

boolean dbg = FALSE;
int when_bug = 0;
char *filename;
int lineno = 0;
char libstr[50];
boolean radflag = FALSE;

BOXPTR boxlist = NULL;

main(argc, argv)
int argc;
char *argv[];
{
	bug_off;
	while (argc > 1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
		case 'd':
			when_bug = argv[1][2]?atoi(&argv[1][2]):~0;
			break;
		case 'l':
			idinclude (&argv[1][2], LIBFIL);
			while (yyparse());
			break;
		case 'r':
			radflag = TRUE;
			break;
		case 'p':
		case '4':
		case 'n':
			break;
		default:
			fprintf(stderr, "ideal: unknown flag %c\n", argv[1][1]);
			break;
		}
		argc--;
		argv++;
	}
	if (argc < 2) {
		filename = "standard input";
		lineno = 0;
		filepush (stdin);
		while (yyparse ());
	} else
		while (argc-- > 1) {
			filename = *argv;
			lineno = 0;
			idinclude (*++argv, CHATTY);
			while (yyparse ());
		}
	exit(0);
}

interpret()
{
	PUTNODE dummyroot;
	if (when_bug & 01) bug_on;
	else bug_off;
	dummyroot.name = lookup("main");
	dummyroot.parm = boxgen (lookup("main"), (STMTPTR) NULL);
	/* if boxlist includes main, execute it */
	if (boxlist && findbox(lookup("main"),TRUE)->stmtlist) {
		NOADPTR noadtree;
		bug_off;
		/* make room for all variables */
		noadtree = buildnoadtree (&dummyroot);
		/* solve all equations */
		eqneval (noadtree);
		nl_eval ();
		depvarkill ();
		/* make a list of segments in the picture */
		noadtree->linelist = build (noadtree, noadtree->linelist);
		/* draw the thing */
		act (noadtree->linelist);
		/* free the thing, but save definitions */
		noadfree (noadtree);
		fflush (stdout);
		forget (lookup ("main"));
	}
}

idinclude (filnam, mode)
char *filnam;
int mode;
{
	FILE *nufile;
	dprintf "opening file %s\n", filnam);
	if (mode == CHATTY)
		nufile = fopen (filnam, "r");
	else if (mode == LIBFIL) {
		strcpy (libstr, LIBDIR);
		strcat (&libstr[0],filnam);
		filnam = libstr;
		nufile = fopen (filnam, "r");
	}
	filename = filnam;
	if (!nufile) {
		fprintf (stderr, "ideal: can't open file %s\n", filnam);
		exit (1);
	} else {
		filepush (nufile);
	}
}

act (the_picture)
LINEPTR the_picture;
{
	LINEPTR lineseg;
	float maxx, maxy, minx, miny;
	maxx = -10000.0;
	maxy = -10000.0;
	minx = 10000.0;
	miny = 10000.0;
	for (lineseg = the_picture; lineseg; lineseg = lineseg->next) {
		switch (lineseg->kind) {
		case LINE:
			maxx = max(maxx, max(lineseg->x0, lineseg->x1));
			maxy = max(maxy, max(lineseg->y0, lineseg->y1));
			minx = min(minx, min(lineseg->x0, lineseg->x1));
			miny = min(miny, min(lineseg->y0, lineseg->y1));
			break;
		case CIRCLE:
			maxx = max(maxx, ((CIRCPTR) lineseg)->x0 + fabs(((CIRCPTR) lineseg)->r));
			minx = min(minx, ((CIRCPTR) lineseg)->x0 - fabs(((CIRCPTR) lineseg)->r));
			maxy = max(maxy, ((CIRCPTR) lineseg)->y0 + fabs(((CIRCPTR) lineseg)->r));
			miny = min(miny, ((CIRCPTR) lineseg)->y0 - fabs(((CIRCPTR) lineseg)->r));
			break;
		case ARC:
			maxx = max(maxx, ((ARCPTR) lineseg)->x0 + fabs(((ARCPTR) lineseg)->radius));
			minx = min(minx, ((ARCPTR) lineseg)->x0 - fabs(((ARCPTR) lineseg)->radius));
			maxy = max(maxy, ((ARCPTR) lineseg)->y0 + fabs(((ARCPTR) lineseg)->radius));
			miny = min(miny, ((ARCPTR) lineseg)->y0 - fabs(((ARCPTR) lineseg)->radius));
			break;
		case STRING:
			maxx = max(maxx, ((TEXTPTR) lineseg)->x0);
			minx = min(minx, ((TEXTPTR) lineseg)->x0);
			maxy = max(maxy, ((TEXTPTR) lineseg)->y0);
			miny = min(miny, ((TEXTPTR) lineseg)->y0);
			break;
		case SPLINE:
			break;
		default:
			fprintf (stderr, "ideal: act: can't happen\n");
			break;
		}
	}
	if (the_picture) {
		boundscall (maxx, maxy, minx, miny);
	}
	for (lineseg = the_picture; lineseg; lineseg = lineseg->next) {
		switch (lineseg->kind) {
		case LINE:
			linecall (lineseg);
			break;
		case CIRCLE:
			circcall ((CIRCPTR) lineseg);
			break;
		case ARC:
			arccall ((ARCPTR) lineseg);
			break;
		case STRING:
			textcall ((TEXTPTR) lineseg);
			break;
		case SPLINE:
			splcall (((SPLPTR) lineseg)->knotlist);
			break;
		default:
			fprintf (stderr, "ideal: act: can't happen\n");
			break;
		}
	}
}
