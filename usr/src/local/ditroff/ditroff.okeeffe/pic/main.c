#ifndef lint
static char sccsid[] = "@(#)main.c	1.1 (CWI) 85/07/19";
#endif lint

#include	<stdio.h>
#include	"pic.h"
#include	"y.tab.h"

struct	obj	*objlist[MAXOBJ];	/* store the elements here */
int	nobj	= 0;

struct attr	attr[40];	/* attributes stored here as collected */
int	nattr	= 0;	/* number of entries in attr_list */

struct	text	text[MAXTEXT];	/* text strings stored here as collected */
int	ntext	= 0;
int	ntext1	= 0;	/* record ntext here on entry to each figure */

float	curx	= 0;
float	cury	= 0;

int	hvmode	= R_DIR;	/* R => join left to right, D => top to bottom, etc. */

int	codegen	= 0;	/* 1=>output for this picture; 0=>no output */

float	deltx	= 6;	/* max x value in output, for scaling */
float	delty	= 6;	/* max y value in output, for scaling */
int	dbg	= 0;
extern	FILE	*yyin,	/* input file pointer */
		*skeldb; /* output pointer for dbg messeges */
int	lineno	= 0;
char	*filename	= "-";
int	synerr	= 0;
char	*cmdname;
int	crop	= 1;	/* trim off exterior white space if non-zero */
extern int	useDline;	/* if set, use \D for all lines */

/* You may want to change this if you don't have a 202... */

/*
 *#ifdef	APS
 *	int	devtype	= DEVAPS;
 *	int	res	= 723;
 *	int	DX	= 3;
 *	int	DY	= 3;
 *
 *#ifdef 202
 *	int	devtype	= DEV202;
 *	int	res	= 972;	/* default is 202 *
 *	int	DX	= 4;	/* used only for old-style troff *
 *	int	DY	= 4;
 */
#ifdef OLDTROFF
/* mandatory values for graphic systems CAT: */
int	devtype = DEVCAT;
int	res	= 432;
int	DX = 3;
int	DY = 3;
#else
	int	devtype	= DEVHAR;
	int	res	= 1445;	/* default is HARRIS */
	int	DX	= 4;	/* used only for old-style troff */
	int	DY	= 4;
#endif

float	hshift	= 0;	/* move this far left for text (in em's) */
float	vshift	= 0.2;	/* this far down */

float	sxmin;		/* lower limit from s command */
float	symin;
float	sxmax	= 4096;	/* upper */
float	symax	= 4096;

float	xmin	= 30000;	/* min values found in actual data */
float	ymin	= 30000;
float	xmax	= -30000;	/* max */
float	ymax	= -30000;

main(argc, argv)
	char **argv;
{
	cmdname = argv[0];
	while (argc > 1 && *argv[1] == '-') {
		switch (argv[1][1]) {
		case 'T':
			if (strcmp(&argv[1][2], "aps") == 0) {
				res = 723;
				devtype = DEVAPS;
				DX = DY = 1;
			} else if (strcmp(&argv[1][2], "cat") == 0) {
				res = 432;
				devtype = DEVCAT;
				DX = DY = 3;
			} else if (strcmp(&argv[1][2], "ver") == 0) {
				res = 200;
				devtype = DEVVER;
				DX = DY = 1;
			} else if (strcmp(&argv[1][2], "450") == 0) {
				res = 240;
				devtype = DEV450;
			} else if (strcmp(&argv[1][2], "har") == 0) {
				res = 1445;
				devtype = DEVHAR;
			} else {
				res = atoi(&argv[1][2]);
			}
			break;
		case 'd':
			dbg = 1;
			break;
		case 'D':
#			ifdef OLDTROFF
			useDline = !useDline;
#			endif
			break;
		}
		argc--;
		argv++;
	}
#	ifndef OLDTROFF
	useDline = 1;
#	endif
#
	setdefaults();
	if (argc <= 1) {
		yyin = stdin;
		getdata(yyin);
	} else
		while (argc-- > 1) {
			if ((yyin = fopen(*++argv, "r")) == NULL) {
				fprintf(stderr, "pic: can't open %s\n", *argv);
				exit(1);
			}
			filename = *argv;
			getdata(yyin);
			fclose(yyin);
		}
	exit(0);
}

static struct {
	char *name;
	float val;
} defaults[] ={
	"scale", SCALE,
	"lineht", HT,
	"linewid", HT,
	"moveht", HT,
	"movewid", HT,
	"dashwid", HT10,
	"boxht", HT,
	"boxwid", WID,
	"circlerad", HT2,
	"arcrad", HT2,
	"ellipseht", HT,
	"ellipsewid", WID,
	"arrowht", HT5,
	"arrowwid", HT10,
	"textht", HT,
	"textwid", WID,
	NULL, 0
};

setdefaults()	/* set default sizes for variables like boxht */
{
	int i;
	YYSTYPE v;

	for (i = 0; defaults[i].name != NULL; i++) {
		v.f = defaults[i].val;
		makevar(tostring(defaults[i].name), VARNAME, v);
	}
}


checkscale(s)	/* if s is "scale", adjust default variables */
	char *s;
{
	int i;
	float scale;

	if (strcmp(s, "scale") == 0) {
		scale = getfval("scale");
		for (i = 1; defaults[i].name != NULL; i++)
			setfval(defaults[i].name, defaults[i].val * scale);
	}
}

getdata(fin)
	register FILE *fin;
{
	char buf[1000], buf1[50];
	FILE *svyyin;
	int svlineno;
	char *svfilename, *p;

	lineno = 0;
	while (fgets(buf, sizeof buf, fin) != NULL) {
		lineno++;
		if (*buf == '.' && *(buf+1) == 'P' && *(buf+2) == 'S') {
			for (p = &buf[3]; *p == ' '; p++)
				;
			if (*p++ == '<') {
				svyyin = yyin;
				svlineno = lineno;
				svfilename = filename;
				sscanf(p, "%s", buf1);
				if ((yyin = fopen(buf1, "r")) == NULL) {
					fprintf(stderr, "pic: can't open %s\n", buf1);
					exit(1);
				}
				lineno = 0;
				filename = p;
				getdata(yyin);
				fclose(yyin);
				lineno = svlineno;
				yyin = svyyin;
				filename = svfilename;
				continue;
			}
			reset();
			yyparse();
			/* yylval now contains 'E' or 'F' from .PE or .PF */
			if (buf[3] == ' ')	/* assume next thing is width */
				deltx = delty = atof(&buf[4]);
			else {
				deltx = xmax - xmin;
				if (deltx <= 0)
					deltx = ymax - ymin;
				deltx /= getfval("scale");
				delty = deltx;
			}
			dprintf("deltx = %.3f\n", deltx);
			if (codegen && !synerr) {
				openpl(&buf[3]);	/* puts out .PS, with ht & wid stuck in */
				print();	/* assumes \n at end */
				closepl(yylval.p);	/* does the .PE/F */
			}
			fflush(stdout);
		}
		else
			fputs(buf, stdout);
	}
}

reset()
{
	struct obj *op;
	int i;
	struct symtab *p;
	extern int nstack;

	for (i = 0; i < nobj; i++) {
		op = objlist[i];
		if (op->o_type == BLOCK)
			freesymtab(op->o_dotdash);	/* funny place */
		free(objlist[i]);
	}
	nobj = 0;
	nattr = 0;
	for (i = 0; i < ntext; i++)
		free(text[i].t_val);
	ntext = ntext1 = 0;
	codegen = synerr = 0;
	nstack = 0;
	curx = cury = 0;
	hvmode = R_DIR;
	sxmin = symin = 0;
	sxmax = symax = 4096;
	xmin = ymin = 30000;
	xmax = ymax = -30000;
}
