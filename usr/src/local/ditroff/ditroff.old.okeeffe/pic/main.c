/* main.c	(Berkeley)	1.9	86/03/11	*/
#include	<stdio.h>
#include	"pic.h"
#include	"y.tab.h"
#include	"dev.h"

#ifndef DEVDIR
#define DEVDIR	"/usr/lib/font"		/* place to look up device files */
#endif

char	*devdir = DEVDIR;
char	*dev = "va";			/* default typesetter is varian */
char	*getenv();

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

float	deltx	= 6.0;	/* max x value in output, for scaling */
float	delty	= 6.0;	/* max y value in output, for scaling */
float	xbound	= 8.0;	/* max allowed x value in output */
float	ybound	= 10.0;	/* max allowed y value in output */
int	dbg	= 0;
extern	FILE	*yyin,	/* input file pointer */
		*skeldb; /* output pointer for dbg messeges */
int	lineno	= 0;
char	*filename	= "-";
int	synerr	= 0;
char	*cmdname;
int	crop	= 1;	/* trim off exterior white space if non-zero */
extern int	useDline;	/* if set, use \D for all lines */
int	res;		/* resolution of output device (dots/inch) */
int	DX;		/* smallest change in X, and Y for output device */
int	DY;


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
	register char *cp;
	if ((cp = getenv("PRINTER")) != NULL) dev = cp;
	if ((cp = getenv("TYPESETTER")) != NULL) dev = cp;
	cmdname = argv[0];
	while (argc > 1 && *argv[1] == '-' && argv[1][1]) {
		switch (argv[1][1]) {
		case 'F':
			devdir = &argv[1][2];
			break;
		case 'T':
		case 'P':
			dev = &argv[1][2];
			break;
		case 'd':
			dbg = 1;
			break;
		case 'D':
			useDline = !useDline;
			break;
		case 'x':
			xbound = atof(&argv[1][2]);
			break;
		case 'y':
			ybound = atof(&argv[1][2]);
			break;
		}
		argc--;
		argv++;
	}

	fileinit();
	setdefaults();
	if (argc <= 1) {
		yyin = stdin;
		getdata(yyin);
	} else
		while (argc-- > 1) {
			filename = *++argv;
			if ((yyin = fopen(filename, "r")) == NULL) {
			    if (filename[0] != '-' || filename[1]) {
				fprintf(stderr, "pic: can't open %s\n", *argv);
				exit(1);
			    } else {
				yyin = stdin;
				filename = "standard input";
			    }
			}
			getdata(yyin);
			fclose(yyin);
		}
	exit(synerr);
}


fileinit()
{
	int fin;
	struct dev device;
	char temp[100];

	sprintf(temp, "%s/dev%s/DESC.out", devdir, dev);
	if ((fin = open(temp, 0)) < 0) {
	    fprintf(stderr, "can't open tables for %s\n", temp);
	    exit(1);
	}
	read(fin, &device, sizeof(struct dev));
	res = device.res;
	DX = device.hor;
	DY = device.vert;
	close(fin);
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
