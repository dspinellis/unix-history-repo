#ifndef lint
static char sccsid[] = "@(#)main.c	3.1 (CWI) 85/07/30";
#endif lint

#include	<stdio.h>
#include	<signal.h>
#include	"pic.h"
#include	"y.tab.h"

obj	**objlist = 0;	/* store the elements here */
int	nobjlist = 0;		/* size of objlist array */
int	nobj	= 0;

Attr	*attr;	/*;attributes stored here as collected */
int	nattrlist = 0;
int	nattr	= 0;	/* number of entries in attr_list */

Text	*text	= 0;	/* text strings stored here as collected */
int	ntextlist = 0;		/* size of text[] array */
int	ntext	= 0;
int	ntext1	= 0;	/* record ntext here on entry to each figure */

float	curx	= 0;
float	cury	= 0;

int	hvmode	= R_DIR;	/* R => join left to right, D => top to bottom, etc. */

int	codegen	= 0;	/* 1=>output for this picture; 0=>no output */

float	deltx	= 6;	/* max x value in output, for scaling */
float	delty	= 6;	/* max y value in output, for scaling */
int	dbg	= 0;
int	lineno	= 0;
char	*filename	= "-";
int	synerr	= 0;
char	*cmdname;

float	xmin	= 30000;	/* min values found in actual data */
float	ymin	= 30000;
float	xmax	= -30000;	/* max */
float	ymax	= -30000;

main(argc, argv)
	char *argv[];
{
	char buf[20];
	extern int fpecatch();

	signal(SIGFPE, fpecatch);
	cmdname = argv[0];
	while (argc > 1 && *argv[1] == '-') {
		switch (argv[1][1]) {
		case 'd':
			dbg = atoi(&argv[1][2]);
			if (dbg == 0)
				dbg = 1;
			break;
		}
		argc--;
		argv++;
	}
	setdefaults();
	objlist = (obj **) grow(objlist, "objlist", nobjlist += 1000, sizeof(obj *));
	text = (Text *) grow(text, "text", ntextlist += 1000, sizeof(Text));
	attr = (Attr *) grow(attr, "attr", nattrlist += 100, sizeof(Attr));

	sprintf(buf, "/%d/", getpid());
	pushsrc(String, buf);
	definition("pid");

	pushsrc(File, curfile = infile);
	if (argc <= 1) {
		curfile->fin = stdin;
		curfile->fname = tostring("-");
		getdata(curfile);
	} else
		while (argc-- > 1) {
			if ((curfile->fin = fopen(*++argv, "r")) == NULL) {
				fprintf(stderr, "%s: can't open %s\n", cmdname, *argv);
				exit(1);
			}
			curfile->fname = tostring(*argv);
			getdata(curfile);
			fclose(curfile->fin);
			free(curfile->fname);
		}
	exit(0);
}

fpecatch()
{
	fatal("floating point exception");
}

char *grow(ptr, name, num, size)	/* make array bigger */
	char *ptr, *name;
	int num, size;
{
	char *p;

	if (ptr == NULL)
		p = malloc(num * size);
	else
		p = realloc(ptr, num * size);
	if (p == NULL)
		fatal("can't grow %s to %d", name, num * size);
	return p;
}

static struct {
	char *name;
	float val;
	short scalable;		/* 1 => adjust when "scale" changes */
} defaults[] ={
	"scale", SCALE, 1,
	"lineht", HT, 1,
	"linewid", HT, 1,
	"moveht", HT, 1,
	"movewid", HT, 1,
	"dashwid", HT10, 1,
	"boxht", HT, 1,
	"boxwid", WID, 1,
	"circlerad", HT2, 1,
	"arcrad", HT2, 1,
	"ellipseht", HT, 1,
	"ellipsewid", WID, 1,
	"arrowht", HT5, 1,
	"arrowwid", HT10, 1,
	"arrowhead", 2, 0,		/* arrowhead style */
	"textht", 0.0, 1,		/* 6 lines/inch is also a useful value */
	"textwid", 0.0, 1,
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

resetvar()	/* reset variables listed */
{
	int i, j;

	if (nattr == 0) {	/* none listed, so do all */
		setdefaults();
		return;
	}
	for (i = 0; i < nattr; i++) {
		for (j = 0; defaults[j].name != NULL; j++)
			if (strcmp(defaults[j].name, attr[i].a_val.p) == 0) {
				setfval(defaults[j].name, defaults[j].val);
				free(attr[i].a_val.p);
				break;
			}
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
			if (defaults[i].scalable)
				setfval(defaults[i].name, defaults[i].val * scale);
	}
}

getdata()
{
	char *p, buf[1000], buf1[100];
	int ln;

	curfile->lineno = 0;
	printf(".lf 1 %s\n", curfile->fname);
	while (fgets(buf, sizeof buf, curfile->fin) != NULL) {
		curfile->lineno++;
		if (*buf == '.' && *(buf+1) == 'P' && *(buf+2) == 'S') {
			for (p = &buf[3]; *p == ' '; p++)
				;
			if (*p++ == '<') {
				Infile svfile;
				svfile = *curfile;
				sscanf(p, "%s", buf1);
				if ((curfile->fin=fopen(buf1, "r")) == NULL)
					fatal("can't open %s", buf1);
				curfile->fname = tostring(buf1);
				getdata();
				fclose(curfile->fin);
				free(curfile->fname);
				*curfile = svfile;
				printf(".lf %d %s\n", curfile->lineno, curfile->fname);
				continue;
			}
			reset();
			yyparse();
			/* yylval.i now contains 'E' or 'F' from .PE or .PF */

			deltx = (xmax - xmin) / getfval("scale");
			delty = (ymax - ymin) / getfval("scale");
			if (buf[3] == ' ') {	/* next things are wid & ht */
				if (sscanf(&buf[4],"%f%f",&deltx,&delty) < 2)
					delty = deltx * (ymax-ymin) / (xmax-xmin);
			}
			dprintf("deltx = %g, delty = %g\n", deltx, delty);
			if (codegen && !synerr) {
				openpl(&buf[3]);	/* puts out .PS, with ht & wid stuck in */
				printf(".lf %d\n", curfile->lineno+1);
				print();	/* assumes \n at end */
				closepl(yylval.i);	/* does the .PE/F */
			}
			printf(".lf %d\n", curfile->lineno+1);
			fflush(stdout);
		} else if (buf[0] == '.' && buf[1] == 'l' && buf[2] == 'f') {
			if (sscanf(buf+3, "%d %s", &ln, buf1) == 2) {
				free(curfile->fname);
				printf(".lf %d %s\n", curfile->lineno = ln, curfile->fname = tostring(buf1));
			} else
				printf(".lf %d\n", curfile->lineno = ln);
		} else
			fputs(buf, stdout);
	}
}

reset()
{
	obj *op;
	int i;
	struct symtab *p;
	extern int nstack;

	for (i = 0; i < nobj; i++) {
		op = objlist[i];
		if (op->o_type == BLOCK)
			freesymtab(op->o_symtab);
		free(objlist[i]);
	}
	nobj = 0;
	nattr = 0;
	for (i = 0; i < ntext; i++)
		if (text[i].t_val)
			free(text[i].t_val);
	ntext = ntext1 = 0;
	codegen = synerr = 0;
	nstack = 0;
	curx = cury = 0;
	hvmode = R_DIR;
	xmin = ymin = 30000;
	xmax = ymax = -30000;
}
