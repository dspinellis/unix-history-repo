#ifndef lint
static char sccsid[] = "@(#)print.c	1.1 (CWI) 85/07/19";
#endif lint
#include <stdio.h>
#include <ctype.h>
#include "grap.h"
#include "y.tab.h"

double	margin	= MARGIN;	/* extra space around edges */
extern	double	frame_ht, frame_wid, ticklen;

char	graphname[50] = "Graph";
char	graphpos[200] = "";

print()	/* arrange final output */
{
	FILE *fd;
	Obj *p, *dfp, *setauto();
	int c;
	static int firstG1 = 0;
	double dx, dy, xfac, yfac;
	extern double pow();

	if (tfd != stdout) {
		fclose(tfd);	/* end the temp file */
		tfd = stdout;
	}

	if ((p=lookup("margin",0)) != NULL)
		margin = p->fval;
	if (frame_ht < 0)	/* wasn't set explicitly, so use default */
		frame_ht = getvar(lookup("frameht", 0));
	if (frame_wid < 0)
		frame_wid = getvar(lookup("framewid", 0));
	dfp = NULL;
	for (p = objlist; p; p = p->next) {
		dprintf("print: name = <%s>, type = %d\n", p->name, p->type);
		if (p->type == NAME) {
			Point pt, pt1;	
			pt = p->pt;
			pt1 = p->pt1;
			fprintf(tfd, "\t# %s %g .. %g, %g .. %g\n",
				p->name, pt.x, pt1.x, pt.y, pt1.y);
			if (p->log & XFLAG) {
				if (pt.x <= 0.0)
					fatal("can't take log of x coord %g", pt.x);
				logit(pt.x);
				logit(pt1.x);
			}
			if (p->log & YFLAG) {
				if (pt.y <= 0.0)
					fatal("can't take log of y coord %g", pt.y);
				logit(pt.y);
				logit(pt1.y);
			}
			if (!(p->coord & XFLAG)) {
				dx = pt1.x - pt.x;
				pt.x -= margin * dx;
				pt1.x += margin * dx;
			}
			if (!(p->coord & YFLAG)) {
				dy = pt1.y - pt.y;
				pt.y -= margin * dy;
				pt1.y += margin * dy;
			}
			if (autoticks && strcmp(p->name, dflt_coord) == 0) {
				p->pt = pt;
				p->pt1 = pt1;
				if (p->log & XFLAG) {
					p->pt.x = pow(10.0, pt.x);
					p->pt1.x = pow(10.0, pt1.x);
				}
				if (p->log & YFLAG) {
					p->pt.y = pow(10.0, pt.y);
					p->pt1.y = pow(10.0, pt1.y);
				}
				dfp = setauto(p);
			}		
			dx = pt1.x - pt.x;
			dy = pt1.y - pt.y;
			xfac = dx > 0 ? frame_wid/dx : 0;
			yfac = dy > 0 ? frame_ht/dy : 0;

			fprintf(tfd, "define xy_%s @ ", p->name);
			fprintf(tfd, "\t(($1)-(%g))*%g", pt.x, xfac);
			fprintf(tfd, ", (($2)-(%g))*%g @\n", pt.y, yfac);
			fprintf(tfd, "define x_%s @ ", p->name);
			fprintf(tfd, "\t(($1)-(%g))*%g @\n", pt.x, xfac);
			fprintf(tfd, "define y_%s @ ", p->name);
			fprintf(tfd, "\t(($1)-(%g))*%g @\n", pt.y, yfac);
		}
	}
	if (codegen)
		frame();
	if (codegen && autoticks && dfp)
		do_autoticks(dfp);

	if ((fd = fopen(tempfile, "r")) != NULL) {
		while ((c = getc(fd)) != EOF)
			putc(c, tfd);
		fclose(fd);
	}
	tfd = NULL;
}

endstat()	/* clean up after each statement */
{
	extern int just, sizeop, tick_dir;
	extern double sizexpr, lab_up, lab_rt;

	just = sizeop = 0;
	lab_up = lab_rt = 0.0;
	sizexpr = 0.0;
	nnum = 0;
	ntick = 0;
	tside = 0;
	tick_dir = OUT;
	ticklen = TICKLEN;
}

graph(s)	/* graph statement */
	char *s;
{
	char *p, *os;
	int c;

	if (codegen) {
		fprintf(stdout, "%s: [\n", graphname);
		print();	/* pump out previous graph */
		fprintf(stdout, "\n] %s\n", graphpos);
		reset();
	}
	if (s) {
		dprintf("into graph with <%s>\n", s);
		os = s;
		while ((c = *s) == ' ' || c == '\t')
			s++;
		if (c == '\0')
			yyerror("no name on graph statement");
		if (!isupper(s[0]))
			yyerror("graph name %s must be capitalized", s);
		for (p=graphname; (c = *s) != ' ' && c != '\t' && c != '\0'; )
			*p++ = *s++;
		*p = '\0';
		strcpy(graphpos, s);
		dprintf("graphname = <%s>, graphpos = <%s>\n", graphname, graphpos);
		free(os);
	}
}

setup()		/* done at each .G1 */
{
	static int firstG1 = 0;

	reset();
	frame_ht = frame_wid = -1;	/* reset in frame() */
	ticklen = getvar(lookup("ticklen", 0));
	if (firstG1++ == 0)
		do_first();
	codegen = synerr = 0;
	strcpy(graphname, "Graph");
	strcpy(graphpos, "");
}

do_first()	/* done at first .G1:  definitions, etc. */
{
	extern int lib;
	extern char *lib_defines;
	static char buf[50], buf1[50];	/* static because pbstr uses them */

	sprintf(buf, "define pid /%d/\n", getpid());
	pbstr(buf);	
	if (lib != 0) {
		if (access(lib_defines, 4) == 0) {
			sprintf(buf1, "copy \"%s\"\n", lib_defines);
			pbstr(buf1);
		} else {
			fprintf(stderr, "grap warning: can't open %s\n", lib_defines);
		}
	}
}

reset()		/* done at each "graph ..." statement */
{
	Obj *p, *np, *deflist;

	curr_coord = dflt_coord;
	ncoord = auto_x = 0;
	autoticks = LEFT|BOT;
	margin = MARGIN;
	deflist = NULL;
	for (p = objlist; p; p = np) {
		np = p->next;
		if (p->type == DEFNAME || p->type == VARNAME) {
			p->next = deflist;
			deflist = p;
		} else {
			free(p->name);
			freeattr(p->attr);
			free(p);
		}
	}
	objlist = deflist;
	if (tfd != stdout && (tfd = fopen(tempfile, "w")) == NULL) {
		fprintf(stderr, "grap: can't open %s\n", tempfile);
		exit(1);
	}
}
