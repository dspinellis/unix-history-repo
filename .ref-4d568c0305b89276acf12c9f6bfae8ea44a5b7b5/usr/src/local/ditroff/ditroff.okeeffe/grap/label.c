#ifndef lint
static char sccsid[] = "@(#)label.c	1.1 (CWI) 85/07/19";
#endif lint
#include <stdio.h>
#include "grap.h"
#include "y.tab.h"

int	pointsize	= 10;	/* assumed pointsize to start */
int	ps_set		= 0;	/* someone has set pointsize explicitly */

double	textht	= 1.0/6.0;	/* 6 lines/inch */
double	textwid = 1;		/* width of text box for vertical */

double	lab_up	= 0.0;		/* extra motion for label */
double	lab_rt	= 0.0;		/* extra motion for label */

labelmove(dir, amt)	/* record direction & motion of position corr */
	int dir;
	double amt;
{
	switch (dir) {
	case UP:	lab_up += amt; break;
	case DOWN:	lab_up -= amt; break;
	case LEFT:	lab_rt -= amt; break;
	case RIGHT:	lab_rt += amt; break;
	}
}

label(label_side, stringlist)	/* stick label on label_side */
	int label_side;
	Attr *stringlist;
{
	int i, m;
	Attr *ap, *nap;

	fprintf(tfd, "\ttextht = %g\n", textht);
	if (label_side == LEFT || label_side == RIGHT) {
		textwid = 0;
		for (ap = stringlist; ap != NULL; ap = ap->next)
			if ((m = strlen(ap->sval)) > textwid)
				textwid = m;
		textwid /= 15;	/* estimate width at 15 chars/inch */
		fprintf(tfd, "\ttextwid = %g\n", textwid);
	}
	fprintf(tfd, "Label:\t%s", slprint(stringlist));
	freeattr(stringlist);
	switch (label_side) {
	case BOT:
	case 0:
		fprintf(tfd, " with .n at Frame.s - (0,2 * textht)");
		break;
	case LEFT:
		fprintf(tfd, " wid textwid with .e at Frame.w - (0.2,0)");
		break;
	case RIGHT:
		fprintf(tfd, " wid textwid with .w at Frame.e + (0.2,0)");
		break;
	case TOP:
		fprintf(tfd, " with .s at Frame.n + (0,2 * textht)");
		break;
	}
	lab_adjust();
	fprintf(tfd, "\n");
	label_side = BOT;
}

lab_adjust()	/* add a string to adjust labels, ticks, etc. */
{
	if (lab_up != 0.0 || lab_rt != 0.0)
		fprintf(tfd, " + (%g,%g)", lab_rt, lab_up);
}

char *sizeit(ap)		/* add \s..\s to ap->sval */
	Attr *ap;
{
	int n;
	static char buf[1000];

	if (!ap->op) {	/* no explicit size command */
		if (ps_set) {
			sprintf(buf, "\\s%d%s\\s0", pointsize, ap->sval);
			return buf;
		} else
			return ap->sval;
	} else if (!ps_set) {	/* explicit size but no global size */
		n = (int) ap->fval;
		switch (ap->op) {
		case ' ':	/* absolute size */
			sprintf(buf, "\\s%d%s\\s0", n, ap->sval);
			break;
		case '+':	/* better be only one digit! */
			sprintf(buf, "\\s+%d%s\\s-%d", n, ap->sval, n);
			break;
		case '-':
			sprintf(buf, "\\s-%d%s\\s+%d", n, ap->sval, n);
			break;
		case '*':
		case '/':
			return ap->sval;	/* ignore for now */
		}
		return buf;
	} else {
		/* explicit size and a global background size */
		n = (int) ap->fval;
		switch (ap->op) {
		case ' ':	/* absolute size */
			sprintf(buf, "\\s%d%s\\s0", n, ap->sval);
			break;
		case '+':
			sprintf(buf, "\\s%d%s\\s0", pointsize+n, ap->sval);
			break;
		case '-':
			sprintf(buf, "\\s%d%s\\s0", pointsize-n, ap->sval);
			break;
		case '*':
		case '/':
			return ap->sval;	/* ignore for now */
		}
		return buf;
	}
}
